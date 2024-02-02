package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.Kafka.Dto.SyncKafkaDto;
import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataSyncResponse;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@Slf4j
public class SyncService implements ISyncService {

    @Autowired
    private IV1Service v1Service;
    @Autowired
    private EmailServiceUtility emailServiceUtility;
    @Autowired
    private KafkaProducer producer;
    @Value("${data.sync.kafka.queue}")
    private String senderQueue;
    @Autowired
    private JsonHelper jsonHelper;
    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Override
    public void callSync(String json, String id, String guid, String entity, HttpHeaders headers) {
        retryTemplate.execute(ctx -> {
            log.info("Current retry : {}", ctx.getRetryCount());
            if (ctx.getLastThrowable() != null) {
                log.error("V1 error -> {}", ctx.getLastThrowable().getMessage());
            }
            V1DataSyncResponse response_ = v1Service.v1DataSync(json, headers);
            if (!response_.getIsSuccess()) {
                try {
                    emailServiceUtility.sendEmailForSyncEntity(id, guid,
                            entity, response_.getError().toString());
                } catch (Exception ex) {
                    log.error("Not able to send email for sync: {} failure for: {}", entity, ex.getMessage());
                }
            }
            return ResponseHelper.buildSuccessResponse(response_);
        });
    }

    @Override
    @Async
    public void pushToKafka(String json, String id, String guid, String entity, String transactionId) {
        try {
            SyncKafkaDto request = SyncKafkaDto.builder()
                    .data(json).id(id).guid(guid).entity(entity).tenantId(TenantContext.getCurrentTenant())
                    .userName(UserContext.getUser().getUsername()).transactionId(transactionId).build();
            producer.produceToKafka(jsonHelper.convertToJson(request), senderQueue, transactionId);
        } catch (Exception ex) {
            log.error("Exception occurred during event: {} for entity: {} with exception: {} with data: {}", LoggerEvent.KAFKA_PUSH_FOR_V1_SYNC, entity, ex.getLocalizedMessage(), json);
        }
    }
}