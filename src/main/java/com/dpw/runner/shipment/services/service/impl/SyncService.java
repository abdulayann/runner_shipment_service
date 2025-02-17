package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataSyncResponse;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.SyncKafkaDto;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
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

import java.util.List;

@Service
@Slf4j
public class SyncService implements ISyncService {

    static Integer maxAttempts = 3;
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
    private final RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(maxAttempts)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Override
    public void callSync(String json, String id, String guid, String entity, HttpHeaders headers) throws RunnerException {
        retryTemplate.execute(ctx -> {
            log.info("Current retry : {}", ctx.getRetryCount());
            if (ctx.getLastThrowable() != null) {
                log.error("V1 error -> {}", ctx.getLastThrowable().getMessage());
            }
            V1DataSyncResponse response_ = v1Service.v1DataSync(json, headers);
            if (!response_.getIsSuccess()) {
                try {
                    if (ctx.getRetryCount() == maxAttempts - 1)
                        emailServiceUtility.sendEmailForSyncEntity(id, guid,
                                entity, response_.getError().toString());
                } catch (Exception ex) {
                    log.error("Not able to send email for sync: {} failure for: {}", entity, ex.getMessage());
                }
                throw new RunnerException((String) response_.error);
            }
            return ResponseHelper.buildSuccessResponse(response_);
        });
    }

    @Override
    public void callSyncAsync(String json, String id, String guid, String entity, HttpHeaders headers) throws RunnerException {
        callSync(json, id, guid, entity, headers);
    }

    @Override
    @Async
    public void callSyncAsync(List<String> json, List<String> id, List<String> guid, List<String> entity, HttpHeaders headers) throws RunnerException {
        for (int i = 0; i < json.size(); i++) {
            callSync(json.get(i), id.get(i), guid.get(i), entity.get(i), headers);
        }
    }

    @Override
    public void pushToKafka(String json, String id, String guid, String entity, String transactionId, Integer tenantId, String username, String updatedUsername) {
        try {
            SyncKafkaDto request = SyncKafkaDto.builder()
                    .data(json).id(id).guid(guid).entity(entity).tenantId(tenantId)
                    .userName(username).transactionId(transactionId).updateUsername(updatedUsername).build();
            producer.produceToKafka(jsonHelper.convertToJson(request), senderQueue, transactionId);
        } catch (Exception ex) {
            log.error("Exception occurred during event: {} for entity: {} with exception: {} with data: {}", LoggerEvent.KAFKA_PUSH_FOR_V1_SYNC, entity, ex.getLocalizedMessage(), json);
        }
    }

    @Override
    public void pushToKafka(String json, String id, String guid, String entity, String transactionId) {
        pushToKafka(json, id, guid, entity, transactionId, TenantContext.getCurrentTenant(), UserContext.getUser().getUsername(), null);
    }
}