package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.syncing.Entity.BulkContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.ContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IContainerSync;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;
import java.util.List;

@Component
@Slf4j
@EnableAsync
public class ContainerSync implements IContainerSync {

    @Autowired
    ModelMapper modelMapper;
    @Autowired
    JsonHelper jsonHelper;
    @Autowired
    RestTemplate restTemplate;

    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Value("${v1service.url.base}${v1service.url.bulkContainerSync}")
    private String BULK_CONTAINER_SYNC_URL;

    @Override
    @Async("asyncExecutor")
    public ResponseEntity<?> sync(List<Containers> containers, Long consolidationId, Long shipmentId) {
        List<ContainerRequestV2> requestV2List = new ArrayList<>();
        for (var container : containers) {
            requestV2List.add(modelMapper.map(container, ContainerRequestV2.class));
        }
        BulkContainerRequestV2 containerRequestV2 = BulkContainerRequestV2.builder()
                .bulkContainers(requestV2List).ConsolidationId(consolidationId).ShipmentId(shipmentId).build();
        String finalCs = jsonHelper.convertToJson(containerRequestV2);
        var resp = retryTemplate.execute(ctx -> {
            log.info("Current retry : {}", ctx.getRetryCount());
            HttpEntity<V1DataResponse> entity = new HttpEntity(finalCs, V1AuthHelper.getHeaders());
            var response = this.restTemplate.postForEntity(this.BULK_CONTAINER_SYNC_URL, entity, V1DataResponse.class, new Object[0]);
            return response;
        });
        return ResponseHelper.buildSuccessResponse(resp);
    }

}
