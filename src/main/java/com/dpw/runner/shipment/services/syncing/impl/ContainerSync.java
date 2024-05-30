package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.dto.v1.response.V1DataSyncResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.BulkContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.ContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.V1DataSyncRequest;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IContainerSync;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.UUID;

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

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private EmailServiceUtility emailServiceUtility;

    @Autowired
    private SyncEntityConversionService syncEntityConversionService;
    @Autowired
    private ISyncService syncService;

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
        List<ContainerRequestV2> requestV2List = syncEntityConversionService.containersV2ToV1(containers);
        BulkContainerRequestV2 containerRequestV2 = BulkContainerRequestV2.builder()
                .bulkContainers(requestV2List).ConsolidationId(consolidationId).ShipmentId(shipmentId).build();
        String finalCs = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(containerRequestV2).module(SyncingConstants.BULK_CONTAINERS).build());
        syncService.pushToKafka(finalCs, containers.stream().map(BaseEntity::getId).toList().toString(), containers.stream().map(BaseEntity::getGuid).toList().toString(), "Bulk Containers", UUID.randomUUID().toString());
        return ResponseHelper.buildSuccessResponse(true);
    }

}
