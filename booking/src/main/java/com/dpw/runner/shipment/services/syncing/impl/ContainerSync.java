package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.syncing.Entity.BulkContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.ContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.V1DataSyncRequest;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IContainerSync;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.UUID;

@Component
@Slf4j
@EnableAsync
public class ContainerSync implements IContainerSync {

    @Autowired
    JsonHelper jsonHelper;

    @Autowired
    private SyncEntityConversionService syncEntityConversionService;
    @Autowired
    private ISyncService syncService;

    @Override
    @Async("asyncExecutor")
    public void sync(List<Containers> containers, Long consolidationId, Long shipmentId) {
        if (!Boolean.TRUE.equals(SyncingContext.getContext()))
            return;
        List<ContainerRequestV2> requestV2List = syncEntityConversionService.containersV2ToV1(containers);
        BulkContainerRequestV2 containerRequestV2 = BulkContainerRequestV2.builder()
                .bulkContainers(requestV2List).ConsolidationId(consolidationId).ShipmentId(shipmentId).build();
        String finalCs = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(containerRequestV2).module(SyncingConstants.BULK_CONTAINERS).build());
        syncService.pushToKafka(finalCs, containers.stream().map(BaseEntity::getId).toList().toString(), containers.stream().map(BaseEntity::getGuid).toList().toString(), "Bulk Containers", UUID.randomUUID().toString());
        ResponseHelper.buildSuccessResponse(true);
    }

}
