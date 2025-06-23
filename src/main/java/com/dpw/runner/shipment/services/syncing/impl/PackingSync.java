package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.syncing.Entity.BulkPackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.V1DataSyncRequest;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingSync;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Component
@Slf4j
@EnableAsync
public class PackingSync implements IPackingSync {

    @Autowired
    JsonHelper jsonHelper;
    @Autowired
    private IContainerDao containerDao;
    @Autowired
    private SyncEntityConversionService syncEntityConversionService;

    @Autowired
    private ISyncService syncService;


    @Override
    @Async("asyncExecutor")
    public void sync(List<Packing> packings, Long consolidationId, Long shipmentId) {
        if (!Boolean.TRUE.equals(SyncingContext.getContext()))
            return;
        List<PackingRequestV2> requestV2List = null;
        List<Containers> containersList = new ArrayList<>();
        if(shipmentId != null) {
            containersList = containerDao.findByShipmentId(shipmentId);
        }
        requestV2List = syncEntityConversionService.packingsV2ToV1(packings, containersList, null, null);
        BulkPackingRequestV2 packingRequestV2 = BulkPackingRequestV2.builder()
                .bulkPacking(requestV2List).ConsolidationId(consolidationId).ShipmentId(shipmentId).build();
        String finalCs = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(packingRequestV2).module(SyncingConstants.BULK_PACKAGES).build());
        syncService.pushToKafka(finalCs, packings.stream().map(BaseEntity::getId).toList().toString(), packings.stream().map(BaseEntity::getId).toList().toString(), SyncingConstants.BULK_PACKAGES, UUID.randomUUID().toString());
        ResponseHelper.buildSuccessResponse(true);
    }
}
