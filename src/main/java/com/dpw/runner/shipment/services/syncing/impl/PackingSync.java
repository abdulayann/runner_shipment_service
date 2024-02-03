package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataSyncResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.BulkPackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.V1DataSyncRequest;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingSync;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import com.dpw.runner.shipment.services.utils.StringUtility;
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

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Component
@Slf4j
@EnableAsync
public class PackingSync implements IPackingSync {

    @Autowired
    ModelMapper modelMapper;
    @Autowired
    JsonHelper jsonHelper;
    @Autowired
    RestTemplate restTemplate;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    private IContainerDao containerDao;
    @Autowired
    private SyncEntityConversionService syncEntityConversionService;

    @Autowired
    private EmailServiceUtility emailServiceUtility;
    @Autowired
    private ISyncService syncService;

    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Value("${v1service.url.base}${v1service.url.bulkPackingSync}")
    private String BULK_PACKING_SYNC_URL;

    @Override
    @Async("asyncExecutor")
    public ResponseEntity<?> sync(List<Packing> packings, Long consolidationId, Long shipmentId) {
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
        return ResponseHelper.buildSuccessResponse(true);
    }
}
