package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataSyncResponse;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IHblSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.utils.CommonUtils.convertToClass;

@Component
@Slf4j
public class HblSync implements IHblSync {

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    RestTemplate restTemplate;

    @Autowired
    ModelMapper modelMapper;
    @Autowired
    private IV1Service v1Service;

    @Autowired
    private EmailServiceUtility emailServiceUtility;
    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private ISyncService syncService;
    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Value("${v1service.url.base}${v1service.url.hblSync}")
    private String HBL_V1_SYNC_URL;

    @Override
    @Async
    public ResponseEntity<?> sync(Hbl hbl) {
        HblRequestV2 hblRequest = new HblRequestV2();
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(hbl.getShipmentId());
        hblRequest = convertEntityToDto(hbl);
        hblRequest.setShipmentGuid(shipmentDetails.get().getGuid());
        if(hblRequest.getContainers() != null && hblRequest.getContainers().size() > 0) {
            for (HblContainerRequestV2 hblContainerRequestV2: hblRequest.getContainers()) {
                if(hblContainerRequestV2.getHazardous() == null)
                    hblContainerRequestV2.setHazardous(0);
            }
        }
        String finalHbl = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(hblRequest).module(SyncingConstants.HBL).build());
//        CompletableFuture.runAsync(commonUtils.withMdc(() -> callSync(finalHbl, hbl.getId(), hbl.getGuid())), commonUtils.syncExecutorService);
        syncService.callSync(finalHbl, hbl.getId(), hbl.getGuid(), "HBL");
        return ResponseHelper.buildSuccessResponse(modelMapper.map(finalHbl, HblDataRequestV2.class));
    }

//    public void callSync(String finalHbl, Long id, UUID guid) {
//        retryTemplate.execute(ctx -> {
//            log.info("Current retry : {}", ctx.getRetryCount());
//            if (ctx.getLastThrowable() != null) {
//                log.error("V1 error -> {}", ctx.getLastThrowable().getMessage());
//            }
//            V1DataSyncResponse response_ = v1Service.v1DataSync(finalHbl);
//            if (!response_.getIsSuccess()) {
//                try {
//                    emailServiceUtility.sendEmailForSyncEntity(String.valueOf(id),
//                            String.valueOf(guid),
//                            "HBL", response_.getError().toString());
//                } catch (Exception ex) {
//                    log.error("Not able to send email for sync failure for HBL: " + ex.getMessage());
//                }
//            }
//            return ResponseHelper.buildSuccessResponse(response_);
//        });
//    }

    private HblRequestV2 convertEntityToDto(Hbl hbl) {
        HblRequestV2 response = jsonHelper.convertValue(hbl.getHblData(), HblRequestV2.class);
        response.setCargoes(convertToList(hbl.getHblCargo(), HblCargoRequestV2.class));
        response.setContainers(convertToList(hbl.getHblContainer(), HblContainerRequestV2.class));
        response.setNotifyParties(convertToList(hbl.getHblNotifyParty(), HblPartyRequestV2.class));
        return response;
    }

    private <T,P> List<P> convertToList(final List<T> lst, Class<P> clazz) {
        if(lst == null)
            return null;
        return  lst.stream()
                .map(item -> convertToClass(item, clazz))
                .collect(Collectors.toList());
    }
}
