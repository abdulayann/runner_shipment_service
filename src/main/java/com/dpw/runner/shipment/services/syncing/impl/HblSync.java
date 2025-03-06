package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IHblSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class HblSync implements IHblSync {

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    ModelMapper modelMapper;

    @Autowired
    private ISyncService syncService;

    @Autowired
    private CommonUtils commonUtils;

    @Override
    public ResponseEntity<IRunnerResponse> sync(Hbl hbl, String transactionId) {
        if (!Boolean.TRUE.equals(SyncingContext.getContext()))
            return ResponseHelper.buildSuccessResponse();

        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(hbl.getShipmentId());
        if (shipmentDetails.isEmpty()) {
            return ResponseHelper.buildSuccessResponse();
        }
        HblRequestV2 hblRequest = convertEntityToDto(hbl);
        hblRequest.setShipmentGuid(shipmentDetails.get().getGuid());
        if(hblRequest.getContainers() != null && !hblRequest.getContainers().isEmpty()) {
            for (HblContainerRequestV2 hblContainerRequestV2: hblRequest.getContainers()) {
                if(hblContainerRequestV2.getHazardous() == null)
                    hblContainerRequestV2.setHazardous(0);
            }
        }
        String finalHbl = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(hblRequest).module(SyncingConstants.HBL).build());
        syncService.pushToKafka(finalHbl, StringUtility.convertToString(hbl.getId()), StringUtility.convertToString(hbl.getGuid()), "HBL", transactionId);
        return ResponseHelper.buildSuccessResponse(modelMapper.map(finalHbl, HblDataRequestV2.class));
    }

    private HblRequestV2 convertEntityToDto(Hbl hbl) {
        HblRequestV2 response = jsonHelper.convertValue(hbl.getHblData(), HblRequestV2.class);
        response.setGuid(hbl.getGuid());
        response.setCargoes(convertToList(hbl.getHblCargo(), HblCargoRequestV2.class));
        response.setContainers(convertToList(hbl.getHblContainer(), HblContainerRequestV2.class));
        response.setNotifyParties(convertToList(hbl.getHblNotifyParty(), HblPartyRequestV2.class));
        return response;
    }

    private <T,P> List<P> convertToList(final List<T> lst, Class<P> clazz) {
        if(lst == null)
            return null;
        return  lst.stream()
                .map(item -> commonUtils.convertToClass(item, clazz))
                .toList();
    }
}
