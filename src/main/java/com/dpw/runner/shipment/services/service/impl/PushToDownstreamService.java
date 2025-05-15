package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.LogHistoryRequest;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.service.interfaces.ILogsHistoryService;
import com.dpw.runner.shipment.services.service.interfaces.IPushToDownstreamService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.BookingIntegrationsUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

@Service
@Slf4j
public class PushToDownstreamService implements IPushToDownstreamService {

    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    private DependentServiceHelper dependentServiceHelper;
    @Autowired
    private BookingIntegrationsUtility bookingIntegrationsUtility;
    @Autowired
    private ILogsHistoryService logsHistoryService;
    @Autowired
    private JsonHelper jsonHelper;

    @Transactional
    @Override
    public void process(PushToDownstreamEventDto message) {
        // Setting Service account auth token for v1 call
        v1Service.setAuthContext();
        // Process the messages based on parent entity - "SHIPMENT", "CONSOLIDATION"
        if (Objects.equals(message.getParentEntityName(), Constants.SHIPMENT)) {
            this.pushShipmentData(message.getParentEntityId(), message.getMeta().getIsCreate(), message.getMeta().getIsAutoSellRequired());

            // Processing the Dependent Triggers for given parent trigger
            if(message.getTriggers() != null) {
                message.getTriggers().forEach(trigger -> {

                    // Dependent Triggers for Shipment
                    if (Objects.equals(trigger.getEntityName(), Constants.SHIPMENT)) {
                        this.pushShipmentData(trigger.getEntityId(), false, false);
                    }
                });
            }

        }

    }

    private void pushShipmentData(Long entityId, boolean isCreate, boolean isAutoSellRequired) {
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentByIdWithQuery(entityId);
        if(shipmentDetails.isEmpty()) {
            log.info("Shipment {} not found.", entityId);
            return;
        }
        // Setting tenant id of shipment to context for V1TenantSettings
        TenantContext.setCurrentTenant(shipmentDetails.get().getTenantId());
        // Pushing Data to dependent services
        dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails.get(), isCreate, isAutoSellRequired, null);
        // Update to Platform
        bookingIntegrationsUtility.updateBookingInPlatform(shipmentDetails.get());
        // Create Entry to log History
        String entityPayload = jsonHelper.convertToJson(shipmentDetails.get());
        this.createLogHistoryForShipment(entityPayload, shipmentDetails.get().getId(), shipmentDetails.get().getGuid());
    }

    public void createLogHistoryForShipment(String entityPayload, Long id, UUID guid) {
        try {
            logsHistoryService.createLogHistory(LogHistoryRequest.builder().entityId(id)
                    .entityType(Constants.SHIPMENT).entityGuid(guid).entityPayload(entityPayload).build());
        } catch (Exception ex) {
            log.error("Error while creating LogsHistory for Shipment: " + ex.getMessage());
        }
    }



}
