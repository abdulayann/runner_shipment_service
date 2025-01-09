package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.dto.EventsRequestDTO;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.Generated;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;
import java.util.UUID;

@Service
@Slf4j
@Generated
public class EventConsumer {

  private final JsonHelper jsonHelper;
  private final IEventDao eventDao;
  private final IShipmentDao shipmentDao;
  private final IConsolidationDetailsDao consolidationDetailsDao;
  private final CommonUtils commonUtils;
  private IV1Service iv1Service;

  @Autowired
  public EventConsumer(
      JsonHelper jsonHelper,
      IEventDao eventDao,
      IShipmentDao shipmentDao,
      IConsolidationDetailsDao consolidationDetailsDao,
      CommonUtils commonUtils, IV1Service iv1Service) {
    this.jsonHelper = jsonHelper;
    this.eventDao = eventDao;
    this.shipmentDao = shipmentDao;
    this.consolidationDetailsDao = consolidationDetailsDao;
    this.commonUtils = commonUtils;
    this.iv1Service = iv1Service;
  }

  // @KafkaListener(topics = {"#{'${wfm.event.kafka.queue}'}"}, groupId = "#{'${wfm.event.kafka.subs}'}")
  public void consume(String message) {
    log.info("Received message from WFM Event Kafka queue: {}", message);

    try {
      // Parse and validate the message
      EventsRequestDTO eventsRequestDTO = jsonHelper.readFromJson(message, EventsRequestDTO.class);
      if (eventsRequestDTO == null) {
        log.warn("Parsed EventsRequestDTO is null, skipping processing");
        return;
      }

      // Convert DTO to entity
      Events events = jsonHelper.convertValue(eventsRequestDTO, Events.class);
      iv1Service.setAuthContext();
      // Determine entity type and set context
      if (eventsRequestDTO.getShipmentGuid() != null) {
        handleShipmentEntity(eventsRequestDTO, events);
      } else if (eventsRequestDTO.getConsolidationGuid() != null) {
        handleConsolidationEntity(eventsRequestDTO, events);
      } else {
        log.warn("Both shipmentGuid and consolidationGuid are null. Unable to process the event.");
        return;
      }

      // Update and save event
      eventDao.updateEventDetails(events);
      commonUtils.updateEventWithMasterData(List.of(events));
      eventDao.save(events);

      iv1Service.clearAuthContext();
      TenantContext.removeTenant();
      log.info("Event successfully saved from WFM Event message");
    } catch (Exception ex) {
      log.error("{} | Exception occurred while processing event message: {} with exception: {}", LoggerEvent.WFM_EVENTS_KAFKA_PULL, message, ex.getMessage(), ex);
    }
  }

  private void handleShipmentEntity(EventsRequestDTO eventsRequestDTO, Events events) {
    UUID shipmentGuid = eventsRequestDTO.getShipmentGuid();
    List<ShipmentDetails> shipmentDetailsList = shipmentDao.findByGuids(List.of(shipmentGuid));

    if (shipmentDetailsList.isEmpty()) {
      log.warn("No shipment details found for shipmentGuid: {}", shipmentGuid);
      return;
    }

    ShipmentDetails shipmentDetails = shipmentDetailsList.get(0);
    events.setEntityId(shipmentDetails.getId());
    events.setEntityType(Constants.SHIPMENT);
    TenantContext.setCurrentTenant(shipmentDetails.getTenantId());
  }

  private void handleConsolidationEntity(EventsRequestDTO eventsRequestDTO, Events events) {
    UUID consolidationGuid = eventsRequestDTO.getConsolidationGuid();
    List<ConsolidationDetails> consolidationDetailsList = consolidationDetailsDao.findConsolidationsByGuids(Set.of(consolidationGuid));

    if (consolidationDetailsList.isEmpty()) {
      log.warn("No consolidation details found for consolidationGuid: {}", consolidationGuid);
      return;
    }

    ConsolidationDetails consolidationDetails = consolidationDetailsList.get(0);
    events.setEntityId(consolidationDetails.getId());
    events.setEntityType(Constants.CONSOLIDATION);
    TenantContext.setCurrentTenant(consolidationDetails.getTenantId());
  }
}
