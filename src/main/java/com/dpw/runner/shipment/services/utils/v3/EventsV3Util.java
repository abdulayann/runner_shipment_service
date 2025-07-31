package com.dpw.runner.shipment.services.utils.v3;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.entity.enums.DateBehaviorType.ACTUAL;
import static com.dpw.runner.shipment.services.entity.enums.DateBehaviorType.ESTIMATED;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class EventsV3Util {

    private IEventDao eventDao;
    private CommonUtils commonUtils;

    @Autowired
    public EventsV3Util(IEventDao eventDao,
                        CommonUtils commonUtils) {
        this.eventDao = eventDao;
        this.commonUtils = commonUtils;
    }

    public List<Events> createOrUpdateEvents(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> updatedEvents, Boolean isNewShipment) {
        List<Events> newUpdatedEvents = (updatedEvents != null) ? new ArrayList<>(updatedEvents) : new ArrayList<>();

        if (Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEventsRevampEnabled())) {
            // we need db events now instead of the eventslist in shipment
            newUpdatedEvents = Optional.ofNullable(oldEntity).map(ShipmentDetails::getEventsList).orElse(new ArrayList<>());
        }

        // Update the direction if blank
        newUpdatedEvents.forEach(events -> events.setDirection(events.getDirection() == null ?
                shipmentDetails.getDirection() : events.getDirection()));

        createUpdateEvent(shipmentDetails, oldEntity, newUpdatedEvents, isNewShipment);

        // Update event fields for runner events generated
        // linking specific events to consol and populating other fields
        eventDao.updateFieldsForShipmentGeneratedEvents(newUpdatedEvents, shipmentDetails);

        return newUpdatedEvents;
    }

    private void createUpdateEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment) {
        commonUtils.removeDuplicateTrackingEvents(events);
        Map<String, List<Events>> cargoesRunnerDbEvents = groupCargoesRunnerEventsByCode(events);
        oldEntity = Optional.ofNullable(oldEntity).orElse(new ShipmentDetails());
        oldEntity.setAdditionalDetails(Optional.ofNullable(oldEntity.getAdditionalDetails()).orElse(new AdditionalDetails()));

        processLclOrFclOrAirEvents(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

        processLclOrAirEvents(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

        processEMCREvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

        processECCCEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

        processBLRSEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

        processFNMUEvent(shipmentDetails, oldEntity, isNewShipment, cargoesRunnerDbEvents);

        processCOODEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

    }

    private Map<String, List<Events>> groupCargoesRunnerEventsByCode(List<Events> events) {
        Map<String, List<Events>> eventMap = new HashMap<>();

        for (Events event : events) {
            String key = event.getEventCode();
            if(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER.equalsIgnoreCase(event.getSource())){
                if (eventMap.containsKey(key)) {
                    // Append the event to the existing list
                    eventMap.get(key).add(event);
                } else {
                    // Create a new list and add the event
                    List<Events> eventList = new ArrayList<>();
                    eventList.add(event);
                    eventMap.put(key, eventList);
                }
            }
        }
        return eventMap;
    }

    private void processLclOrFclOrAirEvents(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isLclOrFclOrAir(shipmentDetails)) {

            processBOCOEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processCUREEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processDOTPEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processPRDEEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);

            processSEPUEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);
        }
    }

    private void processBOCOEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isImportTransferredScenario(shipmentDetails)) return;
        if (isEventChanged(shipmentDetails.getBookingNumber(), oldEntity.getBookingNumber(), isNewShipment) && (Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP) || Objects.equals(shipmentDetails.getDirection(), DIRECTION_IMP) || Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_CTS))) {
            boolean shouldCreateBOCO = true;
            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.BOCO))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.BOCO);
                for (Events event : dbEvents) {
                    if (Objects.equals(shipmentDetails.getBookingNumber(), event.getContainerNumber())) {
                        event.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
                        eventDao.updateUserFieldsInEvent(event, true);
                        shouldCreateBOCO = false;
                    }
                }
            }
            createBOCOEvent(shipmentDetails, events, shouldCreateBOCO);
        }
    }

    private void createBOCOEvent(ShipmentDetails shipmentDetails, List<Events> events, boolean shouldCreateBOCO) {
        if(Boolean.TRUE.equals(shouldCreateBOCO)) {
            Events bocoEvent = initializeAutomatedEvents(shipmentDetails, EventConstants.BOCO, commonUtils.getUserZoneTime(LocalDateTime.now()), null);
            if (!CommonUtils.isStringNullOrEmpty(shipmentDetails.getBookingNumber()))
                bocoEvent.setContainerNumber(shipmentDetails.getBookingNumber());
            events.add(bocoEvent);
        }
    }


    private boolean isImportTransferredScenario(ShipmentDetails details) {
        return Objects.equals(details.getDirection(), Constants.DIRECTION_IMP) && details.getSourceGuid() != null &&
                !Objects.equals(details.getGuid(), details.getSourceGuid());
    }


    private void processCUREEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment,
                                  Map<String, List<Events>> cargoesRunnerDbEvents) {

        boolean isActualChanged = isEventChanged(shipmentDetails.getBrokerageAtDestinationDate(),
                oldEntity.getBrokerageAtOriginDate(), isNewShipment);
        boolean isEstimatedChanged = isEventChanged(shipmentDetails.getEstimatedBrokerageAtDestinationDate(),
                oldEntity.getEstimatedBrokerageAtOriginDate(), isNewShipment);

        if ((isActualChanged || isEstimatedChanged) && Constants.DIRECTION_IMP.equalsIgnoreCase(shipmentDetails.getDirection())) {
            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.CURE))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.CURE);
                setActualAndEstimatedEventTime(shipmentDetails.getBrokerageAtDestinationDate(),
                        shipmentDetails.getEstimatedBrokerageAtDestinationDate(), dbEvents, isActualChanged, isEstimatedChanged);
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.CURE,
                        Objects.nonNull(shipmentDetails.getBrokerageAtDestinationDate()) ?
                                commonUtils.getUserZoneTime(shipmentDetails.getBrokerageAtDestinationDate()) : null,
                        Objects.nonNull(shipmentDetails.getEstimatedBrokerageAtDestinationDate()) ?
                                commonUtils.getUserZoneTime(shipmentDetails.getEstimatedBrokerageAtDestinationDate()) : null));
            }
        }
    }

    private void processDOTPEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventBooleanChanged(shipmentDetails.getAdditionalDetails().getDocTurnedOverToCustomer(),
                        oldEntity.getAdditionalDetails().getDocTurnedOverToCustomer(), isNewShipment)) {

            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.DOTP))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.DOTP);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.DOTP,
                        commonUtils.getUserZoneTime(LocalDateTime.now()), null));
            }
        }
    }

    private void processPRDEEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventChanged(shipmentDetails.getAdditionalDetails().getProofOfDeliveryDate(),
                        oldEntity.getAdditionalDetails().getProofOfDeliveryDate(), isNewShipment)) {
            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.PRDE))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.PRDE);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getProofOfDeliveryDate()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.PRDE,
                        commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getProofOfDeliveryDate()), null));
            }
        }
    }

    private void processSEPUEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventBooleanChanged(shipmentDetails.getAdditionalDetails().getPickupByConsigneeCompleted(),
                        oldEntity.getAdditionalDetails().getPickupByConsigneeCompleted(), isNewShipment)) {

            if(ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.SEPU))){
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.SEPU);
                for(Events event: dbEvents){
                    event.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            }else{
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.SEPU,
                        commonUtils.getUserZoneTime(LocalDateTime.now()), null));
            }
        }
    }

    private void processLclOrAirEvents(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isLclOrAir(shipmentDetails)) {
            if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                    isEventChanged(shipmentDetails.getAdditionalDetails().getWarehouseCargoArrivalDate(),
                            oldEntity.getAdditionalDetails().getWarehouseCargoArrivalDate(), isNewShipment)) {
                if(ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.CAFS))){
                    List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.CAFS);
                    for(Events event: dbEvents){
                        event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getWarehouseCargoArrivalDate()));
                        eventDao.updateUserFieldsInEvent(event, true);
                    }
                }else{
                    events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.CAFS,
                            commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getWarehouseCargoArrivalDate()), null));
                }
            }

            processCAAWEvent(shipmentDetails, oldEntity, events, isNewShipment, cargoesRunnerDbEvents);
        }
    }

    private void processCAAWEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isEventChanged(shipmentDetails.getShipmentGateInDate(), oldEntity.getShipmentGateInDate(), isNewShipment) &&
                shipmentDetails.getDateType()!=null) {
            if(ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.CAAW))){
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.CAAW);
                processDbCaawEvent(shipmentDetails, dbEvents);
            }else{
                if(shipmentDetails.getDateType() == ACTUAL){
                    events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.CAAW,
                            commonUtils.getUserZoneTime(shipmentDetails.getShipmentGateInDate()), null));
                } else if (shipmentDetails.getDateType() == ESTIMATED) {
                    events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.CAAW, null,
                            commonUtils.getUserZoneTime(shipmentDetails.getShipmentGateInDate())));
                }
            }
        }
    }

    private void processDbCaawEvent(ShipmentDetails shipmentDetails, List<Events> dbEvents) {
        for(Events event: dbEvents){
            if(ACTUAL.equals(shipmentDetails.getDateType())) {
                event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getShipmentGateInDate()));
            }else if (ESTIMATED.equals(shipmentDetails.getDateType() )){
                event.setEstimated(commonUtils.getUserZoneTime(shipmentDetails.getShipmentGateInDate()));
            }
            eventDao.updateUserFieldsInEvent(event, true);
        }
    }

    private void processEMCREvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isFcl(shipmentDetails) && ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventBooleanChanged(shipmentDetails.getAdditionalDetails().getEmptyContainerReturned(),
                        oldEntity.getAdditionalDetails().getEmptyContainerReturned(), isNewShipment)) {

            if(ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.EMCR))){
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.EMCR);
                for(Events event: dbEvents){
                    event.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            }else{
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.EMCR,
                        commonUtils.getUserZoneTime(LocalDateTime.now()), null));
            }
        }
    }
    private void processECCCEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {

        boolean isActualChanged = isEventChanged(shipmentDetails.getBrokerageAtOriginDate(),
                oldEntity.getBrokerageAtOriginDate(), isNewShipment);

        boolean isEstimatedChanged = isEventChanged(shipmentDetails.getEstimatedBrokerageAtOriginDate(),
                oldEntity.getEstimatedBrokerageAtOriginDate(), isNewShipment);

        if ((Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP) || Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_CTS)) && (isActualChanged || isEstimatedChanged)) {
            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.ECCC))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.ECCC);
                    setActualAndEstimatedEventTime(shipmentDetails.getBrokerageAtOriginDate(),shipmentDetails.getEstimatedBrokerageAtOriginDate(),
                            dbEvents, isActualChanged, isEstimatedChanged);
            } else {

                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.ECCC,
                        Objects.nonNull(shipmentDetails.getBrokerageAtOriginDate()) ?
                                commonUtils.getUserZoneTime(shipmentDetails.getBrokerageAtOriginDate()) : null,
                        Objects.nonNull(shipmentDetails.getEstimatedBrokerageAtOriginDate()) ?
                                commonUtils.getUserZoneTime(shipmentDetails.getEstimatedBrokerageAtOriginDate()) : null));
            }
        }
    }

    private void setActualAndEstimatedEventTime(LocalDateTime actualTime,
                                                LocalDateTime estimatedTime,
                                                List<Events> events, boolean isActualChanged,
                                                boolean isEstimatedChanged) {
        for (Events event : events) {
            if (isActualChanged) {
                event.setActual(commonUtils.getUserZoneTime(actualTime));
            }
            if (isEstimatedChanged) {
                event.setEstimated(commonUtils.getUserZoneTime(estimatedTime));
            }
            eventDao.updateUserFieldsInEvent(event, true);
        }
    }

    private void processBLRSEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventChanged(shipmentDetails.getAdditionalDetails().getBlInstructionReceived(),
                        oldEntity.getAdditionalDetails().getBlInstructionReceived(), isNewShipment)) {

            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.BLRS))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.BLRS);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getBlInstructionReceived()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.BLRS,
                        commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getBlInstructionReceived()), null));
            }
        }
    }

    private void processFNMUEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (isEventChanged(shipmentDetails.getMasterBill(), oldEntity.getMasterBill(), isNewShipment) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.FNMU))) {
            List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.FNMU);
            for (Events event : dbEvents) {
                event.setContainerNumber(shipmentDetails.getMasterBill());
                eventDao.updateUserFieldsInEvent(event, true);
            }
        }
    }

    private void processCOODEvent(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, List<Events> events, Boolean isNewShipment, Map<String, List<Events>> cargoesRunnerDbEvents) {
        if (ObjectUtils.isNotEmpty(shipmentDetails.getAdditionalDetails()) &&
                isEventChanged(shipmentDetails.getAdditionalDetails().getCargoOutForDelivery(),
                        oldEntity.getAdditionalDetails().getCargoOutForDelivery(), isNewShipment)) {

            if (ObjectUtils.isNotEmpty(cargoesRunnerDbEvents) && ObjectUtils.isNotEmpty(cargoesRunnerDbEvents.get(EventConstants.COOD))) {
                List<Events> dbEvents = cargoesRunnerDbEvents.get(EventConstants.COOD);
                for (Events event : dbEvents) {
                    event.setActual(commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getCargoOutForDelivery()));
                    eventDao.updateUserFieldsInEvent(event, true);
                }
            } else {
                events.add(initializeAutomatedEvents(shipmentDetails, EventConstants.COOD,
                        commonUtils.getUserZoneTime(shipmentDetails.getAdditionalDetails().getCargoOutForDelivery()), null));
            }
        }
    }

    public void autoGenerateCreateEvent(ShipmentDetails shipmentDetails) {
        Events response = null;
        response = createAutomatedEvents(shipmentDetails, EventConstants.SHCR, commonUtils.getUserZoneTime(LocalDateTime.now()), null);

        if (shipmentDetails.getEventsList() == null) {
            shipmentDetails.setEventsList(new ArrayList<>());
        }
        shipmentDetails.getEventsList().add(response);
    }

    private Events createAutomatedEvents(ShipmentDetails shipmentDetails, String eventCode,
                                         LocalDateTime actualDateTime, LocalDateTime estimatedDateTime) {
        Events events = initializeAutomatedEvents(shipmentDetails, eventCode, actualDateTime, estimatedDateTime);
        commonUtils.updateEventWithMasterData(List.of(events));
        // Persist the event
        eventDao.save(events);
        return events;
    }

    private Events initializeAutomatedEvents(ShipmentDetails shipmentDetails, String eventCode,
                                             LocalDateTime actualDateTime, LocalDateTime estimatedDateTime) {
        Events events = new Events();
        // Set event fields from shipment
        events.setActual(actualDateTime);
        events.setEstimated(estimatedDateTime);
        events.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
        events.setIsPublicTrackingEvent(true);
        events.setEntityType(Constants.SHIPMENT);
        events.setEntityId(shipmentDetails.getId());
        events.setTenantId(TenantContext.getCurrentTenant());
        events.setEventCode(eventCode);
        events.setShipmentNumber(shipmentDetails.getShipmentId());
        events.setDirection(shipmentDetails.getDirection());
        // Attach to console as well
        eventDao.updateFieldsForShipmentGeneratedEvents(List.of(events), shipmentDetails);

        return events;
    }

    private boolean isEventChanged(Object newValue, Object oldValue, Boolean isNewShipment) {
        return Boolean.TRUE.equals(isNewShipment) ? ObjectUtils.isNotEmpty(newValue) : !Objects.equals(newValue, oldValue);
    }

    private boolean isEventBooleanChanged(Boolean newValue, Boolean oldValue, Boolean isNewShipment) {
        return Boolean.TRUE.equals(newValue) && (Boolean.TRUE.equals(isNewShipment) || !Boolean.TRUE.equals(oldValue));
    }

    private boolean isLclOrFclOrAir(ShipmentDetails shipmentDetails) {
        return SHIPMENT_TYPE_LCL.equalsIgnoreCase(shipmentDetails.getShipmentType())
                || CARGO_TYPE_FCL.equalsIgnoreCase(shipmentDetails.getShipmentType())
                || Constants.CARGO_TYPE_LTL.equalsIgnoreCase(shipmentDetails.getShipmentType())
                || Constants.CARGO_TYPE_FTL.equalsIgnoreCase(shipmentDetails.getShipmentType())
                || TRANSPORT_MODE_AIR.equalsIgnoreCase(shipmentDetails.getTransportMode());
    }

    private boolean isLclOrAir(ShipmentDetails shipmentDetails) {
        return SHIPMENT_TYPE_LCL.equalsIgnoreCase(shipmentDetails.getShipmentType())
                || Constants.CARGO_TYPE_LTL.equalsIgnoreCase(shipmentDetails.getShipmentType())
                || TRANSPORT_MODE_AIR.equalsIgnoreCase(shipmentDetails.getTransportMode());
    }

    private boolean isFcl(ShipmentDetails shipmentDetails) {
        return CARGO_TYPE_FCL.equalsIgnoreCase(shipmentDetails.getShipmentType()) || Constants.CARGO_TYPE_FTL.equalsIgnoreCase(shipmentDetails.getShipmentType());
    }
}
