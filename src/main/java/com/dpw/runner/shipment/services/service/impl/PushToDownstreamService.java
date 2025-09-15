package com.dpw.runner.shipment.services.service.impl;

import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION;

import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.LogHistoryRequest;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload.UniversalEventsPayload;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.dto.OrderManageDto;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto.Meta;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto.Triggers;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.interfaces.ILogsHistoryService;
import com.dpw.runner.shipment.services.service.interfaces.IPickupDeliveryDetailsService;
import com.dpw.runner.shipment.services.service.interfaces.IPushToDownstreamService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.BookingIntegrationsUtility;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
public class PushToDownstreamService implements IPushToDownstreamService {

    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private IContainerDao containerDao;
    @Autowired
    private ICustomerBookingDao customerBookingDao;
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
    @Autowired
    private IContainerV3Service containerV3Service;
    @Autowired
    private KafkaProducer producer;
    @Value("${containersKafka.queue}")
    private String containerKafkaQueue;
    @Value("${consolidationsKafka.queue}")
    private String consolidationKafkaQueue;
    @Value("${booking.event.kafka.queue}")
    private String bookingEventKafkaQueue;
    @Autowired
    private IConsolidationV3Service consolidationV3Service;
    @Autowired
    private ITrackingServiceAdapter trackingServiceAdapter;

    @Autowired
    private IPickupDeliveryDetailsService pickupDeliveryDetailsService;

    @Transactional
    @Override
    public void process(PushToDownstreamEventDto message, String transactionId) {
        // Setting Service account auth token for v1 call
        v1Service.setAuthContext();
        // Process the messages based on parent entity - "SHIPMENT", "CONSOLIDATION"
        if (Objects.equals(message.getParentEntityName(), Constants.SHIPMENT)) {
            processShipmentData(message);
        } else if (Constants.CONTAINER.equalsIgnoreCase(message.getParentEntityName())) {
            processContainerData(message, transactionId);
        } else if (Constants.CONSOLIDATION.equalsIgnoreCase(message.getParentEntityName())) {
            pushConsolidationDataToService(message, transactionId);
        } else if (Objects.equals(message.getParentEntityName(), Constants.CUSTOMER_BOOKING)) {
            pushCustomerBookingDataToService(message, transactionId);
        } else if (Objects.equals(message.getParentEntityName(), Constants.TRANSPORT_INSTRUCTION)) {
            pushTransportInstructionDataToTIQueue(message, transactionId);
        }
    }

    private void processContainerData(PushToDownstreamEventDto message, String transactionId) {

        if (Constants.CONTAINER_AFTER_SAVE.equalsIgnoreCase(message.getMeta().getSourceInfo())) {
            log.info("[InternalKafkaPush] Pushing container data | containerId={} | transactionId={}",
                    message.getParentEntityId(), transactionId);

            this.pushContainerData(message, transactionId);

            // Processing the Dependent Triggers for given parent trigger
            if (message.getTriggers() != null) {
                log.info("[InternalKafkaPush] Found {} dependent triggers for containerId={} | transactionId={}",
                        message.getTriggers().size(), message.getParentEntityId(), transactionId);

                message.getTriggers().forEach(trigger -> {
                    String triggerEntity = trigger.getEntityName();
                    Long triggerEntityId = trigger.getEntityId();

                    if (Objects.equals(triggerEntity, Constants.SHIPMENT)) {
                        log.info("[InternalKafkaPush] Triggering shipment push | shipmentId={} | source=Container | transactionId={}",
                                triggerEntityId, transactionId);
                        this.pushShipmentData(triggerEntityId, false, false);
                    }
                    sendBookingUpdateToPlatform(message, transactionId, triggerEntity, triggerEntityId);

                    if (Objects.equals(triggerEntity, Constants.CONSOLIDATION)) {

                        log.info("[InternalKafkaPush] Triggering consolidation push | consolidationId={} | phase=AFTER_SAVE | transactionId={}",
                                triggerEntityId, transactionId);
                        PushToDownstreamEventDto build1 = PushToDownstreamEventDto.builder()
                                .parentEntityId(trigger.getEntityId())
                                .parentEntityName(CONSOLIDATION)
                                .meta(Meta.builder()
                                        .sourceInfo(Constants.CONSOLIDATION_AFTER_SAVE)
                                        .isCreate(Boolean.FALSE)
                                        .tenantId(message.getMeta().getTenantId()).build()).build();
                        pushConsolidationDataToService(build1, transactionId);

                        log.info("[InternalKafkaPush] Triggering consolidation push | consolidationId={} | phase=AFTER_SAVE_TO_TRACKING | transactionId={}",
                                triggerEntityId, transactionId);
                        PushToDownstreamEventDto build2 = PushToDownstreamEventDto.builder()
                                .parentEntityId(trigger.getEntityId())
                                .parentEntityName(CONSOLIDATION)
                                .meta(Meta.builder()
                                        .sourceInfo(Constants.CONSOLIDATION_AFTER_SAVE_TO_TRACKING)
                                        .isCreate(Boolean.FALSE)
                                        .tenantId(message.getMeta().getTenantId()).build()).build();
                        pushConsolidationDataToService(build2, transactionId);
                    }
                });
            } else {
                log.info("[InternalKafkaPush] No dependent triggers found for containerId={} | transactionId={}",
                        message.getParentEntityId(), transactionId);
            }
        }
    }

    private void sendBookingUpdateToPlatform(PushToDownstreamEventDto message, String transactionId, String triggerEntity, Long triggerEntityId) {
        if (Objects.equals(triggerEntity, Constants.CUSTOMER_BOOKING)) {
            log.info("[InternalKafkaPush] Triggering shipment push | Booking={} | source=Container | transactionId={}",
                    triggerEntityId, transactionId);
            PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
            pushToDownstreamEventDto.setParentEntityId(triggerEntityId);
            pushToDownstreamEventDto.setParentEntityName(triggerEntity);
            pushToDownstreamEventDto.setMeta(message.getMeta());
            this.pushCustomerBookingDataToPlatform(pushToDownstreamEventDto, transactionId);
        }
    }

    private void processShipmentData(PushToDownstreamEventDto message) {

        this.pushShipmentData(message.getParentEntityId(), message.getMeta().getIsCreate(), message.getMeta().getIsAutoSellRequired());
        // Processing the Dependent Triggers for given parent trigger
        if (message.getTriggers() != null) {
            message.getTriggers().forEach(trigger -> {
                // Dependent Triggers for Shipment
                if (Objects.equals(trigger.getEntityName(), Constants.SHIPMENT)) {
                    this.pushShipmentData(trigger.getEntityId(), false, false);
                }
            });
        }
    }

    private void pushTransportInstructionDataToTIQueue(PushToDownstreamEventDto message, String transactionId) {
        Optional<PickupDeliveryDetails> pickupDeliveryDetails = pickupDeliveryDetailsService.findById(message.getParentEntityId());
        if (pickupDeliveryDetails.isPresent()) {
            PickupDeliveryDetails pickupDeliveryDetailsEntity = pickupDeliveryDetails.get();
            if (pickupDeliveryDetailsEntity.getShipmentId() != null) {
                List<PickupDeliveryDetails> pickupDeliveryDetailsList = pickupDeliveryDetailsService.findByShipmentId(pickupDeliveryDetailsEntity.getShipmentId());
                if (!CommonUtils.listIsNullOrEmpty(pickupDeliveryDetailsList)) {
                    log.info("Processing TI down stream data for shipment id: {}", pickupDeliveryDetailsEntity.getShipmentId());
                    pickupDeliveryDetailsService.processDownStreamConsumerData(pickupDeliveryDetailsList, pickupDeliveryDetailsEntity.getShipmentId(), message, transactionId);
                }
            }
        }
    }

    private void pushConsolidationDataToService(PushToDownstreamEventDto message, String transactionId) {
        if (Constants.CONSOLIDATION_AFTER_SAVE.equalsIgnoreCase(message.getMeta().getSourceInfo())) {
            this.pushConsolidationData(message, transactionId);
        }
        if (Constants.CONSOLIDATION_AFTER_SAVE_TO_TRACKING.equalsIgnoreCase(message.getMeta().getSourceInfo())) {
            this.pushConsolidationDataToTracking(message, transactionId);
        }
    }

    private void pushCustomerBookingDataToService(PushToDownstreamEventDto message, String transactionId) {
        if (Constants.CUSTOMER_BOOKING_TO_OMS_SYNC.equalsIgnoreCase(message.getMeta().getSourceInfo())) {
            this.pushCustomerBookingDataToOMS(message, transactionId);
        } else if (Constants.CUSTOMER_BOOKING_TO_PLATFORM_SYNC.equalsIgnoreCase(message.getMeta().getSourceInfo())) {
            this.pushCustomerBookingDataToPlatform(message, transactionId);
        }
    }

    @Override
    public void pushContainerData(PushToDownstreamEventDto eventDto, String transactionId) {
        Long parentEntityId = eventDto.getParentEntityId();
        Boolean isCreate = eventDto.getMeta().getIsCreate();
        Integer tenantId = eventDto.getMeta().getTenantId();
        log.info("[InternalKafkaConsume] Pushing container data | transactionId={} | parentEntityId={} | isCreate={}",
                transactionId, parentEntityId, isCreate);

        // Fetch container data
        TenantContext.setCurrentTenant(tenantId);
        List<Containers> containersList = containerV3Service.findByIdIn(List.of(parentEntityId));
        if (containersList.isEmpty()) {
            log.warn("[InternalKafkaConsume] No containers found for parentEntityId={} | transactionId={}",
                    parentEntityId, transactionId);
            return;
        }

        Containers container = containersList.get(0);
        log.debug("[InternalKafkaConsume] Container details: {} | transactionId={}",
                container, transactionId);

        // Prepare Kafka message
        KafkaResponse kafkaResponse = producer.getKafkaResponse(container, isCreate);
        String message = jsonHelper.convertToJson(kafkaResponse);
        log.debug("[InternalKafkaConsume] Kafka payload: {} | transactionId={}",
                message, transactionId);

        if(container.getConsolidationId() != null) {
            List<Containers> containersList1 = containerDao.findByConsolidationId(container.getConsolidationId());
            containerV3Service.pushContainersToDependentServices(containersList1);
        }

        // Send message to Kafka
        producer.produceToKafka(message, containerKafkaQueue, transactionId);
        log.info("[InternalKafkaConsume] Kafka message sent to queue='{}' | transactionId={}",
                containerKafkaQueue, transactionId);
        TenantContext.removeTenant();
    }

    @Override
    public void pushConsolidationData(PushToDownstreamEventDto eventDto, String transactionId) {
        Long parentEntityId = eventDto.getParentEntityId();
        Boolean isCreate = eventDto.getMeta().getIsCreate();
        Integer tenantId = eventDto.getMeta().getTenantId();

        TenantContext.setCurrentTenant(tenantId);

        Optional<ConsolidationDetails> consolidationDetailsOpt = consolidationV3Service.findById(parentEntityId);

        if (consolidationDetailsOpt.isPresent()) {
            ConsolidationDetails consolidationDetails = consolidationDetailsOpt.get();

            if (consolidationDetails.getTenantId() == null) {
                consolidationDetails.setTenantId(TenantContext.getCurrentTenant());
            }

            // block 1
            KafkaResponse kafkaResponse = producer.getKafkaResponse(consolidationDetails, isCreate);
            kafkaResponse.setTransactionId(UUID.randomUUID().toString());
            log.info("Producing consolidation data to kafka with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(kafkaResponse));
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), consolidationKafkaQueue, StringUtility.convertToString(consolidationDetails.getGuid()));

            // block 2
            if (consolidationDetails.getShipmentsList() != null) {
                List<Long> shipmentIds = consolidationDetails.getShipmentsList().stream().map(BaseEntity::getId).toList();
                if (!shipmentIds.isEmpty()) {
                    List<ShipmentDetails> shipments = shipmentDao.findShipmentsByIds(new HashSet<>(shipmentIds));
                    for (ShipmentDetails shipment : shipments) {
                        dependentServiceHelper.pushShipmentDataToDependentService(shipment, false, false, shipment.getContainersList());
                    }
                }
            }

            // block 3
            List<Events> events = trackingServiceAdapter.getAllEvents(null, consolidationDetails, consolidationDetails.getReferenceNumber());
            UniversalEventsPayload universalEventsPayload = trackingServiceAdapter.mapEventDetailsForTracking(consolidationDetails.getReferenceNumber(), Constants.CONSOLIDATION,
                    consolidationDetails.getConsolidationNumber(), events);
            List<UniversalTrackingPayload.UniversalEventsPayload> trackingPayloads = new ArrayList<>();
            if (universalEventsPayload != null) {
                trackingPayloads.add(universalEventsPayload);
                String jsonBody = jsonHelper.convertToJson(trackingPayloads);
                log.info("Producing tracking service payload from consolidation with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonBody);
                trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody, true);
            }
        }
        TenantContext.removeTenant();
    }

    @Override
    public void pushConsolidationDataToTracking(PushToDownstreamEventDto eventDto, String transactionId) {
        Long parentEntityId = eventDto.getParentEntityId();
        Integer tenantId = eventDto.getMeta().getTenantId();

        TenantContext.setCurrentTenant(tenantId);

        Optional<ConsolidationDetails> consolidationDetailsOpt = consolidationV3Service.findById(parentEntityId);

        if (consolidationDetailsOpt.isPresent()) {
            ConsolidationDetails consolidationDetails = consolidationDetailsOpt.get();

            if (consolidationDetails.getTenantId() == null) {
                consolidationDetails.setTenantId(TenantContext.getCurrentTenant());
            }

            if (trackingServiceAdapter.checkIfConsolContainersExist(consolidationDetails) || trackingServiceAdapter.checkIfAwbExists(consolidationDetails)) {

                List<Triggers> triggers = eventDto.getTriggers();
                Set<Long> shipmentIds = triggers.stream().map(Triggers::getEntityId).collect(Collectors.toSet());
                List<ShipmentDetails> shipmentDetailsList = shipmentDao.findShipmentsByIds(shipmentIds);

                for (ShipmentDetails shipmentDetails : shipmentDetailsList) {
                    UniversalTrackingPayload payload = trackingServiceAdapter.mapConsoleDataToTrackingServiceData(
                            consolidationDetails, shipmentDetails);
                    List<UniversalTrackingPayload> payloadList = new ArrayList<>();
                    if (payload != null) {
                        payloadList.add(payload);
                        var jsonBody = jsonHelper.convertToJson(payloadList);
                        log.info(
                                "Producing tracking service payload from consolidation with RequestId: {} and payload: {}",
                                LoggerHelper.getRequestIdFromMDC(), jsonBody);
                        trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody,
                                false);
                    }
                }
            }


        }
        TenantContext.removeTenant();
    }

    @Override
    public void pushCustomerBookingDataToPlatform(PushToDownstreamEventDto downstreamEventDto, String transactionId) {
        Integer tenantId = downstreamEventDto.getMeta().getTenantId();
        TenantContext.setCurrentTenant(tenantId);
        Optional<CustomerBooking> customerBooking = customerBookingDao.findById(downstreamEventDto.getParentEntityId());
        if (customerBooking.isEmpty()) {
            log.info("[InternalKafkaConsume] Customer Booking: {} | transactionId={} not found.", downstreamEventDto.getParentEntityId(), transactionId);
            return;
        }
        log.info("[InternalKafkaConsume] Initiating booking creation in platform with payload: {} | transactionId={}", jsonHelper.convertToJson(customerBooking.get()), transactionId);
        // Create at platform
        bookingIntegrationsUtility.createBookingInPlatform(customerBooking.get());

        log.info("[InternalKafkaConsume] Customer booking creation done at platform | transactionId={}", transactionId);
    }

    private void pushCustomerBookingDataToOMS(PushToDownstreamEventDto downstreamEventDto, String transactionId) {
        Integer tenantId = downstreamEventDto.getMeta().getTenantId();
        TenantContext.setCurrentTenant(tenantId);
        Optional<CustomerBooking> customerBookingOptional = customerBookingDao.findById(downstreamEventDto.getParentEntityId());
        if (customerBookingOptional.isEmpty()) {
            log.info("[InternalKafkaConsume] Customer Booking: {} | transactionId={} not found.", downstreamEventDto.getParentEntityId(), transactionId);
            return;
        }
        CustomerBooking customerBooking = customerBookingOptional.get();
        OrderManageDto.OrderManagement orderManagement = OrderManageDto.OrderManagement.builder()
                .orderManagementId(customerBooking.getOrderManagementId())
                .orderManagementNumber(customerBooking.getOrderManagementNumber())
                .moduleStatus(customerBooking.getBookingStatus())
                .moduleId(customerBooking.getBookingNumber())
                .moduleGuid(customerBooking.getGuid().toString())
                .tenantId(TenantContext.getCurrentTenant())
                .build();

        producer.produceToKafka(jsonHelper.convertToJson(orderManagement), bookingEventKafkaQueue, StringUtility.convertToString(customerBooking.getGuid()));
        log.info("[InternalKafkaConsume] Customer booking data sent at OMS | transactionId={}", transactionId);
    }

    private void pushShipmentData(Long entityId, boolean isCreate, boolean isAutoSellRequired) {
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentByIdWithQuery(entityId);
        if (shipmentDetails.isEmpty()) {
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
