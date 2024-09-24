package com.dpw.runner.shipment.services.service.TO.impl;

import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducerHelper;
import com.dpw.runner.shipment.services.commons.EAWBConstants;
import com.dpw.runner.shipment.services.dto.TO.fsu.*;
import com.dpw.runner.shipment.services.entityTO.IntegrationEntity;
import com.dpw.runner.shipment.services.entity.ShipmentEventUpdatePayload;
import com.dpw.runner.shipment.services.entity.ShipmentStatusPayload;
import com.dpw.runner.shipment.services.entity.enums.DescarteStatusCodeEnum;
import com.dpw.runner.shipment.services.entity.enums.ResponseMessageType;
import com.dpw.runner.shipment.services.entity.enums.StatusType;
import com.dpw.runner.shipment.services.dto.TO.fnm.FNMPayload;
import com.dpw.runner.shipment.services.exception.exceptions.DescartesEventUpdateException;
import com.dpw.runner.shipment.services.exception.exceptions.DescartesStatusUpdateException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repositoryTO.IntegrationRepository;
import com.dpw.runner.shipment.services.service.TO.IDescartesAdapter;
import com.dpw.runner.shipment.services.service.TO.request.DescartesRequest;
import com.dpw.runner.shipment.services.service.TO.response.DescartesBaseResponse;
import com.dpw.runner.shipment.services.service.TO.response.DescartesResponse;
import com.dpw.runner.shipment.services.service.TO.response.DescartesStatusResponse;
import com.dpw.runner.shipment.services.utils.ParseUtils;
import com.dpw.runner.shipment.services.utils.RestTemplateUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import com.google.gson.Gson;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.MultiValueMap;
import org.springframework.util.ObjectUtils;
import org.springframework.web.client.RestTemplate;

import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.Validator;
import javax.validation.ValidatorFactory;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;


@Service
@Slf4j
public class DescartesAdapterImpl implements IDescartesAdapter {

    @Autowired
    private RestTemplate restTemplate;

    @Value("${descartes.username:username}")
    private String descartesUsername;

    @Value("${descartes.password:password}")
    private String descartesPassword;

    @Value("${descartes.baseUrl:default}")
    private String descartesServiceBaseUrl;

    @Value("${descartes.documentUploadEndpoint:default}")
    private String uploadDocumentEndpoint;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private DBService dbService;

    @Autowired
    private JsonHelper jsonHelperr;

    @Autowired
    @Qualifier("secondaryEntityManagerFactory")
    private IntegrationRepository integrationRepository;

    @Value("${eawb.kafka.producer.queue}")
    private String shipmentKafkaTopic;

    @Autowired
    private KafkaProducerHelper kafkaProducerHelper;

    @Override
    public DescartesResponse pushAirWayBillDetails(String base64EncodedContent) {
        DescartesRequest descartesRequest = buildDescartesBase64Request();
        descartesRequest.setContent(base64EncodedContent);
        MultiValueMap<String, String> headers = RestTemplateUtility
                .getCustomDescartesHeaders(descartesUsername, descartesPassword);

        try {
            Gson gson = new Gson();
            String jsonRequest = gson.toJson(descartesRequest);
            HttpEntity<String> entity = new HttpEntity<>(jsonRequest, headers);

            log.info("Hitting Descartes: {} with body: {}", descartesServiceBaseUrl + uploadDocumentEndpoint, entity.getBody());
            ResponseEntity<DescartesBaseResponse> descartesResponse =
                    restTemplate.exchange(descartesServiceBaseUrl + uploadDocumentEndpoint, HttpMethod.POST, entity, DescartesBaseResponse.class);

            log.info("Decartes Response: {}", descartesResponse);
            if (descartesResponse.getBody() == null) {
                throw new RuntimeException("Invalid response from Descartes.");
            }

            return descartesResponse.getBody().getResponse();
        } catch (Exception ex) {
            log.error("Exception received from Descartes service: {}", ex.getMessage());
            throw new RuntimeException("Something went wrong while calling descartes API due to " + ex.getMessage());
        }
    }

    public void updateEvents(String json) {
        log.info(json);

        try {
            String xmlBase64 = new String(Base64.getEncoder().encode(json.getBytes()));
            XmlMapper xmlMapper = new XmlMapper();
            ValidatorFactory validatorFactory = Validation.buildDefaultValidatorFactory();
            Validator validator = validatorFactory.getValidator();

            FSUPayload payload = xmlMapper.readValue(json, FSUPayload.class);

            Set<ConstraintViolation<FSUPayload>> violations = validator.validate(payload);
            List<String> errorMessages = new ArrayList<>();
            if (!violations.isEmpty()) {
                for (ConstraintViolation<FSUPayload> violation : violations) {
                    errorMessages.add(violation.getMessage());
                }
            }

            if (errorMessages.size() > 0) {
                throw new DescartesEventUpdateException(errorMessages.stream().map(Object::toString).collect(Collectors.joining(", ")));
            }

            log.info(EAWBConstants.EAWB_LOGS + " Event update payload deserialized");
            log.info(objectMapper.writeValueAsString(payload));

            List<IntegrationEntity> entity = dbService.getByAwbNumber(payload.getMasterConsignment().getTransportContractDocument().getId());
            if (Objects.isNull(entity) || entity.size() < 1) {
                log.error("Invalid AWBNumber: -- " + objectMapper.writeValueAsString(payload));
                return;
            }

            ShipmentEventUpdatePayload shipmentEventUpdatePayload = getShipmentEventUpdatePayload(payload, xmlBase64, entity.get(0));
            dbService.saveIntegrationResponse(com.dpw.runner.shipment.services.entityTO.ResponseEntity.builder()
                    .integrationEntityId(entity.get(0))
                    .messageTypeKey(ResponseMessageType.XFSU)
                    .messageTypeValue(payload.getMasterConsignment().getReportedStatus().get(0).getReasonCode())
                    .xmlData(xmlBase64)
                    .inPayload(jsonHelperr.serialize(payload))
                    .outPayload(jsonHelperr.serialize(shipmentEventUpdatePayload)).build());

            log.info(EAWBConstants.EAWB_LOGS + " Converted Event payload to ShipmentPayload successfully");

            kafkaProducerHelper.produceToKafka(shipmentEventUpdatePayload, UUID.randomUUID().toString(), new HashMap<>() {{
                put(EAWBConstants.HEADER_TYPE_TYPE, EAWBConstants.HEADER_TYPE_EVENT);
            }}, shipmentKafkaTopic, null);

            log.info(EAWBConstants.EAWB_LOGS + " Event status Produced to kafka successfully");

        } catch (Exception ex) {
            log.error("Exception in Descartes UpdateEvent: data:-{} , exception:-- {}", json, ex.getMessage());
            throw new DescartesEventUpdateException(ex.getMessage());
        }
    }

    public void updateStatus(String data) {
        try {
            String xmlBase64 = new String(Base64.getEncoder().encode(data.getBytes()));
            XmlMapper xmlMapper = new XmlMapper();
            ValidatorFactory validatorFactory = Validation.buildDefaultValidatorFactory();
            Validator validator = validatorFactory.getValidator();

            FNMPayload payload = xmlMapper.readValue(data, FNMPayload.class);

            Set<ConstraintViolation<FNMPayload>> violations = validator.validate(payload);
            List<String> errorMessages = new ArrayList<>();
            if (!violations.isEmpty()) {
                for (ConstraintViolation<FNMPayload> violation : violations) {
                    errorMessages.add(violation.getMessage());
                }
            }

            if (errorMessages.size() > 0) {
                throw new DescartesEventUpdateException(errorMessages.stream().map(Object::toString).collect(Collectors.joining(", ")));
            }

            log.info(EAWBConstants.EAWB_LOGS + " Successfully validated and deserialized status payload");

            IntegrationEntity entity = dbService.getByUniqueId(payload.getMessageHeaderDocument().getConversationID());
            if (Objects.isNull(entity)) {
                log.error("Invalid ConversationId: -- " + objectMapper.writeValueAsString(payload));
                return;
            }

            StatusType statusType;
            String metaData = null;

            if (Objects.equals(payload.getBusinessHeaderDocument().getStatusCode(), DescarteStatusCodeEnum.Received.name())) {
                statusType = StatusType.RECEIVED;
            } else if (Objects.equals(payload.getBusinessHeaderDocument().getStatusCode(), DescarteStatusCodeEnum.Rejected.name())) {
                statusType = StatusType.REJECTED;
                metaData = payload.getResponseStatus().getReason();
            } else if (Objects.equals(payload.getBusinessHeaderDocument().getStatusCode(), DescarteStatusCodeEnum.Processed.name())) {
                statusType = StatusType.PROCESSED;
            } else {
                log.error("Invalid status code:----" + payload.getBusinessHeaderDocument().getStatusCode());
                return;
            }

            entity.setStatus(statusType);
            entity.setMetaData(metaData);
            dbService.saveIntegration(entity);

            ShipmentStatusPayload shipmentStatusPayload = getShipmentStatusPayload(entity, xmlBase64);
            dbService.saveIntegrationResponse(com.dpw.runner.shipment.services.entityTO.ResponseEntity.builder()
                    .integrationEntityId(entity)
                    .messageTypeKey(ResponseMessageType.XFNM)
                    .messageTypeValue(entity.getStatus().name())
                    .metaData(entity.getMetaData())
                    .xmlData(xmlBase64)
                    .inPayload(jsonHelperr.serialize(payload))
                    .outPayload(jsonHelperr.serialize(shipmentStatusPayload)).build());

            log.info(EAWBConstants.EAWB_LOGS + " Converted Status payload to Shipment successfully");

            kafkaProducerHelper.produceToKafka(shipmentStatusPayload, UUID.randomUUID().toString(), new HashMap<>() {{
                put(EAWBConstants.HEADER_TYPE_TYPE, EAWBConstants.HEADER_TYPE_STATUS);
            }}, shipmentKafkaTopic, null);

            log.info(EAWBConstants.EAWB_LOGS + " Status Produced to kafka successfully");

        } catch (Exception ex) {
            log.error("Exception in Descartes UpdateStatus: data:-{} , exception:-- {}", data, ex.getMessage());
            throw new DescartesStatusUpdateException(ex.getMessage());
        }

    }

    private ShipmentStatusPayload getShipmentStatusPayload(IntegrationEntity entity, String xml) {
        ShipmentStatusPayload shipmentStatusPayload = new ShipmentStatusPayload();
        shipmentStatusPayload.setGuid(entity.getEntityId());
        shipmentStatusPayload.setStatus(entity.getStatus());
        shipmentStatusPayload.setErrorMessage(entity.getMetaData());
        shipmentStatusPayload.setMessageType(ResponseMessageType.XFNM.name());
        shipmentStatusPayload.setXmlPayload(xml);
        return shipmentStatusPayload;
    }

    private ShipmentEventUpdatePayload getShipmentEventUpdatePayload(FSUPayload payload, String xmlbase64, IntegrationEntity entity) {
        ShipmentEventUpdatePayload shipmentEventUpdatePayload = new ShipmentEventUpdatePayload();

        // set guid
        shipmentEventUpdatePayload.setGuid(entity.getEntityId());

        ReportedStatus reportedStatus = payload.getMasterConsignment().getReportedStatus().get(0);
        if (Objects.nonNull(reportedStatus))
            shipmentEventUpdatePayload.setEventCode(reportedStatus.getReasonCode());
        List<AssociatedStatusConsignment> associatedStatusConsignments = reportedStatus.getAssociatedStatusConsignment();
        if (Objects.nonNull(associatedStatusConsignments)) {
            AssociatedStatusConsignment associatedStatusConsignment = associatedStatusConsignments.get(0);
            if (Objects.nonNull(associatedStatusConsignment.getSpecifiedLogisticsTransportMovement())) {
                FSULogisticsTransportMovement transportMovement = associatedStatusConsignment.getSpecifiedLogisticsTransportMovement().get(0);
                if (Objects.nonNull(transportMovement) && Objects.nonNull(transportMovement.getSpecifiedLocations()) && !transportMovement.getSpecifiedLocations().isEmpty()){
                    shipmentEventUpdatePayload.setPlaceOfEvent(transportMovement.getSpecifiedLocations().get(0).getId());
                    shipmentEventUpdatePayload.setPlaceDescription(transportMovement.getSpecifiedLocations().get(0).getTypeCode());
                }

                // adding specified event
                if (Objects.nonNull(transportMovement.getSpecifiedEvent())) {
                    FSUSpecifiedEventDto specifiedEvent = transportMovement.getSpecifiedEvent();
                    setEvent(shipmentEventUpdatePayload, specifiedEvent);
                }

                // departure
                if (Objects.nonNull(transportMovement.getDepartureEvent())) {
                    FSUDepartureEventDto departureEvent = transportMovement.getDepartureEvent();
                    setDeparture(shipmentEventUpdatePayload, departureEvent);
                }

                // arrival
                if (Objects.nonNull(transportMovement.getArrivalEvent())) {
                    FSUArrivalEventDto arrivalEvent = transportMovement.getArrivalEvent();
                    setArrival(shipmentEventUpdatePayload, arrivalEvent);
                }

                // adding source
                if (Objects.nonNull(transportMovement.getCarrierParty())) {
                    shipmentEventUpdatePayload.setSourceCarrier(transportMovement.getCarrierParty().getPrimaryId());
                }

            }
        }

        // extra
        shipmentEventUpdatePayload.setTimeOfReceivingFSUMessage(LocalDateTime.now());
        shipmentEventUpdatePayload.setTimeOfEventSentByCarrier(LocalDateTime.parse(payload.getMessageHeaderDocument().getIssueDateTime().replace("Z","")));
        shipmentEventUpdatePayload.setOrigin(payload.getMasterConsignment().getOriginLocation().getId());
        shipmentEventUpdatePayload.setDestination(payload.getMasterConsignment().getFinalDestinationLocation().getId());
        shipmentEventUpdatePayload.setPieces(payload.getMasterConsignment().getPieceQuantity());
        shipmentEventUpdatePayload.setTotalPieces(payload.getMasterConsignment().getTotalPieceQuantity());
        if (Objects.nonNull(payload.getMasterConsignment().getGrossWeightMeasure()))
            shipmentEventUpdatePayload.setWeight(ParseUtils.parseDoubleFromString(payload.getMasterConsignment().getGrossWeightMeasure().getValue()));
        if (Objects.nonNull(payload.getMasterConsignment().getTotalGrossWeightMeasure()))
            shipmentEventUpdatePayload.setTotalWeight(ParseUtils.parseDoubleFromString(payload.getMasterConsignment().getTotalGrossWeightMeasure().getValue()));

        shipmentEventUpdatePayload.setIsPartial(payload.getMasterConsignment().getTransportSplitDescription());
        shipmentEventUpdatePayload.setXmlPayload(xmlbase64);

        return shipmentEventUpdatePayload;

    }

    private void setArrival(ShipmentEventUpdatePayload shipmentEventUpdatePayload, FSUArrivalEventDto arrivalEvent) {
        switch (arrivalEvent.getArrivalDateTimeTypeCode()) {
            case "S" ->
                    shipmentEventUpdatePayload.setScheduledTimeOfArrival(LocalDateTime.parse(arrivalEvent.getArrivalOccurrenceDateTime()));
            case "E" ->
                    shipmentEventUpdatePayload.setEstimatedTimeOfArrival(LocalDateTime.parse(arrivalEvent.getArrivalOccurrenceDateTime()));
            case "A" ->
                    shipmentEventUpdatePayload.setActualTimeOfArrival(LocalDateTime.parse(arrivalEvent.getArrivalOccurrenceDateTime()));
            default -> throw new DescartesEventUpdateException("Invalid Departure event code provided");
        }
    }

    private void setDeparture(ShipmentEventUpdatePayload shipmentEventUpdatePayload, FSUDepartureEventDto departureEvent) {
        switch (departureEvent.getDepartureDateTimeTypeCode()) {
            case "S" ->
                    shipmentEventUpdatePayload.setScheduledTimeOfDeparture(LocalDateTime.parse(departureEvent.getDepartureOccurrenceDateTime()));
            case "E" ->
                    shipmentEventUpdatePayload.setEstimatedTimeOfDeparture(LocalDateTime.parse(departureEvent.getDepartureOccurrenceDateTime()));
            case "A" ->
                    shipmentEventUpdatePayload.setActualTimeOfDeparture(LocalDateTime.parse(departureEvent.getDepartureOccurrenceDateTime()));
            default -> throw new DescartesEventUpdateException("Invalid Departure event code provided");
        }
    }

    private void setEvent(ShipmentEventUpdatePayload shipmentEventUpdatePayload, FSUSpecifiedEventDto specifiedEvent) {
        switch (specifiedEvent.getDateTimeTypeCode()) {
            case "S" ->
                    shipmentEventUpdatePayload.setScheduledTimeOfEvent(LocalDateTime.parse(specifiedEvent.getOccurrenceDateTime()));
            case "E" ->
                    shipmentEventUpdatePayload.setEstimatedTimeOfEvent(LocalDateTime.parse(specifiedEvent.getOccurrenceDateTime()));
            case "A" ->
                    shipmentEventUpdatePayload.setActualTimeOfEvent(LocalDateTime.parse(specifiedEvent.getOccurrenceDateTime()));
            default -> throw new DescartesEventUpdateException("Invalid Specified event code provided");
        }
    }

    public DescartesRequest buildDescartesBase64Request() {
        DescartesRequest descartesRequest = new DescartesRequest();
        descartesRequest.setContentType(EAWBConstants.TEXT_XML);
        descartesRequest.setApiVersion(EAWBConstants.DESCARTES_API_VERSION);
        descartesRequest.setContentEncoding(EAWBConstants.DESCARTES_CONTENT_ENCODING);
        descartesRequest.setContentCharset(EAWBConstants.DESCARTES_CONTENT_CHARSET);

        return descartesRequest;
    }

    @Transactional
    @Override
    public DescartesStatusResponse getStatusResponse(String entityId) {
        if (ObjectUtils.isEmpty(entityId)) {
            throw new RuntimeException("Entity Id is mandatory to fetch the integration status");
        }

        IntegrationEntity integrationEntity = integrationRepository.findFirstByEntityIdOrderByCreatedAtDesc(entityId);
        if (integrationEntity == null) {
            throw new RuntimeException("No such integration entity exits with this id");
        }

        DescartesStatusResponse descartesStatusResponse = new DescartesStatusResponse();
        descartesStatusResponse.setStatus(integrationEntity.getStatus() != null ? integrationEntity.getStatus().toString() : null);
        descartesStatusResponse.setAwbNumber(integrationEntity.getAwbNumber());
        descartesStatusResponse.setMessageType(integrationEntity.getMessageType() != null ? integrationEntity.getMessageType().toString() : null);
        descartesStatusResponse.setIntegrationStatusResponse(integrationEntity.getStatusList());

        return descartesStatusResponse;
    }
}