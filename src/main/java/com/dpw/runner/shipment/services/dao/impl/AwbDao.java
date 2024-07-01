package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.Kafka.Dto.AwbShipConsoleDto;
import com.dpw.runner.shipment.services.Kafka.Dto.KafkaResponse;
import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.response.AwbAirMessagingResponse;
import com.dpw.runner.shipment.services.dto.response.AwbResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IAwbRepository;
import com.dpw.runner.shipment.services.utils.AwbUtility;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.*;

@Repository
@Slf4j
public class AwbDao implements IAwbDao {
    @Autowired
    private IAwbRepository awbRepository;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private KafkaProducer producer;
    @Value("${awbKafka.queue}")
    private String senderQueue;
    @Autowired
    @Lazy
    private AwbUtility awbUtility;
    @Autowired
    IShipmentDao shipmentDao;
    @Autowired
    IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Autowired
    private IEventDao eventDao;
    @Override
    public Awb save(Awb awbShipmentInfo) throws RunnerException {
        boolean isCreate = false; // TODO- handle create/update here
        if (awbShipmentInfo.getId() == null) {
            isCreate = true;
        }
        applyValidations(awbShipmentInfo);
        Awb awb = awbRepository.save(awbShipmentInfo);
        pushToKafka(awb, isCreate);
        return awb;
    }

    @Async
    public void pushToKafka(Awb awb, boolean isCreate) {
        try {
            if(awb.getTenantId() == null)
                awb.setTenantId(TenantContext.getCurrentTenant());
            AwbResponse awbResponse = jsonHelper.convertValue(awb, AwbResponse.class);
            if(awb.getShipmentId() != null) {
                Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(awb.getShipmentId());
                if(shipmentDetails.isPresent())
                    awbResponse.setAwbKafkaEntity(jsonHelper.convertValue(shipmentDetails.get(), AwbShipConsoleDto.class));
                else
                    return;
            } else if (awb.getConsolidationId() != null) {
                Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(awb.getConsolidationId());
                if(consolidationDetails.isPresent())
                    awbResponse.setAwbKafkaEntity(jsonHelper.convertValue(consolidationDetails.get(), AwbShipConsoleDto.class));
                else
                    return;
            }
            else {
                return;
            }

            KafkaResponse kafkaResponse = producer.getKafkaResponse(awbResponse, isCreate);
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, UUID.randomUUID().toString());
        }
        catch (Exception e)
        {
            log.error("Error pushing awb to kafka");
        }
    }

    @Override
    public Page<Awb> findAll(Specification<Awb> spec, Pageable pageable) {
        return awbRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Awb> findById(Long id) {
        return awbRepository.findById(id);
    }

    @Override
    public Optional<Awb> findByGuid(UUID guid) {
        return awbRepository.findByGuid(guid);
    }

    @Override
    public List<Awb> findByShipmentId(Long shipmentId) {
        return awbRepository.findByShipmentId(shipmentId);
    }

    @Override
    public List<Awb> findByConsolidationId(Long consolidationId) {return awbRepository.findByConsolidationId(consolidationId);}

    @Override
    public List<Awb> findByShipmentIdList(List<Long> shipmentIds) {
        return awbRepository.findByShipmentIdList(shipmentIds);
    }

    @Override
    public List<Awb> findByShipmentIdByQuery(Long shipmentId) {
        return awbRepository.findByShipmentIdByQuery(shipmentId);
    }

    @Override
    public List<Awb> findByShipmentIdsByQuery(List<Long> shipmentIds) {
        return awbRepository.findByShipmentIdsByQuery(shipmentIds);
    }
    
    @Override
    public List<Awb> findByConsolidationIdByQuery(Long consolidationId) {
        return awbRepository.findByConsolidationIdByQuery(consolidationId);
    }

    @Override
    public List<Awb> findByIssuingAgent(String issuingAgent) { return awbRepository.findByIssuingAgent(issuingAgent);}

    @Override
    public List<Awb> findByAwbNumber(List<String> awbNumber) { return awbRepository.findByAwbNumber(awbNumber);}

    @Override
    public List<Awb> findByAwbNumberAndIssuingAgent(List<String> awbNumber, String issuingAgent) {
        return awbRepository.findByAwbNumberAndIssuingAgent(awbNumber, issuingAgent);
    }

    @Override
    public List<Awb> saveAll(List<Awb> req) {
        List<Awb> entities = awbRepository.saveAll(req);
        for (Awb awb: entities)
        {
            pushToKafka(awb, false);
        }
        return entities;
    }

    private void applyValidations(Awb awb) throws RunnerException {
        Set<String> errors = new HashSet<>();
        // do not allow duplicate pair of Information Identifier and Trade Identification Code
        if(awb.getAwbOciInfo() != null) {
            Set<Pair<Integer, Integer>> uniqueOciPair = new HashSet<>();
            for(var ociInfo : awb.getAwbOciInfo()) {
                Pair<Integer, Integer> pair = Pair.of(ociInfo.getInformationIdentifier(), ociInfo.getTradeIdentificationCode());
                if(!uniqueOciPair.contains(pair)) {
                    uniqueOciPair.add(pair);
                }
                else {
                    errors.add(AwbConstants.DUPLICATE_PAIR_AWB_OCI_INFO_VALIDATION);
                }
            }
        }

        // max size for IATA description is 3 in otherChargesInfo
        if(awb.getAwbOtherChargesInfo() != null) {
            for(var otherCharges : awb.getAwbOtherChargesInfo()) {
                if(otherCharges.getIataDescription() != null && otherCharges.getIataDescription().length() > 3) {
                    errors.add(AwbConstants.IATA_DESCRIPTION_FIELD_VALIDATION);
                }
            }
        }

        if(!errors.isEmpty())
            throw new RunnerException(errors.toString());
    }

    @Override
    public void airMessagingIntegration(Long id, String reportType, Boolean fromShipment) {
        try {
            if (Objects.equals(reportType, ReportConstants.MAWB)) {
                if (!Boolean.TRUE.equals(fromShipment)) {
                    Awb awb = getMawb(id);
                    if (awb.getConsolidationId() != null) {
                        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(awb.getConsolidationId());
                        if (consolidationDetails.isPresent()) {
                            this.pushToKafkaForAirMessaging(awb, null, consolidationDetails.get());
                            // AirMessageSent flag set to SENT
                            this.updateAirMessageStatus(awb.getGuid(), AwbStatus.AIR_MESSAGE_SENT.name());
                            this.updateLinkedHawbAirMessageStatus(awb.getGuid(), AwbStatus.AIR_MESSAGE_SENT.name());
                            this.updateUserDetails(awb.getGuid(), UserContext.getUser().DisplayName, UserContext.getUser().Email);
                            this.createAirMessagingEvents(consolidationDetails.get().getId(), Constants.CONSOLIDATION, EventConstants.FWB_FZB_EVENT_CODE, "FWB&FZB sent");
                            for (ShipmentDetails ship : consolidationDetails.get().getShipmentsList()) {
                                Awb shipAwb = getHawb(ship.getId());
                                this.pushToKafkaForAirMessaging(shipAwb, ship, null);
                                // AirMessageSent flag set to SENT
                                this.updateAirMessageStatus(shipAwb.getGuid(), AwbStatus.AIR_MESSAGE_SENT.name());
                                this.updateUserDetails(shipAwb.getGuid(), UserContext.getUser().DisplayName, UserContext.getUser().Email);
                                this.createAirMessagingEvents(ship.getId(), Constants.SHIPMENT, EventConstants.FWB_FZB_EVENT_CODE, "FWB&FZB sent");
                            }
                        }
                    }
                } else {
                    Awb awb = getHawb(id);
                    if (awb.getShipmentId() != null) {
                        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(awb.getShipmentId());
                        if (shipmentDetails.isPresent()) {
                            this.pushToKafkaForAirMessaging(awb, shipmentDetails.get(), null);
                            // AirMessageSent flag set to SENT
                            this.updateAirMessageStatus(awb.getGuid(), AwbStatus.AIR_MESSAGE_SENT.name());
                            this.updateUserDetails(awb.getGuid(), UserContext.getUser().DisplayName, UserContext.getUser().Email);
                            this.createAirMessagingEvents(shipmentDetails.get().getId(), Constants.SHIPMENT, EventConstants.FWB_EVENT_CODE, "FWB sent");
                        }

                    }
                }
            }
        } catch (Exception e)
        {
            log.error("Error pushing awb to kafka for entityId: {} with error: {}", id, e.getMessage());
        }
    }

    private void createAirMessagingEvents(Long entityId, String entityType, String eventCode, String description) {
        Events events = new Events();
        events.setEntityType(entityType);
        events.setEntityId(entityId);
        events.setEventCode(eventCode);
        events.setDescription(description);
        events.setEstimated(LocalDateTime.now());
        events.setActual(LocalDateTime.now());
        events.setSource(Constants.CARGO_RUNNER);
        eventDao.save(events);
    }

    public void pushToKafkaForAirMessaging(Awb awb, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails) {
        if(awb.getTenantId() == null)
            awb.setTenantId(TenantContext.getCurrentTenant());
        AwbAirMessagingResponse awbResponse;
        if(shipmentDetails != null) {
            awbResponse = awbUtility.createAirMessagingRequestForShipment(awb, shipmentDetails);
            awbResponse.setAwbKafkaEntity(jsonHelper.convertValue(shipmentDetails, AwbShipConsoleDto.class));
        } else if (consolidationDetails != null) {
            awbResponse = awbUtility.createAirMessagingRequestForConsole(awb, consolidationDetails);
            awbResponse.setAwbKafkaEntity(jsonHelper.convertValue(consolidationDetails, AwbShipConsoleDto.class));
        } else {
            return;
        }
        KafkaResponse kafkaResponse = getKafkaResponseForAirMessaging(awbResponse);
        log.info("RequestId: {} || Event: {} || message: {}", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.AIR_MESSAGING_EVENT_PUSH_TO_KAFKA, jsonHelper.convertToJson(kafkaResponse));
        producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, UUID.randomUUID().toString());
    }

    public Awb getHawb(Long id) {
        List<Awb> awb = awbRepository.findByShipmentId(id);
        if (awb != null && !awb.isEmpty())
            return awb.get(0);
        return null;
    }

    public Awb getMawb(Long id) {
        List<Awb> awb = awbRepository.findByConsolidationId(id);
        if(awb != null && !awb.isEmpty()) {
            return awb.get(0);
        }
        return null;
    }
    public KafkaResponse getKafkaResponseForAirMessaging(Object data) {
        KafkaResponse kafkaResponse = new KafkaResponse();
        kafkaResponse.setEvent(Constants.ORIGINAL_PRINT);
        kafkaResponse.setData(data);
        return kafkaResponse;
    }
    @Override
    public int updateAirMessageStatus(UUID guid, String airMessageStatus) {
        return awbRepository.updateAirMessageStatus(guid, airMessageStatus);
    }

    @Override
    public int updateLinkedHawbAirMessageStatus(UUID guid, String airMessageStatus) {
        return awbRepository.updateLinkedHawbAirMessageStatus(guid, airMessageStatus);
    }

    @Override
    public int updateAirMessageStatusFromShipmentId(Long id, String airMessageStatus) {
        return awbRepository.updateAirMessageStatusFromShipmentId(id, airMessageStatus);
    }
    @Override
    public int updateAirMessageStatusFromConsolidationId(Long id, String airMessageStatus) {
        return awbRepository.updateAirMessageStatusFromConsolidationId(id, airMessageStatus);
    }
    public int updatePrintTypeFromConsolidationId(Long id, String printType){
        return awbRepository.updatePrintTypeFromConsolidationId(id, printType);
    }
    public int updatePrintTypeFromShipmentId(Long id, String printType) {
        return awbRepository.updatePrintTypeFromShipmentId(id, printType);
    }

    @Override
    public int updateUserDetails(UUID guid, String userDisplayName, String userMailId) {
        return awbRepository.updateUserDetails(guid, userDisplayName, userMailId);
    }

    @Override
    public List<Awb> findAllLinkedAwbs(UUID guid) {
        Optional<Awb> awb = Optional.ofNullable(this.findAwbByGuidByQuery(guid));
        if(awb.isPresent()){
            if(awb.get().getShipmentId() != null) {
                var consoleShipMapping = consoleShipmentMappingDao.findByShipmentIdByQuery(awb.get().getShipmentId());
                if(consoleShipMapping == null || consoleShipMapping.isEmpty()) {
                    return Collections.emptyList();
                } else {
                    List<Awb> response = new ArrayList<>();
                    var consoleId = consoleShipMapping.get(0).getConsolidationId();
                    response.addAll(findByConsolidationIdByQuery(consoleId));
                    var consoleShipMappingList = consoleShipmentMappingDao.findByConsolidationIdByQuery(consoleId);
                    if(consoleShipMappingList != null && !consoleShipMappingList.isEmpty()){
                        consoleShipMappingList.forEach(x -> response.addAll(findByShipmentIdByQuery(x.getShipmentId())));
                    }
                    return response;
                }
            } else if(awb.get().getConsolidationId() != null) {
                List<Awb> response = new ArrayList<>();
                var consoleId = awb.get().getConsolidationId();
                response.addAll(findByConsolidationIdByQuery(consoleId));
                var consoleShipMappingList = consoleShipmentMappingDao.findByConsolidationIdByQuery(consoleId);
                if(consoleShipMappingList != null && !consoleShipMappingList.isEmpty()){
                    consoleShipMappingList.forEach(x -> response.addAll(findByShipmentIdByQuery(x.getShipmentId())));
                }
                return response;
            }
        }
        return Collections.emptyList();
    }

    @Override
    public Awb findAwbByGuidByQuery(UUID guid) {
        return awbRepository.findAwbByGuidByQuery(guid);
    }

}
