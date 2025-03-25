package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.awb.AwbSpecialHandlingCodesMappingInfo;
import com.dpw.runner.shipment.services.dto.response.AwbAirMessagingResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.entity.enums.PrintType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.kafka.dto.AwbShipConsoleDto;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.repository.interfaces.IAwbRepository;
import com.dpw.runner.shipment.services.service.interfaces.IKafkaAsyncService;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.AwbUtility;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.Pair;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;
import javax.persistence.EntityManager;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.commons.constants.Constants.EXEMPTION_CARGO;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;

@Repository
@Slf4j
public class AwbDao implements IAwbDao {
    @Autowired
    private IAwbRepository awbRepository;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    IMawbHawbLinkDao mawbHawbLinkDao;
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

    @Autowired
    private EntityManager entityManager;
    private V1ServiceUtil v1ServiceUtil;
    private ModelMapper modelMapper;
    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private IKafkaAsyncService kafkaAsyncService;

    @Autowired
    public void setV1ServiceUtil(V1ServiceUtil v1ServiceUtil) {
        this.v1ServiceUtil = v1ServiceUtil;
    }
    @Autowired
    public void setModelMapper(ModelMapper modelMapper) {
        this.modelMapper = modelMapper;
    }

    @Override
    public Awb save(Awb awbShipmentInfo) throws RunnerException {
        boolean isCreate = awbShipmentInfo.getId() == null;
        applyValidations(awbShipmentInfo);
        Awb awb = awbRepository.save(awbShipmentInfo);
        kafkaAsyncService.pushToKafkaAwb(awb, isCreate);
        return awb;
    }

    @Override
    public Page<Awb> findAll(Specification<Awb> spec, Pageable pageable) {
        return awbRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Awb> findById(Long id) {
        return awbRepository.findAwbByIds(Arrays.asList(id)).stream().findFirst();
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
    public List<Awb> findByConsolidationId(Long consolidationId) {
        return awbRepository.findByConsolidationIdByQuery(consolidationId);
    }

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
            kafkaAsyncService.pushToKafkaAwb(awb, false);
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
        if(!errors.isEmpty())
            throw new RunnerException(errors.toString());
    }

    @Override
    public void airMessagingIntegration(Long id, String reportType, Boolean fromShipment, boolean includeCSD) {
        try {
            if (Objects.equals(reportType, ReportConstants.MAWB)) {
                if (!Boolean.TRUE.equals(fromShipment)) {
                    Awb awb = getMawb(id);
                    getHawbPacks(awb);
                    if (awb.getConsolidationId() != null) {
                        processAirMessagingConsolidation(includeCSD, awb);
                    }
                } else {
                    Awb awb = getHawb(id);
                    if (awb.getShipmentId() != null) {
                        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(awb.getShipmentId());
                        if (shipmentDetails.isPresent()) {
                            this.pushToKafkaForAirMessaging(awb, shipmentDetails.get(), null, null, false, null, includeCSD);
                            // AirMessageSent flag set to SENT
                            this.updateAirMessageStatus(awb.getGuid(), AwbStatus.AIR_MESSAGE_SENT.name());
                            this.updateUserDetails(awb.getGuid(), UserContext.getUser().DisplayName, UserContext.getUser().Email);
                            this.createAirMessagingEvents(shipmentDetails.get().getId(), Constants.SHIPMENT, EventConstants.FWB_EVENT_CODE, "FWB sent", shipmentDetails.get().getTenantId());
                        }

                    }
                }
            }
        } catch (Exception e)
        {
            log.error("Error pushing awb to kafka for entityId: {} with error: {}", id, e.getMessage());
        }
    }

    private void processAirMessagingConsolidation(boolean includeCSD, Awb awb) throws RunnerException {
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(awb.getConsolidationId());
        if (consolidationDetails.isPresent()) {
            this.pushToKafkaForAirMessaging(awb, null, consolidationDetails.get(), null, false, null, includeCSD);
            // AirMessageSent flag set to SENT
            this.updateAirMessageStatus(awb.getGuid(), AwbStatus.AIR_MESSAGE_SENT.name());
            this.updateLinkedHawbAirMessageStatus(awb.getGuid(), AwbStatus.AIR_MESSAGE_SENT.name());
            this.updateUserDetails(awb.getGuid(), UserContext.getUser().DisplayName, UserContext.getUser().Email);
            this.createAirMessagingEvents(consolidationDetails.get().getId(), Constants.CONSOLIDATION, EventConstants.FWB_FZB_EVENT_CODE, "FWB&FZB sent", consolidationDetails.get().getTenantId());
            var v1Map = v1ServiceUtil.getTenantDetails(consolidationDetails.get().getShipmentsList().stream().map(ShipmentDetails::getTenantId).toList());
            for (ShipmentDetails ship : consolidationDetails.get().getShipmentsList()) {
                Awb shipAwb = getHawb(ship.getId());
                this.pushToKafkaForAirMessaging(shipAwb, ship, null, v1Map.containsKey(ship.getTenantId()) ? modelMapper.map(v1Map.get(ship.getTenantId()), TenantModel.class) : null, !Objects.equals(consolidationDetails.get().getTenantId(), shipAwb.getTenantId()), awb, includeCSD);
                // AirMessageSent flag set to SENT
                this.updateAirMessageStatus(shipAwb.getGuid(), AwbStatus.AIR_MESSAGE_SENT.name());
                this.updateUserDetails(shipAwb.getGuid(), UserContext.getUser().DisplayName, UserContext.getUser().Email);
                this.createAirMessagingEvents(ship.getId(), Constants.SHIPMENT, EventConstants.FWB_FZB_EVENT_CODE, "FWB&FZB sent", ship.getTenantId());
            }
        }
    }

    private void createAirMessagingEvents(Long entityId, String entityType, String eventCode, String description, Integer tenantId) {
        eventDao.createEventForAirMessagingStatus(UUID.randomUUID(), entityId, entityType, eventCode, description, LocalDateTime.now(), LocalDateTime.now(),
                Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER,
                tenantId, null, LocalDateTime.now(), LocalDateTime.now());
    }

    public void pushToKafkaForAirMessaging(Awb awb, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, TenantModel tenantModel, boolean overrideData, Awb masterAwb, boolean includeCSD) throws RunnerException {
        if(awb.getTenantId() == null)
            awb.setTenantId(TenantContext.getCurrentTenant());
        AwbAirMessagingResponse awbResponse;
        if(shipmentDetails != null) {
            awbResponse = awbUtility.createAirMessagingRequestForShipment(awb, shipmentDetails, tenantModel, masterAwb);
            awbResponse.setAwbKafkaEntity(jsonHelper.convertValue(shipmentDetails, AwbShipConsoleDto.class));
        } else if (consolidationDetails != null) {
            awbResponse = awbUtility.createAirMessagingRequestForConsole(awb, consolidationDetails);
            awbResponse.setAwbKafkaEntity(jsonHelper.convertValue(consolidationDetails, AwbShipConsoleDto.class));
        } else {
            return;
        }
        awbResponse.getMeta().setIncludeCSD(includeCSD);
        awbUtility.overrideInfoForCoLoadShipment(awbResponse, overrideData);
        KafkaResponse kafkaResponse = getKafkaResponseForAirMessaging(awbResponse);
        log.info("RequestId: {} || Event: {} || message: {}", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.AIR_MESSAGING_EVENT_PUSH_TO_KAFKA, jsonHelper.convertToJson(kafkaResponse));
        producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, UUID.randomUUID().toString());
    }

    public Awb getHawb(Long id) {
        List<Awb> awb = awbRepository.findByShipmentIdByQuery(id);
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
    public int updatePrintTypeFromConsolidationId(Long id, String printType, Boolean isOriginal, LocalDateTime printedAt) {
        var awbList = findByConsolidationId(id);
        if (!awbList.isEmpty() && !Objects.equals(awbList.get(0).getPrintType(), PrintType.ORIGINAL_PRINTED))
            return awbRepository.updatePrintTypeFromConsolidationId(id, printType);
        else if (!awbList.isEmpty() && Boolean.TRUE.equals(isOriginal)) {
            awbRepository.updatePrintDateFromConsolidationId(id, printedAt);
            return awbRepository.updatePrintTypeFromConsolidationId(id, printType);
        }
        return 0;
    }
    public int updatePrintTypeFromShipmentId(Long id, String printType, Boolean isOriginal, LocalDateTime printedAt) {
        var awbList = findByShipmentId(id);
        if (!awbList.isEmpty() && !Objects.equals(awbList.get(0).getPrintType(), PrintType.ORIGINAL_PRINTED))
            return awbRepository.updatePrintTypeFromShipmentId(id, printType);
        else if (!awbList.isEmpty() && Boolean.TRUE.equals(isOriginal)) {
            awbRepository.updatePrintDateFromShipmentId(id, printedAt);
            return awbRepository.updatePrintTypeFromShipmentId(id, printType);
        }
        return 0;
    }

    @Override
    @Transactional
    public void updateAwbPrintInformation(Long shipmentId, Long consolidationId, PrintType printType, Boolean isOriginal, LocalDateTime printedAt) {
        Awb awb = null;
        List<Awb> awbList;
        if(shipmentId != null) {
            awbList = findByShipmentId(shipmentId);
        }
        else {
            awbList = findByConsolidationId(consolidationId);
        }
        awb = !awbList.isEmpty() ? awbList.get(0) : null;

        if(awb != null) {
            // if not original printed
            if(!Objects.equals(awb.getPrintType(), PrintType.ORIGINAL_PRINTED))
                awb.setPrintType(printType);
            if(Boolean.TRUE.equals(isOriginal)) {
                awb.setOriginalPrintedAt(printedAt);
                commonUtils.checkForMandatoryHsCodeForUAE(awb);
            }
            try {
                save(awb);
            } catch (Exception e) {
                log.error("Exception occurred while saving print information for awb : {}", e.getMessage());
            }
        }
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
                return processLinkedShipment(awb.get());
            } else if(awb.get().getConsolidationId() != null) {
                var consoleId = awb.get().getConsolidationId();
                List<Awb> response = new ArrayList<>(findByConsolidationIdByQuery(consoleId));
                var consoleShipMappingList = consoleShipmentMappingDao.findByConsolidationIdByQuery(consoleId);
                if(consoleShipMappingList != null && !consoleShipMappingList.isEmpty()){
                    response.addAll(findByShipmentIdsByQuery(consoleShipMappingList.stream().map(ConsoleShipmentMapping::getShipmentId).toList()));
                }
                return response;
            }
        }
        return Collections.emptyList();
    }

    private List<Awb> processLinkedShipment(Awb awb) {
        var consoleShipMapping = consoleShipmentMappingDao.findByShipmentIdByQuery(awb.getShipmentId());
        if(consoleShipMapping == null || consoleShipMapping.isEmpty()) {
            return Collections.emptyList();
        } else {
            var consoleId = consoleShipMapping.get(0).getConsolidationId();
            List<Awb> response = new ArrayList<>(findByConsolidationIdByQuery(consoleId));
            var consoleShipMappingList = consoleShipmentMappingDao.findByConsolidationIdByQuery(consoleId);
            if(consoleShipMappingList != null && !consoleShipMappingList.isEmpty()){
                response.addAll(findByShipmentIdsByQuery(consoleShipMappingList.stream().map(ConsoleShipmentMapping::getShipmentId).toList()));
            }
            return response;
        }
    }

    @Override
    public Awb findAwbByGuidByQuery(UUID guid) {
        return awbRepository.findAwbByGuidByQuery(guid);
    }

    private void getAwbSphEntity(String eFreightStatus, String securityStatus, Long id, Awb awb) {
        awb.setAwbSpecialHandlingCodesMappings(null);
        List<AwbSpecialHandlingCodesMappingInfo> sphs = new ArrayList<>();
        if(!IsStringNullOrEmpty(eFreightStatus) && !Constants.NON.equals(eFreightStatus)) {
            AwbSpecialHandlingCodesMappingInfo sph = AwbSpecialHandlingCodesMappingInfo.builder()
                    .shcId(eFreightStatus)
                    .entityId(id)
                    .build();

            sph.setEntityType(awb.getAwbShipmentInfo().getEntityType());
            sphs.add(sph);
        }
        if(!IsStringNullOrEmpty(securityStatus) && !Constants.INSECURE.equals(securityStatus)) {
            if(securityStatus.equalsIgnoreCase(EXEMPTION_CARGO) || securityStatus.equalsIgnoreCase(ReportConstants.EXEMPTION_CARGO))
                securityStatus = Constants.SPX;
            AwbSpecialHandlingCodesMappingInfo sph = AwbSpecialHandlingCodesMappingInfo.builder()
                    .shcId(securityStatus)
                    .entityId(id)
                    .build();

            sph.setEntityType(awb.getAwbShipmentInfo().getEntityType());
            sphs.add(sph);
        }
        if(!sphs.isEmpty())
            awb.setAwbSpecialHandlingCodesMappings(sphs);
    }

    @Override
    public void updatedAwbInformationEvent(Object newEntity, Object oldEntity) throws RunnerException {
        // fetch Awb
        Awb awb = null;
        AwbSpecialHandlingCodesMappingInfo sph = null;

        if(newEntity instanceof ShipmentDetails shipmentDetails) {
            awb = getAwbFromShipment((ShipmentDetails) oldEntity, shipmentDetails);
            if (awb == null) return;
        }
        else if (newEntity instanceof ConsolidationDetails consolidationDetails) {
            ConsolidationDetails oldConsolidation = (ConsolidationDetails) oldEntity;
            var list = findByConsolidationId(consolidationDetails.getId());
            awb = !list.isEmpty() ? list.get(0) : null;
            if(Objects.isNull(awb))
                return;

            if(!Objects.equals(consolidationDetails.getEfreightStatus(), oldConsolidation.getEfreightStatus()) ||
                    !Objects.equals(consolidationDetails.getSecurityStatus(), oldConsolidation.getSecurityStatus())) {
                getAwbSphEntity(consolidationDetails.getEfreightStatus(), consolidationDetails.getSecurityStatus(), consolidationDetails.getId(), awb);
                awb.setAirMessageResubmitted(false);
            }
            if(!Objects.equals(consolidationDetails.getSci(), oldConsolidation.getSci())){
                awb.getAwbCargoInfo().setSci(consolidationDetails.getSci());
                awb.setAirMessageResubmitted(false);
            }
        }

        if(Objects.isNull(awb))
            return;

        save(awb);

    }

    private Awb getAwbFromShipment(ShipmentDetails oldEntity, ShipmentDetails shipmentDetails) throws RunnerException {
        Awb awb;
        ShipmentDetails oldShipment = oldEntity;
        var list = findByShipmentId(shipmentDetails.getId());
        awb = !list.isEmpty() ? list.get(0) : null;
        if(Objects.isNull(awb))
            return null;

        if(!Objects.equals(shipmentDetails.getAdditionalDetails().getEfreightStatus(), oldShipment.getAdditionalDetails().getEfreightStatus()) ||
                !Objects.equals(shipmentDetails.getSecurityStatus(), oldShipment.getSecurityStatus())) {
            getAwbSphEntity(shipmentDetails.getAdditionalDetails().getEfreightStatus(), shipmentDetails.getSecurityStatus(), shipmentDetails.getId(), awb);
            awb.setAirMessageResubmitted(false);
        }
        if(!Objects.equals(shipmentDetails.getAdditionalDetails().getSci(), oldShipment.getAdditionalDetails().getSci())){
            awb.getAwbCargoInfo().setSci(shipmentDetails.getAdditionalDetails().getSci());
            awb.setAirMessageResubmitted(false);
            updateSciFieldFromHawb(awb, null, false, awb.getId());
        }
        return awb;
    }

    public void updateSciFieldFromHawb(Awb awb, Awb oldEntity, boolean isReset, Long awbId) throws RunnerException {
        if(awb.getAwbShipmentInfo().getEntityType().equals(Constants.HAWB) && Objects.equals(awb.getAwbCargoInfo().getSci(), AwbConstants.T1)){
            List<MawbHawbLink> mawbHawbLinks = mawbHawbLinkDao.findByHawbId(awb.getId());
            if(mawbHawbLinks != null && !mawbHawbLinks.isEmpty()){
                Long mawbId = mawbHawbLinks.get(0).getMawbId();
                Optional<Awb> mawb = findById(mawbId);
                if(mawb.isPresent() && !Objects.equals(mawb.get().getAirMessageStatus(), AwbStatus.AWB_FSU_LOCKED) && !Objects.equals(mawb.get().getAwbCargoInfo().getSci(), awb.getAwbCargoInfo().getSci())){
                    mawb.get().getAwbCargoInfo().setSci(awb.getAwbCargoInfo().getSci());
                    mawb.get().setAirMessageResubmitted(false);
                    save(mawb.get());
                }
            }
        } else if(awb.getAwbShipmentInfo().getEntityType().equals(Constants.HAWB) && !Objects.equals(awb.getAwbCargoInfo().getSci(), AwbConstants.T1) && ((oldEntity != null && Objects.equals(oldEntity.getAwbCargoInfo().getSci(), AwbConstants.T1)) || isReset || oldEntity == null)){
            processNonT1SciField(awb, awbId);
        }
    }

    private void processNonT1SciField(Awb awb, Long awbId) throws RunnerException {
        List<MawbHawbLink> mawbHawbLinks = mawbHawbLinkDao.findByHawbId(awb.getId());
        if(mawbHawbLinks != null && !mawbHawbLinks.isEmpty()){
            Long mawbId = mawbHawbLinks.get(0).getMawbId();
            List<Awb> hawbsList = getLinkedAwbFromMawb(mawbId);
            boolean updateSci = false;
            if(hawbsList != null && !hawbsList.isEmpty()){
                Optional<Awb> hawb = hawbsList.stream().filter(x -> Objects.equals(x.getAwbCargoInfo().getSci(), AwbConstants.T1) && !Objects.equals(x.getId(), awbId)).findAny();
                if(hawb.isEmpty()){
                    updateSci = true;
                }
            }
            if(updateSci) {
                Optional<Awb> mawb = findById(mawbId);
                if (mawb.isPresent() && !Objects.equals(mawb.get().getAirMessageStatus(), AwbStatus.AWB_FSU_LOCKED) && Objects.equals(mawb.get().getAwbCargoInfo().getSci(), AwbConstants.T1)) {
                    mawb.get().getAwbCargoInfo().setSci(null);
                    mawb.get().setAirMessageResubmitted(false);
                    save(mawb.get());
                }
            }
        }
    }

    @Override
    public List<Awb> getLinkedAwbFromMawb(Long mawbId) {
        List<MawbHawbLink> mawbHawbLinks = mawbHawbLinkDao.findByMawbId(mawbId);

        // Fetch all the awb records with the mapped hawbId
        ListCommonRequest listCommonRequest = CommonUtils.constructListCommonRequest("id", mawbHawbLinks.stream().map(MawbHawbLink::getHawbId).toList(), "IN");
        com.nimbusds.jose.util.Pair<Specification<Awb>, Pageable> pair = fetchData(listCommonRequest, Awb.class);
        Page<Awb> page = findAll(pair.getLeft(), pair.getRight());

        List<Awb> linkedHawb = new ArrayList<>();
        if(!page.isEmpty())
            linkedHawb = page.getContent();

        return linkedHawb;
    }

    @Override
    public List<Awb> findByIds(List<Long> id) {
        return awbRepository.findAwbByIds(id);
    }

    @Override
    public List<Awb> findAwbByAwbNumbers(List<String> awbNumbers) {
        return awbRepository.findAwbByAwbNumbers(awbNumbers);
    }

    @Override
    public void validateAirMessaging(Long id) throws RunnerException {
        List<Awb> awb = findByConsolidationId(id);
        if(awb != null && !awb.isEmpty() && awb.get(0).getAirMessageStatus() != null && (Objects.equals(awb.get(0).getAirMessageStatus(), AwbStatus.AIR_MESSAGE_SENT) ||
                Objects.equals(awb.get(0).getAirMessageStatus(), AwbStatus.AIR_MESSAGE_FAILED) || Objects.equals(awb.get(0).getAirMessageStatus(), AwbStatus.AIR_MESSAGE_SUCCESS))) {
            throw new RunnerException("FWB & FZB are already submitted and further modifications are prohibited for given console.");
        }
    }

    private void getHawbPacks(Awb awb) {
        List<Awb> linkedHouses = getLinkedAwbFromMawb(awb.getId());
        awb.setAwbPackingInfo(new ArrayList<>());

        linkedHouses.stream()
                .filter(house -> Objects.nonNull(house.getAwbPackingInfo()))
                .forEach(house -> awb.getAwbPackingInfo().addAll(house.getAwbPackingInfo()));
    }
}
