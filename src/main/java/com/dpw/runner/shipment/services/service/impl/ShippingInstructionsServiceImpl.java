package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.interfaces.IBridgeServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerPackageSiPayload;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShippingInstructionContainerWarningResponse;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.ShippingInstructionRequest;
import com.dpw.runner.shipment.services.dto.response.FieldClassDto;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ReferenceNumberResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionInttraRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.FlowType;
import com.dpw.runner.shipment.services.entity.enums.PayerType;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionStatus;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionType;
import com.dpw.runner.shipment.services.entity.enums.SourceSystem;
import com.dpw.runner.shipment.services.exception.exceptions.GenericException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.helpers.ShippingInstructionMasterDataHelper;
import com.dpw.runner.shipment.services.kafka.dto.inttra.ShippingInstructionEventDto;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.projection.CarrierBookingInfoProjection;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IShippingInstructionsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.FieldUtils;
import com.dpw.runner.shipment.services.utils.IntraCommonKafkaHelper;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingInttraUtil;
import com.dpw.runner.shipment.services.utils.v3.ShippingInstructionUtil;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.transaction.Transactional;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_REQUEST_EMPTY_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_REQUEST_NULL_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_RESPONSE_SUCCESS;
import static com.dpw.runner.shipment.services.commons.constants.ShippingInstructionsConstants.*;
import static com.dpw.runner.shipment.services.entity.enums.ShippingInstructionStatus.Requested;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class ShippingInstructionsServiceImpl implements IShippingInstructionsService {

    @Autowired
    private IShippingInstructionDao repository;

    @Autowired
    ICarrierBookingDao carrierBookingDao;

    @Autowired
    IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    IPackingV3Service packingV3Service;

    @Autowired
    IVerifiedGrossMassDao vgmDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    @Qualifier("executorServiceMasterData")
    ExecutorService executorServiceMasterData;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    ShippingInstructionMasterDataHelper shippingInstructionMasterDataHelper;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    IntraCommonKafkaHelper kafkaHelper;

    @Autowired
    ShippingInstructionUtil shippingInstructionUtil;

    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private CarrierBookingInttraUtil carrierBookingInttraUtil;

    @Autowired
    private IBridgeServiceAdapter bridgeServiceAdapter;
    @Autowired
    private INotificationService notificationService;


    @Override
    @Transactional
    public ShippingInstructionResponse createShippingInstruction(ShippingInstructionRequest info) {
        ShippingInstruction shippingInstruction = jsonHelper.convertValue(info, ShippingInstruction.class);
        shippingInstruction = validateFetchAndSetSI(shippingInstruction, true);
        shippingInstruction.setStatus(ShippingInstructionStatus.Draft);
        shippingInstruction.setInternalEmails(carrierBookingInttraUtil.parseEmailListToString(info.getInternalEmailsList()));
        shippingInstruction.setExternalEmails(carrierBookingInttraUtil.parseEmailListToString(info.getExternalEmailsList()));
        shippingInstruction.setCreateByUserEmail(UserContext.getUser().getEmail());
        shippingInstruction.setSubmitByUserEmail(UserContext.getUser().getEmail());
        List<Parties> additionalPartiesList = shippingInstruction.getAdditionalParties();
        ShippingInstruction savedInfo = repository.save(shippingInstruction);
        if (additionalPartiesList != null) {
            List<Parties> updatedParties = partiesDao.saveEntityFromOtherEntity(commonUtils.convertToEntityList(additionalPartiesList,
                    Parties.class, false), shippingInstruction.getId(), SHIPPING_INSTRUCTION_ADDITIONAL_PARTIES);
            shippingInstruction.setAdditionalParties(updatedParties);
        }

        return jsonHelper.convertValue(savedInfo, ShippingInstructionResponse.class);
    }

    private ShippingInstruction validateFetchAndSetSI(ShippingInstruction shippingInstruction, boolean isCreate) {
        validateSIRequest(shippingInstruction);
        ShippingInstructionResponseMapper mapper = new ShippingInstructionResponseMapper();
        mapper.setShippingInstruction(shippingInstruction);
        if (EntityType.CONSOLIDATION == shippingInstruction.getEntityType()) {
            List<CarrierBookingInfoProjection> cbInfoProjection = repository.findBookingByConsolId(shippingInstruction.getEntityNumber());
            if (!cbInfoProjection.isEmpty()) {
                throw new ValidationException("SI creation not allowed. Consolidation linked with a Booking already!!");
            }
        }
        populateReadOnlyFields(mapper, isCreate);
        return mapper.getShippingInstruction();
    }

    private void setDefaultValues(EntityType type, Long entityId,
                                  ShippingInstructionResponseMapper response, boolean isCreate) {
        ConsolidationDetails consolidationDetails;
        ShippingInstruction shippingInstruction = response.getShippingInstruction() != null ? response.getShippingInstruction() : new ShippingInstruction();

        if (EntityType.CARRIER_BOOKING == type) {
            Optional<CarrierBooking> carrierBooking = carrierBookingDao.findById(entityId);
            if (carrierBooking.isEmpty()) {
                throw new ValidationException("Invalid entity id");
            }
            populateHeaderSection(shippingInstruction, carrierBooking.get());
            populateSailingInformationFromCarrierBooking(shippingInstruction, carrierBooking.get());
            consolidationDetails = carrierBookingInttraUtil.getConsolidationDetail(carrierBooking.get().getEntityId());
            setReferenceNumber(shippingInstruction, carrierBooking.get());
            response.setBookingStatus(carrierBooking.get().getStatus().name());
            setPartiesNumber(shippingInstruction, carrierBooking.get());

        } else if (EntityType.CONSOLIDATION == type) {
            consolidationDetails = carrierBookingInttraUtil.getConsolidationDetail(entityId);
            response.setBookingStatus(consolidationDetails.getBookingStatus());
            shippingInstruction.setReferenceNumbersList(getReferenceNumberResponses(consolidationDetails));
        } else {
            throw new ValidationException(INVALID_ENTITY_TYPE);
        }
        shippingInstruction.setEntityId(entityId);
        setEntityNumber(shippingInstruction);
        fillDetailsFromConsol(shippingInstruction, consolidationDetails);
        shippingInstruction.setStatus(ShippingInstructionStatus.Draft);
        if (isCreate) {
            setPackingAndContainerDetails(consolidationDetails, shippingInstruction);
        }
        response.setShippingInstruction(shippingInstruction);
    }

    private void setReferenceNumber(ShippingInstruction shippingInstruction, CarrierBooking carrierBooking) {
        if (!Objects.nonNull(carrierBooking.getReferenceNumbersList())) {
            log.debug("Reference list empty for booking {}", carrierBooking.getId());
            return;
        }
        Set<ReferenceNumbers> referenceNumbersList = shippingInstruction.getReferenceNumbersList()
                != null ? new HashSet<>(shippingInstruction.getReferenceNumbersList()) : new HashSet<>();
        for (ReferenceNumbers referenceNumbers : carrierBooking.getReferenceNumbersList()) {
            referenceNumbers.setId(null);
            referenceNumbers.setGuid(null);
            referenceNumbersList.add(referenceNumbers);
        }

        shippingInstruction.setReferenceNumbersList(new ArrayList<>(referenceNumbersList));
    }

    private void setPartiesNumber(ShippingInstruction shippingInstruction, CarrierBooking carrierBooking) {
        if (Objects.nonNull(carrierBooking.getConsignee())) {
            Parties partiesResponse = jsonHelper.convertValue(carrierBooking.getConsignee(), Parties.class);
            partiesResponse.setId(null);
            partiesResponse.setGuid(null);
            shippingInstruction.setConsignee(partiesResponse);
        }
        if (Objects.nonNull(carrierBooking.getShipper())) {
            Parties partiesResponse = jsonHelper.convertValue(carrierBooking.getShipper(), Parties.class);
            partiesResponse.setId(null);
            partiesResponse.setGuid(null);
            shippingInstruction.setShipper(partiesResponse);
        }
        if (Objects.nonNull(carrierBooking.getForwardingAgent())) {
            Parties partiesResponse = jsonHelper.convertValue(carrierBooking.getForwardingAgent(), Parties.class);
            partiesResponse.setId(null);
            partiesResponse.setGuid(null);
            shippingInstruction.setForwardingAgent(partiesResponse);
        }
        if (Objects.nonNull(carrierBooking.getContract())) {
            Parties partiesResponse = jsonHelper.convertValue(carrierBooking.getContract(), Parties.class);
            partiesResponse.setId(null);
            partiesResponse.setGuid(null);
            shippingInstruction.setContract(partiesResponse);
        }
        if (Objects.nonNull(carrierBooking.getRequester())) {
            Parties partiesResponse = jsonHelper.convertValue(carrierBooking.getRequester(), Parties.class);
            partiesResponse.setId(null);
            partiesResponse.setGuid(null);
            shippingInstruction.setRequestor(partiesResponse);
        }
    }

    @NotNull
    private static List<ReferenceNumbers> getReferenceNumberResponses(ConsolidationDetails consolidationDetails) {
        List<ReferenceNumbers> referenceNumbersList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(consolidationDetails.getReferenceNumbersList())) {
            for (ReferenceNumbers referenceNumbers : consolidationDetails.getReferenceNumbersList()) {
                ReferenceNumberResponse referenceNumberResponse = new ReferenceNumberResponse();
                referenceNumberResponse.setType(referenceNumbers.getType());
                referenceNumberResponse.setReferenceNumber(referenceNumbers.getReferenceNumber());
                referenceNumbersList.add(referenceNumbers);
            }
        }
        return referenceNumbersList;
    }

    public void setPackingAndContainerDetails(ConsolidationDetails consolidationDetails, ShippingInstruction shippingInstruction) {
        try {
            List<Packing> packingList = packingV3Service.getPackingsByConsolidationId(consolidationDetails.getId());
            List<Containers> containersList = consolidationDetails.getContainersList();
            Map<Long, Containers> containersMap = new HashMap<>();
            if (!CollectionUtils.isEmpty(containersList)) {
                containersMap = containersList.stream()
                        .collect(Collectors.toMap(
                                Containers::getId,
                                Function.identity(),
                                (existing, replacement) -> existing
                        ));
            }
            shippingInstruction.setCommonPackagesList(setCommonPackages(packingList, containersMap));
            shippingInstruction.setContainersList(setCommonContainers(containersList));
        } catch (Exception e) {
            log.error("Error in pulling container data from consolidation");
        }
    }

    private List<CommonContainers> setCommonContainers(List<Containers> containersList) {
        List<CommonContainers> commonContainersList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(containersList)) {
            for (Containers containers : containersList) {
                CommonContainers commonContainers = new CommonContainers();
                commonContainers.setContainerCode(containers.getContainerCode());
                commonContainers.setContainerNo(containers.getContainerNumber());
                commonContainers.setPacks(StringUtility.isNotEmpty(containers.getPacks()) ? Integer.parseInt(containers.getPacks()) : null);
                commonContainers.setPacksUnit(containers.getPacksType());
                commonContainers.setHsCode(containers.getHsCode());
                commonContainers.setCommodityCode(containers.getCommodityCode());
                commonContainers.setCommodityGroup(containers.getCommodityGroup());
                commonContainers.setMarksNums(containers.getMarksNums());
                commonContainers.setGoodsDescription(containers.getDescriptionOfGoods());
                commonContainers.setGrossWeight(containers.getGrossWeight());
                commonContainers.setGrossWeightUnit(containers.getGrossWeightUnit());
                commonContainers.setVolume(containers.getGrossVolume());
                commonContainers.setVolumeUnit(containers.getGrossVolumeUnit());
                commonContainers.setNetWeight(containers.getNetWeight());
                commonContainers.setNetWeightUnit(containers.getNetWeightUnit());
                commonContainers.setTareWeight(containers.getTareWeight());
                commonContainers.setTareWeightUnit(containers.getTareWeightUnit());
                commonContainers.setCustomsSealNumber(containers.getCustomsSealNumber());
                commonContainers.setShipperSealNumber(containers.getShipperSealNumber());
                commonContainers.setVeterinarySealNumber(containers.getVeterinarySealNumber());
                commonContainers.setContainerRefGuid(containers.getGuid());
                commonContainersList.add(commonContainers);
            }
        }
        return commonContainersList;
    }

    private List<CommonPackages> setCommonPackages(List<Packing> packingList, Map<Long, Containers> containersMap) {

        List<CommonPackages> commonPackagesList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(packingList)) {
            for (Packing packing : packingList) {
                CommonPackages commonPackages = new CommonPackages();
                commonPackages.setPacks(StringUtility.isNotEmpty(packing.getPacks()) ? Integer.parseInt(packing.getPacks()) : null);
                commonPackages.setPacksUnit(packing.getPacksType());
                commonPackages.setHsCode(packing.getHSCode());
                commonPackages.setGrossWeight(packing.getWeight());
                commonPackages.setGrossWeightUnit(packing.getWeightUnit());
                commonPackages.setVolume(packing.getVolume());
                commonPackages.setVolumeUnit(packing.getVolumeUnit());
                commonPackages.setGoodsDescription(packing.getGoodsDescription());
                commonPackages.setCommodityCode(packing.getCommodity());
                commonPackages.setCommodityGroup(packing.getCommodityGroup());
                commonPackages.setMarksnNums(packing.getMarksnNums());
                commonPackages.setPackingRefGuid(packing.getGuid());
                Containers containers = containersMap.get(packing.getContainerId());
                if (Objects.nonNull(containers)) {
                    commonPackages.setContainerNo(containers.getContainerNumber());
                }
                commonPackagesList.add(commonPackages);
            }
        }
        return commonPackagesList;
    }

    private void populateSailingInformationFromCarrierBooking(ShippingInstruction shippingInstruction, CarrierBooking carrierBooking) {
        if (Objects.nonNull(shippingInstruction.getSailingInformation()) && Objects.nonNull(carrierBooking.getSailingInformation())) {
            shippingInstruction.getSailingInformation().setCarrierReceiptPlace(carrierBooking.getSailingInformation().getCarrierReceiptPlace());
            shippingInstruction.getSailingInformation().setPol(carrierBooking.getSailingInformation().getPol());
            shippingInstruction.getSailingInformation().setPod(carrierBooking.getSailingInformation().getPod());
            shippingInstruction.getSailingInformation().setCarrierDeliveryPlace(carrierBooking.getSailingInformation().getCarrierDeliveryPlace());
            shippingInstruction.getSailingInformation().setCarrier(carrierBooking.getSailingInformation().getCarrier());
            shippingInstruction.getSailingInformation().setShipInstructionCutoff(carrierBooking.getSailingInformation().getShipInstructionCutoff());
            shippingInstruction.getSailingInformation().setVerifiedGrossMassCutoff(carrierBooking.getSailingInformation().getVerifiedGrossMassCutoff());
        }
    }

    public ShippingInstructionResponse getShippingInstructionsById(Long id) {
        Optional<ShippingInstruction> shippingInstruction = repository.findById(id);
        if (shippingInstruction.isEmpty()) {
            log.debug("SI is null for Id {}", id);
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShippingInstructionResponseMapper mapper = new ShippingInstructionResponseMapper();
        ShippingInstruction instruction = shippingInstruction.get();
        if (EntityType.CARRIER_BOOKING == instruction.getEntityType()) {
            CarrierBookingInfoProjection projection = repository.findBookingInfoById(instruction.getEntityId());
            if (projection != null) {
                mapper.setBookingStatus(projection.getBookingStatus());
                instruction.setEntityNumber(projection.getBookingStatus());
            }
        } else {
            ConsolidationDetails consolidationDetails = carrierBookingInttraUtil.getConsolidationDetail(instruction.getEntityId());
            shippingInstruction.get().setEntityNumber(consolidationDetails.getConsolidationNumber());
            populateFreightDetails(instruction, consolidationDetails);
            instruction.setReferenceNumbersList(getReferenceNumberResponses(consolidationDetails));
        }
        ShippingInstructionResponse response = jsonHelper.convertValue(instruction, ShippingInstructionResponse.class);
        response.setVgmStatus(getVgmStatus(instruction));
        response.setCrBookingId(instruction.getCarrierBookingNo());
        if (Objects.nonNull(instruction.getPayloadJson())) {
            ContainerPackageSiPayload siPayload = jsonHelper.readFromJson(instruction.getPayloadJson(), ContainerPackageSiPayload.class);
            List<ShippingInstructionContainerWarningResponse> containerWarningResponses
                    = shippingInstructionUtil.compareContainerDetails(instruction.getContainersList(), siPayload.getContainerDetail());
            List<ShippingInstructionContainerWarningResponse> packageWarningResponses
                    = shippingInstructionUtil.comparePackageDetails(instruction.getCommonPackagesList(), siPayload.getPackageDetail());
            response.setContainerDiff(containerWarningResponses);
            response.setPackageDiff(packageWarningResponses);
        }

        if (mapper.getBookingStatus() != null) {
            response.setBookingStatus(mapper.getBookingStatus());
        }
        return response;
    }

    private String getVgmStatus(ShippingInstruction instruction) {
        VerifiedGrossMass vgmEntity = vgmDao.findByEntityIdType(instruction.getEntityType(), instruction.getEntityId());
        if (Objects.nonNull(vgmEntity)) {
            return vgmEntity.getStatus().name();
        }
       return null;
    }

    @Override
    @Transactional
    public ShippingInstructionResponse updateShippingInstructions(ShippingInstructionRequest shippingInstructionRequest) {
        Optional<ShippingInstruction> shippingInstructionEntity = repository.findById(shippingInstructionRequest.getId());
        if (shippingInstructionEntity.isEmpty()) {
            throw new ValidationException("Invalid shipping instruction id");
        }

        ShippingInstruction shippingInstruction = jsonHelper.convertValue(shippingInstructionRequest, ShippingInstruction.class);
        validateSIRequest(shippingInstruction);
        shippingInstruction.setCreateByUserEmail(shippingInstructionEntity.get().getCreateByUserEmail());
        shippingInstruction.setSubmitByUserEmail(shippingInstructionEntity.get().getSubmitByUserEmail());
        shippingInstruction.setContainersList(shippingInstructionEntity.get().getContainersList());
        shippingInstruction.setCommonPackagesList(shippingInstructionEntity.get().getCommonPackagesList());
        shippingInstruction.setInternalEmails(carrierBookingInttraUtil.parseEmailListToString(shippingInstructionRequest.getInternalEmailsList()));
        shippingInstruction.setExternalEmails(carrierBookingInttraUtil.parseEmailListToString(shippingInstructionRequest.getExternalEmailsList()));
        ShippingInstructionResponseMapper responseMapper = new ShippingInstructionResponseMapper();
        responseMapper.setShippingInstruction(shippingInstruction);
        populateReadOnlyFields(responseMapper, false);
        ShippingInstruction si = responseMapper.getShippingInstruction();
        ShippingInstruction saved = repository.save(si);
        ShippingInstructionResponse response = jsonHelper.convertValue(saved, ShippingInstructionResponse.class);
        response.setBookingStatus(responseMapper.getBookingStatus());
        return response;
    }

    public void deleteShippingInstructions(Long id) {
        log.info("ShippingInstructionService.delete() called with RequestId: {} and id: {}", LoggerHelper.getRequestIdFromMDC(), id);
        ShippingInstruction shippingInstruction = repository.findById(id)
                .orElseThrow(() -> new ValidationException("SI not found with Id: " + id));
        repository.delete(shippingInstruction);
        log.info("ShippingInstructionService.delete() successful with RequestId: {} and id: {}",
                LoggerHelper.getRequestIdFromMDC(), id);
    }

    public ResponseEntity<IRunnerResponse> getAllMasterData(Long shippingInstId) {
        String responseMsg;
        try {
            Optional<ShippingInstruction> shippingInstructionOpt = repository.findById(shippingInstId);
            if (shippingInstructionOpt.isEmpty()) {
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            ShippingInstruction shippingInstruction = shippingInstructionOpt.get();
            long start = System.currentTimeMillis();
            List<String> includeColumns = FieldUtils.getMasterDataAnnotationFields(List.of(createFieldClassDto(ShippingInstruction.class, null), createFieldClassDto(SailingInformation.class, "sailingInformation.")));
            includeColumns.addAll(LIST_INCLUDE_COLUMNS);
            ShippingInstructionResponse shippingInstructionResponse = (ShippingInstructionResponse) commonUtils.setIncludedFieldsToResponse(shippingInstruction, new HashSet<>(includeColumns), new ShippingInstructionResponse());
            log.info("Total time taken in setting carrier booking details response {}", (System.currentTimeMillis() - start));
            Map<String, Object> response = fetchAllMasterDataByKey(shippingInstructionResponse);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel, boolean getMasterData) {
        ListCommonRequest listCommonRequest = (ListCommonRequest) commonRequestModel.getData();
        if (listCommonRequest == null) {
            log.error(SI_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException(SI_LIST_REQUEST_NULL_ERROR);
        }


        Page<ShippingInstruction> shippingInstructionPage = getShippingInstructions(listCommonRequest);
        log.info(SI_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());

        List<IRunnerResponse> filteredList = convertEntityListToDtoList(shippingInstructionPage.getContent(), getMasterData);

        return ResponseHelper.buildListSuccessResponse(
                filteredList,
                shippingInstructionPage.getTotalPages(),
                shippingInstructionPage.getTotalElements());
    }

    @Override
    public Page<ShippingInstruction> getShippingInstructions(ListCommonRequest listCommonRequest) {
        Pair<Specification<ShippingInstruction>, Pageable> tuple = fetchData(listCommonRequest, ShippingInstruction.class, tableNames);
        return repository.findAll(tuple.getLeft(), tuple.getRight());
    }

    @Override
    public ShippingInstructionResponse getDefaultShippingInstructionValues(EntityType entityType, Long entityId) {
        ShippingInstruction instruction = new ShippingInstruction();
        ShippingInstructionResponseMapper mapper = new ShippingInstructionResponseMapper();
        setDefaultValues(entityType, entityId, mapper, false);
        instruction.setStatus(ShippingInstructionStatus.Draft);
        ShippingInstructionResponse response = jsonHelper.convertValue(mapper.getShippingInstruction(), ShippingInstructionResponse.class);
        response.setBookingStatus(mapper.getBookingStatus());
        return response;
    }

    private void fillDetailsFromConsol(ShippingInstruction shippingInstruction, ConsolidationDetails consolidationDetails) {
        shippingInstruction.setEntityNumber(consolidationDetails.getConsolidationNumber());
        setSailingInfoAndCutoff(shippingInstruction, consolidationDetails);
        populateFreightDetails(shippingInstruction, consolidationDetails);
        shippingInstruction.setCarrierBookingNo(consolidationDetails.getBookingNumber());
    }

    private void setSailingInfoAndCutoff(ShippingInstruction shippingInstruction, ConsolidationDetails consolidationDetails) {
        if (shippingInstruction == null) {
            return; // nothing to populate
        }

        if (shippingInstruction.getSailingInformation() == null) {
            shippingInstruction.setSailingInformation(new SailingInformation());
        }

        if (consolidationDetails != null && consolidationDetails.getCarrierDetails() != null) {

            CarrierDetails carrierDetails = consolidationDetails.getCarrierDetails();
            shippingInstruction.getSailingInformation().setCarrierReceiptPlace(carrierDetails.getOrigin());
            shippingInstruction.getSailingInformation().setPol(carrierDetails.getOriginPort());
            shippingInstruction.getSailingInformation().setPod(carrierDetails.getDestinationPort());
            shippingInstruction.getSailingInformation().setCarrierDeliveryPlace(carrierDetails.getDestination());

        }

        assert consolidationDetails != null;
        shippingInstruction.getSailingInformation().setShipInstructionCutoff(consolidationDetails.getShipInstructionCutoff());
        shippingInstruction.getSailingInformation().setVerifiedGrossMassCutoff(consolidationDetails.getVerifiedGrossMassCutoff());

    }

    private void populateHeaderSection(ShippingInstruction shippingInstruction, CarrierBooking carrierBooking) {
        shippingInstruction.setCarrierBookingNo(carrierBooking.getCarrierBookingNo());
        shippingInstruction.setCarrierBlNo(carrierBooking.getCarrierBlNo());
        setEntityNumber(shippingInstruction);
    }

    private void setEntityNumber(ShippingInstruction shippingInstruction) {
        shippingInstruction.setEntityNumber(String.valueOf(shippingInstruction.getEntityNumber()));
    }

    private void validateSIRequest(ShippingInstruction shippingInstruction) {
        if (Objects.nonNull(shippingInstruction.getNoOfFreightCopies()) && (shippingInstruction.getNoOfFreightCopies() > 100
                || shippingInstruction.getNoOfFreightCopies() < 0)) {
            log.info("Validation failed for number of freight copies for SI id : {}", shippingInstruction.getId());
            throw new ValidationException("Invalid freight copies number!");
        }
        if (Objects.nonNull(shippingInstruction.getNoOfUnFreightCopies()) && (shippingInstruction.getNoOfUnFreightCopies() > 100
                || shippingInstruction.getNoOfUnFreightCopies() < 0)) {
            log.info("Validation failed for getNoOfUnFreightCopies for SI id : {}", shippingInstruction.getId());
            throw new ValidationException("Invalid un freight copies number!");
        }
        if (ShippingInstructionType.EXPRESS.equals(shippingInstruction.getShippingInstructionType())) {
            if (Objects.nonNull(shippingInstruction.getNonNegoFreightCopies()) && (shippingInstruction.getNonNegoFreightCopies() > 100
                    || shippingInstruction.getNonNegoFreightCopies() < 0)) {
                log.info("Validation failed for getNonNegoFreightCopies for SI id : {}", shippingInstruction.getId());
                throw new ValidationException("Invalid getNonNegoFreightCopies!");
            }

            if (Objects.nonNull(shippingInstruction.getNonNegoUnFreightCopies()) && (shippingInstruction.getNonNegoUnFreightCopies() > 100
                    || shippingInstruction.getNonNegoUnFreightCopies() < 0)) {
                log.info("Validation failed for getNonNegoUnFreightCopies for SI id : {}", shippingInstruction.getId());
                throw new ValidationException("Invalid getNonNegoUnFreightCopies!");
            }
        }

        validateFreightDetails(shippingInstruction);
    }

    private FieldClassDto createFieldClassDto(Class<?> clazz, String parentref) {
        FieldClassDto fieldClassDto = new FieldClassDto();
        fieldClassDto.setClazz(clazz);
        fieldClassDto.setFieldRef(parentref);
        return fieldClassDto;
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShippingInstruction> shippingInstructionList,
                                                             boolean getMasterData) {
        List<ShippingInstructionResponse> shippingInstructionResponses = new ArrayList<>();

        for (ShippingInstruction shippingInstruction : shippingInstructionList) {
            ShippingInstructionResponse shippingInstructionResponse = jsonHelper.convertValue(shippingInstruction, ShippingInstructionResponse.class);
            if (!CollectionUtils.isEmpty(shippingInstruction.getReferenceNumbersList())) {
                Optional<ReferenceNumbers> contractReferenceNumber = shippingInstruction.getReferenceNumbersList()
                        .stream()
                        .filter(ref -> CarrierBookingConstants.CON.equals(ref.getType()))
                        .findFirst();
                contractReferenceNumber.ifPresent(referenceNumbers -> shippingInstructionResponse.setContractNo(referenceNumbers.getReferenceNumber()));
            }
            shippingInstructionResponse.setVgmStatus(getVgmStatus(shippingInstruction));
            shippingInstructionResponse.setCrBookingId(shippingInstruction.getCarrierBookingNo());
            shippingInstructionResponses.add(shippingInstructionResponse);
        }

        List<IRunnerResponse> responseList = new ArrayList<>(shippingInstructionResponses);
        shippingInstructionMasterDataHelper.getMasterDataForList(responseList, getMasterData, false);
        return responseList;
    }

    public Map<String, Object> fetchAllMasterDataByKey(ShippingInstructionResponse shippingInstructionResponse) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> shippingInstructionMasterDataHelper.addAllMasterDataInSingleCall(shippingInstructionResponse, masterDataResponse)), executorServiceMasterData);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> shippingInstructionMasterDataHelper.addAllUnlocationDataInSingleCall(shippingInstructionResponse, masterDataResponse)), executorServiceMasterData);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> shippingInstructionMasterDataHelper.addAllCarrierDataInSingleCall(shippingInstructionResponse, masterDataResponse)), executorServiceMasterData);
        var vesselsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> shippingInstructionMasterDataHelper.addAllVesselDataInSingleCall(shippingInstructionResponse, masterDataResponse)), executorServiceMasterData);
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> shippingInstructionMasterDataHelper.addAllCommodityTypesInSingleCall(shippingInstructionResponse, masterDataResponse)), executorServiceMasterData);
        var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> shippingInstructionMasterDataHelper.addAllContainerTypesInSingleCall(shippingInstructionResponse, masterDataResponse)), executorServiceMasterData);

        CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, commodityTypesFuture, containerTypeFuture, vesselsFuture).join();
        return masterDataResponse;
    }


    public void populateFreightDetails(ShippingInstruction si, ConsolidationDetails consol) {
        if (Objects.nonNull(si.getFreightDetailList()) && !si.getFreightDetailList().isEmpty()) {
            log.debug("Freight details present in SI. No need to populate from consolidation.");
            return;
        }

        if (consol == null || consol.getPayment() == null) {
            log.debug("Either consolidation null or it does not have payment. SI ID {}", si.getId());
            return;
        }
        TenantModel tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
        FreightDetail freightDetail = new FreightDetail();
        freightDetail.setChargeType("OriginTerminalHandling");

        switch (consol.getPayment()) {
            case PAYMENT_TERM_PREPAID:
                freightDetail.setPaymentTerms("Prepaid");
                freightDetail.setPayerType(PayerType.SHIPPER);
                Optional.ofNullable(masterDataUtils.fetchUnlocationByOneIdentifier(
                                EntityTransferConstants.ID,
                                StringUtility.convertToString(tenantModel.getUnloco())
                        ))
                        .filter(list -> !list.isEmpty())
                        .map(list -> list.get(0).getLocationsReferenceGUID())
                        .ifPresent(freightDetail::setPayerLocation);
                break;

            case PAYMENT_TERM_COLLECT:
                freightDetail.setPaymentTerms("Collect");
                freightDetail.setPayerType(PayerType.CONSIGNEE);

                String paymentLocation;
                if (consol.getReceivingAgent() != null) {
                    paymentLocation = consol.getReceivingAgent().getOrgCode();
                } else {
                    paymentLocation = consol.getCarrierDetails().getDestinationPortCountry();
                }
                freightDetail.setPayerLocation(paymentLocation);
                break;

            default:
                // no defaults if other terms
                return;
        }

        // add to SI (initialize if null)
        if (si.getFreightDetailList() == null) {
            si.setFreightDetailList(new ArrayList<>());
        }
        si.getFreightDetailList().add(freightDetail);
    }

    public ShippingInstructionResponse submitShippingInstruction(Long id) { //Take only Id
        Optional<ShippingInstruction> shippingInstruction = repository.findById(id);
        ShippingInstruction si;
        if (shippingInstruction.isPresent()) {
            si = shippingInstruction.get();
        } else {
            throw new GenericException("Shipping Instruction not found");
        }

        Parties[] partiesToCheck = {si.getRequestor(), si.getShipper(), si.getForwardingAgent()};
        String remoteId = carrierBookingInttraUtil.getInttraRemoteId(partiesToCheck);

        if (null == remoteId) {
            throw new ValidationException("SI does not belong to INTTRA");
        }

        if (si.getEntityType() == EntityType.CARRIER_BOOKING) {
            CarrierBooking booking = carrierBookingDao.findById(si.getEntityId())
                    .orElseThrow(() -> new ValidationException("Carrier Booking not found"));

            validateSubmissionCriteria(booking, si);
        } else if (si.getEntityType() == EntityType.CONSOLIDATION) {
            if (si.getStatus() != ShippingInstructionStatus.Draft) {
                throw new ValidationException("Submit not allowed. Shipping Instruction not in draft state.");
            }

            ConsolidationDetails consolidationDetails = carrierBookingInttraUtil.getConsolidationDetail(si.getEntityId());
            fillDetailsFromConsol(si, consolidationDetails);
        } else {
            throw new ValidationException(INVALID_ENTITY_TYPE);
        }

        // Step 3: Mark SI as submitted
        si.setStatus(Requested);
        si.setSubmitByUserEmail(UserContext.getUser().getEmail());
        si.setPayloadJson(createPackageAndContainerPayload(si));
        ShippingInstructionInttraRequest instructionInttraRequest = jsonHelper.convertValue(si, ShippingInstructionInttraRequest.class);
        instructionInttraRequest.setFileName(getFileName(si));
        instructionInttraRequest.setCrBookingId(si.getCarrierBookingNo());
        try {
            shippingInstructionUtil.populateInttraSpecificData(instructionInttraRequest, remoteId);
        } catch (Exception e) {
            log.error("Error in setting volume/mass details", e);
        }
        shippingInstructionUtil.populateCarrierDetails(carrierBookingInttraUtil.fetchCarrierDetailsForBridgePayload(instructionInttraRequest.getSailingInformation()), instructionInttraRequest);
        try {
            callBridge(instructionInttraRequest, "SI_CREATE");
        } catch (Exception e) {
            log.error("Error in calling bridge", e);
            throw e;
        }
        ShippingInstruction saved = repository.save(si);
        carrierBookingInttraUtil.createTransactionHistory(Requested.getDescription(), FlowType.Inbound, "SI Requested", SourceSystem.CargoRunner, id, EntityTypeTransactionHistory.SI);
        return jsonHelper.convertValue(saved, ShippingInstructionResponse.class);
    }

    private void validateSubmissionCriteria(CarrierBooking booking, ShippingInstruction si) {
        // Step 1: Check booking status
        if (!(CarrierBookingStatus.ConditionallyAccepted.equals(booking.getStatus()) || CarrierBookingStatus.ConfirmedByCarrier.equals(booking.getStatus()))) {
            throw new ValidationException("Submit not allowed. Carrier Booking is not Confirmed/Conditionally Accepted.");
        }

        List<CarrierBookingInfoProjection> projectionList = repository.findConfimedBookingByConsolId(si.getEntityNumber());
        if (projectionList.size() > 1) {
            throw new ValidationException("Only one booking of all booking linked with a consolidation can be in confirmed state!!");
        }
    }

    private void populateReadOnlyFields(ShippingInstructionResponseMapper mapper, boolean isCreate) {
        ShippingInstruction shippingInstruction = mapper.getShippingInstruction();
        populateReadOnlyFields(mapper, shippingInstruction, isCreate);
    }

    private void populateReadOnlyFields(ShippingInstructionResponseMapper mapper, ShippingInstruction shippingInstruction, boolean isCreate) {
        ConsolidationDetails consolidationDetails;
        if (EntityType.CARRIER_BOOKING == shippingInstruction.getEntityType()) {
            Optional<CarrierBooking> carrierBooking = carrierBookingDao.findById(shippingInstruction.getEntityId());
            if (carrierBooking.isEmpty()) {
                throw new ValidationException("Invalid entity id");
            }
            populateSailingInformationFromCarrierBooking(shippingInstruction, carrierBooking.get());
            if (!isCreate) {
                mapper.setBookingStatus(carrierBooking.get().getStatus().name());
            }
            shippingInstruction.setEntityNumber(carrierBooking.get().getBookingNo());
        } else if (EntityType.CONSOLIDATION == shippingInstruction.getEntityType()) {
            consolidationDetails = carrierBookingInttraUtil.getConsolidationDetail(shippingInstruction.getEntityId());
            shippingInstruction.setEntityNumber(consolidationDetails.getConsolidationNumber());
            setSailingInfoAndCutoff(shippingInstruction, consolidationDetails);
        } else {
            throw new ValidationException(INVALID_ENTITY_TYPE);
        }
    }

    private void validateFreightDetails(ShippingInstruction si) {
        if (si.getFreightDetailList() != null) {
            for (FreightDetail fd : si.getFreightDetailList()) {
                if (fd.getPayerLocation() == null || fd.getPayerLocation().trim().isEmpty()) {
                    throw new ValidationException("Payment Location (payerLocation) is mandatory for all Freight Details");
                }
            }
        }
    }


    public ShippingInstructionResponse amendShippingInstruction(Long id) {
        Optional<ShippingInstruction> shippingInstructionEntity = repository.findById(id);
        if (shippingInstructionEntity.isEmpty()) {
            throw new ValidationException("Invalid shipping instruction id");
        }
        ShippingInstruction shippingInstruction = shippingInstructionEntity.get();
        ConsolidationDetails consolidationDetails;

        Parties[] partiesToCheck = {shippingInstruction.getRequestor(), shippingInstruction.getShipper(), shippingInstruction.getForwardingAgent()};
        String remoteId = carrierBookingInttraUtil.getInttraRemoteId(partiesToCheck);

        CarrierBooking booking = null;
        if (shippingInstruction.getEntityType() == EntityType.CARRIER_BOOKING) {
            booking = carrierBookingDao.findById(shippingInstruction.getEntityId())
                    .orElseThrow(() -> new ValidationException("Carrier Booking not found"));
        } else if (shippingInstruction.getEntityType() == EntityType.CONSOLIDATION) {
            consolidationDetails = carrierBookingInttraUtil.getConsolidationDetail(shippingInstruction.getEntityId());
            fillDetailsFromConsol(shippingInstruction, consolidationDetails);
        } else {
            throw new ValidationException(INVALID_ENTITY_TYPE);
        }

        checkIfAllowed(remoteId, shippingInstruction, booking, shippingInstruction.getEntityType() != EntityType.CARRIER_BOOKING);
        validateSIRequest(shippingInstruction);

        if (booking != null) {
            shippingInstruction.setCarrierBookingNo(booking.getCarrierBookingNo());
        }

        // Step 3: Mark SI as amended
        shippingInstruction.setStatus(ShippingInstructionStatus.Changed);
        shippingInstruction.setSubmitByUserEmail(UserContext.getUser().getEmail());
        shippingInstruction.setPayloadJson(createPackageAndContainerPayload(shippingInstruction));
        ShippingInstruction saved = repository.save(shippingInstruction);
        ShippingInstructionInttraRequest instructionInttraRequest = jsonHelper.convertValue(shippingInstruction, ShippingInstructionInttraRequest.class);
        instructionInttraRequest.setFileName(getFileName(shippingInstruction));
        instructionInttraRequest.setCrBookingId(shippingInstruction.getCarrierBookingNo());
        try {
            shippingInstructionUtil.populateInttraSpecificData(instructionInttraRequest, remoteId);
        } catch (Exception e) {
            log.error("Exception during conversion of volume / mass ", e);
        }
        shippingInstructionUtil.populateCarrierDetails(carrierBookingInttraUtil.fetchCarrierDetailsForBridgePayload(instructionInttraRequest.getSailingInformation()), instructionInttraRequest);

        try {
            callBridge(instructionInttraRequest, "SI_AMEND");
        } catch (Exception e) {
            log.error("Exception in calling bridge {}", e.getMessage());
            carrierBookingInttraUtil.createTransactionHistory(Requested.getDescription(), FlowType.Inbound, e.getMessage(), SourceSystem.CargoRunner, id, EntityTypeTransactionHistory.SI);
            throw e;
        }
        carrierBookingInttraUtil.createTransactionHistory(Requested.getDescription(), FlowType.Inbound, "SI Amended", SourceSystem.CargoRunner, id, EntityTypeTransactionHistory.SI);
        return jsonHelper.convertValue(saved, ShippingInstructionResponse.class);
    }

    @Override
    public void updateShippingInstructionsStatus(ShippingInstructionEventDto shippingInstructionEvent, String fileName) {
        if (Objects.nonNull(fileName) && StringUtility.isNotEmpty(shippingInstructionEvent.getSiId())) {
            if (fileName.startsWith(ShippingInstructionsConstants.APERAK_PREFIX) && fileName.endsWith(ShippingInstructionsConstants.XML_SUFFIX)) {
                //received status updated from carrier
                Optional<ShippingInstruction> shippingInstructionOptional = repository.findById(Long.valueOf(shippingInstructionEvent.getSiId()));
                if (shippingInstructionOptional.isEmpty()) {
                    log.error("received invalid shipping instruction id from carrier {}", shippingInstructionEvent.getSiId());
                    return;
                }
                ShippingInstruction shippingInstruction = shippingInstructionOptional.get();
                ShippingInstructionStatus shippingInstructionStatus = parseIntraStatus(shippingInstructionEvent.getStatus());
                shippingInstruction.setStatus(shippingInstructionStatus);
                shippingInstruction.setComments(shippingInstructionEvent.getComments());
                repository.save(shippingInstruction);
                carrierBookingInttraUtil.createTransactionHistory(shippingInstructionStatus.name(), FlowType.Outbound, shippingInstructionStatus.getDescription() + Constants.EMPTY_STRING + shippingInstruction.getSailingInformation().getCarrier(), SourceSystem.Carrier, shippingInstruction.getId(), EntityTypeTransactionHistory.SI);
            } else if (fileName.startsWith(ShippingInstructionsConstants.CONTRLX_PREFIX) && fileName.endsWith(ShippingInstructionsConstants.XML_SUFFIX)) {
                //received status updated from inttra
                Optional<ShippingInstruction> shippingInstructionOptional = repository.findById(Long.valueOf(shippingInstructionEvent.getSiId()));
                if (shippingInstructionOptional.isEmpty()) {
                    log.error("received invalid shipping instruction id from inttra {}", shippingInstructionEvent.getSiId());
                    return;
                }
                ShippingInstruction shippingInstruction = shippingInstructionOptional.get();
                ShippingInstructionStatus shippingInstructionStatus = getShippingInstructionStatus(shippingInstructionEvent);
                shippingInstruction.setStatus(shippingInstructionStatus);
                shippingInstruction.setComments(shippingInstructionEvent.getComments());
                repository.save(shippingInstruction);
                carrierBookingInttraUtil.createTransactionHistory(shippingInstructionStatus.name(), FlowType.Outbound, shippingInstructionStatus.getDescription(), SourceSystem.INTTRA, shippingInstruction.getId(), EntityTypeTransactionHistory.SI);
                sendNotification(shippingInstruction);
            }
        }
    }

    private ShippingInstructionStatus getShippingInstructionStatus(ShippingInstructionEventDto shippingInstructionEvent) {
        String status = shippingInstructionEvent.getStatus();
        if (StringUtility.isNotEmpty(shippingInstructionEvent.getStatus()) && ShippingInstructionsConstants.ACCEPTED.equals(shippingInstructionEvent.getStatus())) {
            status = ShippingInstructionsConstants.PROCESSED;
        } else if (StringUtility.isNotEmpty(shippingInstructionEvent.getStatus()) && ShippingInstructionsConstants.REJECTED.equals(shippingInstructionEvent.getStatus())) {
            status = ShippingInstructionsConstants.REJECTED_BY_CARRIER;
        }
        return parseIntraStatus(status);
    }

    @Override
    public void cancelShippingInstruction(Long id) {
        ShippingInstruction shippingInstruction = repository.findById(id)
                .orElseThrow(() -> new ValidationException("INVALID_SI:" + id));

        shippingInstruction.setStatus(ShippingInstructionStatus.Cancelled);
        ShippingInstruction savedCarrierBooking = repository.save(shippingInstruction);
        log.info("SI with id :{} cancelled ", savedCarrierBooking.getId());
        carrierBookingInttraUtil.createTransactionHistory(Requested.getDescription(), FlowType.Inbound, "SI Cancelled", SourceSystem.CargoRunner, id, EntityTypeTransactionHistory.SI);
    }

    private static void checkIfAllowed(String remoteId, ShippingInstruction shippingInstructionEntity, CarrierBooking booking,
                                       boolean isStandAlone) {
        if (null == remoteId) {
            throw new ValidationException("SI does not belong to INTTRA");
        }

        if (!(Requested == shippingInstructionEntity.getStatus() || ShippingInstructionStatus.AcceptedByCarrier == shippingInstructionEntity.getStatus())) {
            throw new ValidationException("Amendment not allowed. Shipping Instruction is not Submitted.");
        }

        if (!isStandAlone && booking != null && !CarrierBookingStatus.ConfirmedByCarrier.name().equalsIgnoreCase(booking.getStatus().name())) {
            throw new ValidationException("Amendment not allowed. Carrier booking is not submitted.");
        }
    }

    private String getFileName(ShippingInstruction si) {
        // Format timestamp in UTC
        String timestamp = DateTimeFormatter.ofPattern("yyyyMMddHHmmss")
                .withZone(ZoneOffset.UTC)
                .format(Instant.now());

        return "SIRequest_" + si.getId() + "_"
                + si.getCarrierBookingNo()  // use carrier booking number
                + "_"
                + timestamp
                + ".xml";
    }


    private String createPackageAndContainerPayload(ShippingInstruction shippingInstruction) {
        ContainerPackageSiPayload packageSiPayload = new ContainerPackageSiPayload();

        if (shippingInstruction.getCommonPackagesList() != null &&
                !shippingInstruction.getCommonPackagesList().isEmpty()) {
            packageSiPayload.setPackageDetail(shippingInstruction.getCommonPackagesList());
        }

        if (shippingInstruction.getContainersList() != null &&
                !shippingInstruction.getContainersList().isEmpty()) {
            packageSiPayload.setContainerDetail(shippingInstruction.getContainersList());
        }

        if ((packageSiPayload.getPackageDetail() == null || packageSiPayload.getPackageDetail().isEmpty()) &&
                (packageSiPayload.getContainerDetail() == null || packageSiPayload.getContainerDetail().isEmpty())) {
            return null;
        }

        return jsonHelper.convertToJson(packageSiPayload);
    }

    private void callBridge(ShippingInstructionInttraRequest shippingInstruction, String integrationCode) {
        UUID transactionId = UUID.randomUUID();
        try {
            bridgeServiceAdapter.bridgeApiIntegration(shippingInstruction, integrationCode, transactionId.toString(), transactionId.toString());
        } catch (RunnerException e) {
            log.error("Exception while calling bridge. {}", e.getMessage());
        }
    }

    private ShippingInstructionStatus parseIntraStatus(String type) {

        return switch (type) {
            case "ConditionallyAccepted" -> ShippingInstructionStatus.ConditionallyAccepted;
            case "Processed" -> ShippingInstructionStatus.AcceptedByCarrier;
            case "Accepted" -> ShippingInstructionStatus.ConfirmedByCarrier;
            case "Rejected" -> ShippingInstructionStatus.DeclinedByCarrier;
            case "RejectedByCarrier" -> ShippingInstructionStatus.RejectedByCarrier;
            case "Replaced" -> ShippingInstructionStatus.ReplacedByCarrier;
            case "ChangeSIRequested" -> ShippingInstructionStatus.Changed;
            case "NewSIRequested" -> ShippingInstructionStatus.Requested;
            default -> ShippingInstructionStatus.PendingFromCarrier;
        };

    }
    protected void sendNotification(ShippingInstruction shippingInstruction) {
        try {
            List<String> requests = new ArrayList<>(List.of(ShippingInstructionsConstants.SHIPPING_INSTRUCTION_EMAIL_TEMPLATE));
            List<EmailTemplatesRequest> emailTemplates = carrierBookingInttraUtil.fetchEmailTemplate(requests);
            EmailTemplatesRequest verifiedGrossMassEmailTemplate = emailTemplates.stream()
                    .filter(Objects::nonNull)
                    .filter(template -> SHIPPING_INSTRUCTION_EMAIL_TEMPLATE.equalsIgnoreCase(template.getType()))
                    .findFirst()
                    .orElse(null);
            if (Objects.nonNull(verifiedGrossMassEmailTemplate)) {
                List<String> toEmails = shippingInstructionUtil.getSendEmailBaseRequest(shippingInstruction);
                notificationService.sendEmail(verifiedGrossMassEmailTemplate.getBody(), verifiedGrossMassEmailTemplate.getSubject(), toEmails, new ArrayList<>());
                log.info("Email Notification sent successfully for State Change of Shipping instruction Id: {}", shippingInstruction.getId());
            }
        } catch (Exception e) {
            log.error("Error in sending shipping instruction email: {}", e.getMessage());
        }
    }
}
