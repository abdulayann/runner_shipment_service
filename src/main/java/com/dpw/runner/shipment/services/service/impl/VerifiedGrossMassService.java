package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.impl.BridgeServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.CarrierBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITransactionHistoryDao;
import com.dpw.runner.shipment.services.dao.interfaces.IVerifiedGrossMassDao;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.VerifiedGrossMassInttraRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.VerifiedGrossMassRequest;
import com.dpw.runner.shipment.services.dto.response.FieldClassDto;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.bridgeService.BridgeServiceResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.NotificationContactResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ReferenceNumberResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.SailingInformationResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassBulkUpdateRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassInttraResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassListResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.TransactionHistory;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.FlowType;
import com.dpw.runner.shipment.services.entity.enums.OperationType;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionStatus;
import com.dpw.runner.shipment.services.entity.enums.SourceSystem;
import com.dpw.runner.shipment.services.entity.enums.VerifiedGrossMassStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.helpers.VerifiedGrossMassMasterDataHelper;
import com.dpw.runner.shipment.services.projection.CarrierBookingInfoProjection;
import com.dpw.runner.shipment.services.repository.interfaces.ICommonContainersRepository;
import com.dpw.runner.shipment.services.service.interfaces.IVerifiedGrossMassService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.FieldUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.v3.VerifiedGrossMassValidationUtil;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.transaction.Transactional;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class VerifiedGrossMassService implements IVerifiedGrossMassService {
    private final IVerifiedGrossMassDao verifiedGrossMassDao;
    private final JsonHelper jsonHelper;
    private final CarrierBookingDao carrierBookingDao;
    private final IConsolidationDetailsDao consolidationDetailsDao;
    private final CommonUtils commonUtils;
    private final MasterDataUtils masterDataUtils;
    private final ExecutorService executorServiceMasterData;
    private final VerifiedGrossMassMasterDataHelper verifiedGrossMassMasterDataHelper;
    private final VerifiedGrossMassValidationUtil verifiedGrossMassValidationUtil;
    private final ICommonContainersRepository commonContainersRepository;
    private final ITransactionHistoryDao transactionHistoryDao;
    private final BridgeServiceAdapter bridgeServiceAdapter;


    public VerifiedGrossMassService(IVerifiedGrossMassDao verifiedGrossMassDao, JsonHelper jsonHelper, CarrierBookingDao carrierBookingDao, IConsolidationDetailsDao consolidationDetailsDao, CommonUtils commonUtils,
                                    MasterDataUtils masterDataUtils, @Qualifier("executorServiceMasterData") ExecutorService executorServiceMasterData, VerifiedGrossMassMasterDataHelper verifiedGrossMassMasterDataHelper,
                                    ICommonContainersRepository commonContainersRepository, VerifiedGrossMassValidationUtil verifiedGrossMassValidationUtil, BridgeServiceAdapter bridgeServiceAdapter, ITransactionHistoryDao transactionHistoryDao) {
        this.verifiedGrossMassDao = verifiedGrossMassDao;
        this.jsonHelper = jsonHelper;
        this.carrierBookingDao = carrierBookingDao;
        this.consolidationDetailsDao = consolidationDetailsDao;
        this.commonUtils = commonUtils;
        this.masterDataUtils = masterDataUtils;
        this.executorServiceMasterData = executorServiceMasterData;
        this.verifiedGrossMassMasterDataHelper = verifiedGrossMassMasterDataHelper;
        this.commonContainersRepository = commonContainersRepository;
        this.verifiedGrossMassValidationUtil = verifiedGrossMassValidationUtil;
        this.bridgeServiceAdapter = bridgeServiceAdapter;
        this.transactionHistoryDao = transactionHistoryDao;
    }

    @Override
    public VerifiedGrossMassResponse create(VerifiedGrossMassRequest request) {
        verifiedGrossMassValidationUtil.validateServiceType(request);
        Object entity = verifiedGrossMassValidationUtil.validateRequest(request.getEntityType(), request.getEntityId());
        VerifiedGrossMass verifiedGrossMass = jsonHelper.convertValue(request, VerifiedGrossMass.class);
        updateReadOnlyDataToEntity(request, entity, verifiedGrossMass);
        verifiedGrossMass.setStatus(VerifiedGrossMassStatus.Draft);
        verifiedGrossMass.setCreateByUserEmail(UserContext.getUser().getEmail());
        verifiedGrossMass.setSubmitByUserEmail(UserContext.getUser().getEmail());
        VerifiedGrossMass savedEntity = verifiedGrossMassDao.save(verifiedGrossMass);
        return jsonHelper.convertValue(savedEntity, VerifiedGrossMassResponse.class);
    }

    @Override
    public VerifiedGrossMassResponse retrieveById(Long id) {
        Optional<VerifiedGrossMass> verifiedGrossMass = verifiedGrossMassDao.findById(id);
        if (verifiedGrossMass.isEmpty()) {
            throw new ValidationException("Invalid vgm id");
        }
        VerifiedGrossMass verifiedGrossMassEntity = verifiedGrossMass.get();
        VerifiedGrossMassResponse verifiedGrossMassResponse = jsonHelper.convertValue(verifiedGrossMassEntity, VerifiedGrossMassResponse.class);
        if (EntityType.CARRIER_BOOKING.equals(verifiedGrossMassEntity.getEntityType())) {
            CarrierBookingInfoProjection carrierBookingInfo = carrierBookingDao.findCarrierBookingInfoById(verifiedGrossMassEntity.getEntityId());
            if (Objects.nonNull(carrierBookingInfo)) {
                verifiedGrossMassResponse.setBookingStatus(carrierBookingInfo.getBookingStatus() != null ? CarrierBookingStatus.valueOf(carrierBookingInfo.getBookingStatus()) : null);
                verifiedGrossMassResponse.setSiStatus(carrierBookingInfo.getSiStatus() != null ? ShippingInstructionStatus.valueOf(carrierBookingInfo.getSiStatus()) : null);
            }
        }
        return verifiedGrossMassResponse;
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel, boolean getMasterData) {
        ListCommonRequest listCommonRequest = (ListCommonRequest) commonRequestModel.getData();
        if (listCommonRequest == null) {
            log.error(VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException(VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_LIST_REQUEST_NULL_ERROR);
        }

        Pair<Specification<VerifiedGrossMass>, Pageable> tuple = fetchData(listCommonRequest, VerifiedGrossMass.class, VerifiedGrossMassConstants.tableNames);
        Page<VerifiedGrossMass> verifiedGrossMassPage = verifiedGrossMassDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info(VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());


        List<IRunnerResponse> filteredList = convertEntityListToDtoList(verifiedGrossMassPage.getContent());

        return ResponseHelper.buildListSuccessResponse(
                filteredList,
                verifiedGrossMassPage.getTotalPages(),
                verifiedGrossMassPage.getTotalElements());
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<VerifiedGrossMass> verifiedGrossMassList) {
        List<VerifiedGrossMassListResponse> verifiedGrossMassListResponses = new ArrayList<>();

        for (VerifiedGrossMass verifiedGrossMass : verifiedGrossMassList) {
            VerifiedGrossMassListResponse verifiedGrossMassListResponse = jsonHelper.convertValue(verifiedGrossMass, VerifiedGrossMassListResponse.class);
            verifiedGrossMassListResponses.add(verifiedGrossMassListResponse);
        }

        return new ArrayList<>(verifiedGrossMassListResponses);
    }

    @Override
    public VerifiedGrossMassResponse update(VerifiedGrossMassRequest request) {
        if (Objects.isNull(request.getId())) {
            throw new ValidationException("Id can not be null");
        }
        Optional<VerifiedGrossMass> verifiedGrossMassOptional = verifiedGrossMassDao.findById(request.getId());
        if (verifiedGrossMassOptional.isEmpty()) {
            throw new ValidationException("Invalid verified gross mass id");
        }
        VerifiedGrossMass verifiedGrossMassEntity = verifiedGrossMassOptional.get();
        if (!Objects.equals(verifiedGrossMassEntity.getEntityId(), request.getEntityId())) {
            throw new ValidationException("Entity Id mismatch with existing entity id");
        }
        verifiedGrossMassValidationUtil.validateServiceType(request);
        Object entity = verifiedGrossMassValidationUtil.validateRequest(request.getEntityType(), request.getEntityId());
        //update header information from existing entity
        VerifiedGrossMass verifiedGrossMass = jsonHelper.convertValue(request, VerifiedGrossMass.class);
        if (EntityType.CONSOLIDATION.equals(request.getEntityType())) {
            ConsolidationDetails consolidationDetails = (ConsolidationDetails) entity;
            verifiedGrossMass.setEntityNumber(consolidationDetails.getConsolidationNumber());
        } else if (EntityType.CARRIER_BOOKING.equals(request.getEntityType())) {
            CarrierBooking carrierBooking = (CarrierBooking) entity;
            verifiedGrossMass.setCarrierBookingNo(carrierBooking.getCarrierBookingNo());
            verifiedGrossMass.setCarrierBlNo(carrierBooking.getCarrierBlNo());
            verifiedGrossMass.setEntityNumber(carrierBooking.getBookingNo());
        }
        if (Objects.isNull(verifiedGrossMass.getSailingInformation())) {
            verifiedGrossMass.setSailingInformation(new SailingInformation());
        }
        verifiedGrossMass.setStatus(verifiedGrossMassEntity.getStatus());
        verifiedGrossMass.getSailingInformation().setCarrier(verifiedGrossMassEntity.getSailingInformation().getCarrier());
        VerifiedGrossMass savedEntity = verifiedGrossMassDao.save(verifiedGrossMass);
        return jsonHelper.convertValue(savedEntity, VerifiedGrossMassResponse.class);
    }

    private static void updateReadOnlyDataToEntity(VerifiedGrossMassRequest request, Object entity, VerifiedGrossMass verifiedGrossMass) {
        if (EntityType.CONSOLIDATION.equals(request.getEntityType())) {
            ConsolidationDetails consolidationDetails = (ConsolidationDetails) entity;
            verifiedGrossMass.setEntityNumber(consolidationDetails.getConsolidationNumber());
            //read only fields
            SailingInformation sailingInformation = verifiedGrossMass.getSailingInformation();
            if (Objects.isNull(sailingInformation)) {
                sailingInformation = new SailingInformation();
            }
            if (consolidationDetails.getCarrierDetails() != null) {
                sailingInformation.setCarrier(consolidationDetails.getCarrierDetails().getShippingLine());
            }
            verifiedGrossMass.setSailingInformation(sailingInformation);
        } else if (EntityType.CARRIER_BOOKING.equals(request.getEntityType())) {
            CarrierBooking carrierBooking = (CarrierBooking) entity;
            verifiedGrossMass.setCarrierBookingNo(carrierBooking.getCarrierBookingNo());
            verifiedGrossMass.setCarrierBlNo(carrierBooking.getCarrierBlNo());
            verifiedGrossMass.setEntityNumber(carrierBooking.getBookingNo());
            //read only fields
            SailingInformation sailingInformation = verifiedGrossMass.getSailingInformation();
            if (Objects.isNull(sailingInformation)) {
                sailingInformation = new SailingInformation();
            }
            if (Objects.nonNull(carrierBooking.getSailingInformation())) {
                sailingInformation.setCarrier(carrierBooking.getSailingInformation().getCarrier());
            }
            verifiedGrossMass.setSailingInformation(sailingInformation);
        }
    }

    @Override
    public void delete(Long id) {
        verifiedGrossMassDao.delete(id);
    }

    @Override
    public ResponseEntity<IRunnerResponse> getAllMasterData(Long vgmId) {
        String responseMsg;
        try {
            Optional<VerifiedGrossMass> verifiedGrossMassOptional = verifiedGrossMassDao.findById(vgmId);
            if (verifiedGrossMassOptional.isEmpty()) {
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            VerifiedGrossMass verifiedGrossMass = verifiedGrossMassOptional.get();
            long start = System.currentTimeMillis();
            List<String> includeColumns = FieldUtils.getMasterDataAnnotationFields(List.of(createFieldClassDto(VerifiedGrossMass.class, null), createFieldClassDto(SailingInformation.class, "sailingInformation.")));
            includeColumns.addAll(VerifiedGrossMassConstants.LIST_INCLUDE_COLUMNS);
            VerifiedGrossMassResponse verifiedGrossMassResponse = (VerifiedGrossMassResponse) commonUtils.setIncludedFieldsToResponse(verifiedGrossMass, new HashSet<>(includeColumns), new VerifiedGrossMassResponse());
            log.info("Total time taken in setting verified gross mass details response {}", (System.currentTimeMillis() - start));
            Map<String, Object> response = fetchAllMasterDataByKey(verifiedGrossMassResponse);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private FieldClassDto createFieldClassDto(Class<?> clazz, String parentref) {
        FieldClassDto fieldClassDto = new FieldClassDto();
        fieldClassDto.setClazz(clazz);
        fieldClassDto.setFieldRef(parentref);
        return fieldClassDto;
    }

    public Map<String, Object> fetchAllMasterDataByKey(VerifiedGrossMassResponse verifiedGrossMassResponse) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> verifiedGrossMassMasterDataHelper.addAllMasterDataInSingleCall(verifiedGrossMassResponse, masterDataResponse)), executorServiceMasterData);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> verifiedGrossMassMasterDataHelper.addAllUnlocationDataInSingleCall(verifiedGrossMassResponse, masterDataResponse)), executorServiceMasterData);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> verifiedGrossMassMasterDataHelper.addAllCarrierDataInSingleCall(verifiedGrossMassResponse, masterDataResponse)), executorServiceMasterData);
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> verifiedGrossMassMasterDataHelper.addAllCommodityTypesInSingleCall(verifiedGrossMassResponse, masterDataResponse)), executorServiceMasterData);
        var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> verifiedGrossMassMasterDataHelper.addAllContainerTypesInSingleCall(verifiedGrossMassResponse, masterDataResponse)), executorServiceMasterData);
        var vesselsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> verifiedGrossMassMasterDataHelper.addAllVesselDataInSingleCall(verifiedGrossMassResponse, masterDataResponse)), executorServiceMasterData);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, commodityTypesFuture, containerTypeFuture, vesselsFuture).join();

        return masterDataResponse;
    }

    @Override
    public VerifiedGrossMassResponse getDefaultVerifiedGrossMassValues(EntityType type, Long entityId) {
        VerifiedGrossMassResponse verifiedGrossMassResponse = new VerifiedGrossMassResponse();
        verifiedGrossMassResponse.setEntityType(type);
        if (EntityType.CARRIER_BOOKING.equals(type)) {
            setDefaultDataFromCarrierBooking(entityId, verifiedGrossMassResponse);

        } else if (EntityType.CONSOLIDATION.equals(type)) {
            setDefaultDataFromConsol(entityId, verifiedGrossMassResponse);
        } else {
            throw new ValidationException("Invalid value of Entity Type");
        }
        return verifiedGrossMassResponse;
    }

    private void setDefaultDataFromConsol(Long entityId, VerifiedGrossMassResponse verifiedGrossMassResponse) {
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findConsolidationsById(entityId);
        if (Objects.isNull(consolidationDetails)) {
            throw new ValidationException("Invalid consolidation id");
        }
        verifiedGrossMassResponse.setEntityId(consolidationDetails.getId());
        verifiedGrossMassResponse.setEntityNumber(consolidationDetails.getConsolidationNumber());
        SailingInformationResponse sailingInformationResponse = new SailingInformationResponse();
        sailingInformationResponse.setCarrier(consolidationDetails.getCarrierDetails().getShippingLine());
        sailingInformationResponse.setVerifiedGrossMassCutoff(consolidationDetails.getVerifiedGrossMassCutoff());

        verifiedGrossMassResponse.setSailingInformation(sailingInformationResponse);
        //sending agent is origin agent, receivingAgent is destination agent in consol
        setPartiesDataFromConsol(verifiedGrossMassResponse, consolidationDetails);
        List<ReferenceNumberResponse> referenceNumberResponses = getReferenceNumberResponses(consolidationDetails);
        verifiedGrossMassResponse.setReferenceNumbersList(referenceNumberResponses);
        List<CommonContainerResponse> commonContainersList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(consolidationDetails.getContainersList())) {
            for (Containers containers : consolidationDetails.getContainersList()) {
                CommonContainerResponse commonContainers = getCommonContainerResponse(containers);
                commonContainersList.add(commonContainers);
            }
        }
        verifiedGrossMassResponse.setContainersList(commonContainersList);
    }

    private void setPartiesDataFromConsol(VerifiedGrossMassResponse verifiedGrossMassResponse, ConsolidationDetails consolidationDetails) {
        if (Objects.nonNull(consolidationDetails.getSendingAgent())) {
            PartiesResponse partiesResponse = jsonHelper.convertValue(consolidationDetails.getSendingAgent(), PartiesResponse.class);
            partiesResponse.setId(null);
            partiesResponse.setGuid(null);
            verifiedGrossMassResponse.setRequestor(partiesResponse);
            verifiedGrossMassResponse.setResponsible(partiesResponse);
            verifiedGrossMassResponse.setAuthorised(partiesResponse);
        }
    }

    @NotNull
    private static List<ReferenceNumberResponse> getReferenceNumberResponses(ConsolidationDetails consolidationDetails) {
        List<ReferenceNumberResponse> referenceNumberResponses = new ArrayList<>();
        if (!CollectionUtils.isEmpty(consolidationDetails.getReferenceNumbersList())) {
            for (ReferenceNumbers referenceNumbers : consolidationDetails.getReferenceNumbersList()) {
                ReferenceNumberResponse referenceNumberResponse = new ReferenceNumberResponse();
                referenceNumberResponse.setType(referenceNumbers.getType());
                referenceNumberResponse.setReferenceNumber(referenceNumbers.getReferenceNumber());
                referenceNumberResponses.add(referenceNumberResponse);
            }
        }
        return referenceNumberResponses;
    }

    private void setDefaultDataFromCarrierBooking(Long entityId, VerifiedGrossMassResponse verifiedGrossMassResponse) {
        Optional<CarrierBooking> carrierBookingEntity = carrierBookingDao.findById(entityId);
        if (carrierBookingEntity.isEmpty()) {
            throw new ValidationException("Invalid carrier booking id");
        }
        CarrierBooking carrierBooking = carrierBookingEntity.get();
        verifiedGrossMassResponse.setBookingStatus(carrierBooking.getStatus());
        verifiedGrossMassResponse.setStatus(VerifiedGrossMassStatus.Draft);
        if (Objects.nonNull(carrierBooking.getShippingInstruction())) {
            verifiedGrossMassResponse.setSiStatus(carrierBooking.getShippingInstruction().getStatus());
        }
        verifiedGrossMassResponse.setEntityId(carrierBooking.getId());
        verifiedGrossMassResponse.setEntityNumber(carrierBooking.getBookingNo());
        verifiedGrossMassResponse.setCarrierBlNo(carrierBooking.getCarrierBlNo());
        verifiedGrossMassResponse.setCarrierBookingNo(carrierBooking.getCarrierBookingNo());

        SailingInformationResponse sailingInformationResponse = new SailingInformationResponse();
        sailingInformationResponse.setCarrier(carrierBooking.getSailingInformation().getCarrier());
        sailingInformationResponse.setVerifiedGrossMassCutoff(carrierBooking.getSailingInformation().getVerifiedGrossMassCutoff());
        verifiedGrossMassResponse.setSailingInformation(sailingInformationResponse);
        //reference numbers
        List<ReferenceNumberResponse> referenceNumbersResponses = new ArrayList<>();
        if (!CollectionUtils.isEmpty(carrierBooking.getReferenceNumbersList())) {
            for (ReferenceNumbers referenceNumbers : carrierBooking.getReferenceNumbersList()) {
                ReferenceNumberResponse referenceNumbersResponse = new ReferenceNumberResponse();
                referenceNumbersResponse.setReferenceNumber(referenceNumbers.getReferenceNumber());
                referenceNumbersResponse.setType(referenceNumbers.getType());
                referenceNumbersResponses.add(referenceNumbersResponse);
            }
        }
        verifiedGrossMassResponse.setReferenceNumbersList(referenceNumbersResponses);

        //containers
        if (!CollectionUtils.isEmpty(carrierBooking.getContainersList())) {
            verifiedGrossMassResponse.setContainersList(jsonHelper.convertValueToList(carrierBooking.getContainersList(), CommonContainerResponse.class));
            verifiedGrossMassResponse.getContainersList()
                    .forEach(container -> {
                        container.setId(null);
                        container.setGuid(null);
                    });
        }
        //parties
        if (Objects.nonNull(carrierBooking.getRequester())) {
            PartiesResponse partiesResponse = jsonHelper.convertValue(carrierBooking.getRequester(), PartiesResponse.class);
            partiesResponse.setId(null);
            partiesResponse.setGuid(null);
            verifiedGrossMassResponse.setRequestor(partiesResponse);
        }
        if (Objects.nonNull(carrierBooking.getShipper())) {
            PartiesResponse partiesResponse = jsonHelper.convertValue(carrierBooking.getShipper(), PartiesResponse.class);
            partiesResponse.setId(null);
            partiesResponse.setGuid(null);
            verifiedGrossMassResponse.setResponsible(partiesResponse);
        }
        if (Objects.nonNull(carrierBooking.getForwardingAgent())) {
            PartiesResponse partiesResponse = jsonHelper.convertValue(carrierBooking.getForwardingAgent(), PartiesResponse.class);
            partiesResponse.setId(null);
            partiesResponse.setGuid(null);
            verifiedGrossMassResponse.setAuthorised(partiesResponse);
        }
    }

    private static CommonContainerResponse getCommonContainerResponse(Containers containers) {
        CommonContainerResponse commonContainers = new CommonContainerResponse();
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
        return commonContainers;
    }

    @Transactional
    @Override
    public List<CommonContainerResponse> bulkUpdateContainers(VerifiedGrossMassBulkUpdateRequest request) {
        // Fetch all containers
        List<CommonContainers> containers = commonContainersRepository.findAllByIdIn(request.getContainerIds());
        if (Objects.isNull(containers) || containers.size() != request.getContainerIds().size()) {
            throw new ValidationException("Some containers could not be found");
        }

        // Update containers with new values (only fields that are provided)
        for (CommonContainers container : containers) {
            if (request.getWeightDeterminationMethod() != null) {
                container.setWeightDeterminationMethod(request.getWeightDeterminationMethod());
            }
            if (StringUtility.isNotEmpty(request.getWeightDeterminationLocation())) {
                container.setWeightDeterminationLocation(request.getWeightDeterminationLocation());
            }
            if (request.getWeighingParty() != null) {
                container.setWeighingParty(request.getWeighingParty());
            }
            if (StringUtility.isNotEmpty(request.getApprovalSignature())) {
                container.setApprovalSignature(request.getApprovalSignature().toUpperCase());
            }
            if (request.getApprovalDate() != null) {
                container.setApprovalDate(request.getApprovalDate());
            }
        }
        // Save all updated containers
        List<CommonContainers> updatedContainers = commonContainersRepository.saveAll(containers);
        return updatedContainers.stream()
                .map(container -> jsonHelper.convertValue(container, CommonContainerResponse.class))
                .toList();
    }

    public void submitOrAmendVerifiedGrossMass(VerifiedGrossMassInttraRequest verifiedGrossMassInttraRequest) throws RunnerException {

        Optional<VerifiedGrossMass> verifiedGrossMassOptional = verifiedGrossMassDao.findById(verifiedGrossMassInttraRequest.getId());
        if (verifiedGrossMassOptional.isEmpty()) {
            throw new ValidationException("Invalid VGM Id: " + verifiedGrossMassInttraRequest.getId());
        }

        List<CommonContainers> containersList =
                commonContainersRepository.findAllByIdIn(verifiedGrossMassInttraRequest.getContainerIds());
        VerifiedGrossMass verifiedGrossMass = verifiedGrossMassOptional.get();
//        CarrierBooking carrierBooking = carrierBookingDao.findByBookingNo(verifiedGrossMass.getCarrierBookingNo());

        for (CommonContainers container : containersList) {

            VerifiedGrossMassInttraResponse verifiedGrossMassInttraResponse = new VerifiedGrossMassInttraResponse();

            // Set Message Date Time
            verifiedGrossMassInttraResponse.setMessageGuid(UUID.randomUUID());
            verifiedGrossMassInttraResponse.setMessageDateTime(LocalDateTime.now());
            verifiedGrossMassInttraResponse.setTenantId(VerifiedGrossMassConstants.INTTRA);

            // Setting Submitter Party
            verifiedGrossMassInttraResponse.setRequestor(fetchRequiredParty(verifiedGrossMass.getRequestor()));

            NotificationContactResponse notificationContractResponse = new NotificationContactResponse();
            notificationContractResponse.setUsername(UserContext.getUser().getUsername());
            notificationContractResponse.setEmails(populateRequestorEmails(verifiedGrossMass));
            verifiedGrossMassInttraResponse.setRequestorNotificationContact(notificationContractResponse);


            CommonContainerResponse containerResponse = new CommonContainerResponse();

            containerResponse.setContainerNo(container.getContainerNo());
            containerResponse.setVgmWeight(container.getVgmWeight());
            containerResponse.setVgmWeightUnit(container.getVgmWeightUnit());
            containerResponse.setApprovalSignature(container.getApprovalSignature());
            containerResponse.setApprovalDate(container.getApprovalDate());
            containerResponse.setWeightDeterminationMethod(container.getWeightDeterminationMethod());
            containerResponse.setWeightDeterminationDateTime(container.getWeightDeterminationDateTime());

            // Other container fields
            containerResponse.setGrossWeight(container.getGrossWeight());
            containerResponse.setGrossWeightUnit(container.getGrossWeightUnit());
            containerResponse.setTareWeight(container.getTareWeight());
            containerResponse.setTareWeightUnit(container.getTareWeightUnit());
            containerResponse.setSealNumber(container.getSealNumber());

            verifiedGrossMassInttraResponse.setContainer(containerResponse);

            // Set Other parties
            verifiedGrossMassInttraResponse.setResponsible(fetchRequiredParty(verifiedGrossMass.getResponsible()));
            verifiedGrossMassInttraResponse.setAuthorised(fetchRequiredParty(verifiedGrossMass.getAuthorised()));

            // Set carrier carrier booking details
            verifiedGrossMassInttraResponse.setCarrierBookingNo(verifiedGrossMass.getCarrierBookingNo());
            verifiedGrossMassInttraResponse.setSubmitterReference(verifiedGrossMass.getCarrierBookingNo());

            verifiedGrossMassMasterDataHelper.populateCarrierDetails(
                    verifiedGrossMassMasterDataHelper.fetchCarrierDetailsForBridgePayload(verifiedGrossMass),
                    verifiedGrossMassInttraResponse);

            // Generates number between 10000 and 99999 and set fileName
            SecureRandom random = new SecureRandom();
            int rnd = 10000 + random.nextInt(90000);
            String fileName = "VGMRequest_" + verifiedGrossMassInttraRequest.getId() + "_" + rnd + ".xml";
            verifiedGrossMassInttraResponse.setFileName(fileName);

            verifiedGrossMassInttraResponse.setDelegated(verifiedGrossMass.getIsDelegated());


            // Set Response State
            if (OperationType.SUBMIT.equals(verifiedGrossMassInttraRequest.getOperationType())) {
                verifiedGrossMassInttraResponse.setState(VerifiedGrossMassConstants.ORIGINAL);
                log.info("Bridge payload {}", jsonHelper.convertToJson(verifiedGrossMassInttraResponse));
                BridgeServiceResponse bridgeServiceResponse = (BridgeServiceResponse) bridgeServiceAdapter.requestTactResponse(CommonRequestModel.buildRequest((IRunnerRequest) verifiedGrossMassInttraResponse));
                if (isBridgeServiceResponseNotValid(bridgeServiceResponse)) {
                    log.error("Getting error from Bridge while uploading template to: " + jsonHelper.convertToJson(bridgeServiceResponse));
                    throw new RunnerException("Getting error from Bridge");
                }
            } else if (OperationType.AMEND.equals(verifiedGrossMassInttraRequest.getOperationType())) {
                verifiedGrossMassInttraResponse.setState(VerifiedGrossMassConstants.AMEND);
                log.info("Bridge payload {}",  verifiedGrossMassInttraResponse);
            }

            // TO DO: send Email Notification
        }


        // Create single Transaction history for single operation
        String description = "";
        if (OperationType.SUBMIT.equals(verifiedGrossMassInttraRequest.getOperationType())) {
            description = "Booking Requested by : " + UserContext.getUser().getUsername();
        } else if (OperationType.AMEND.equals(verifiedGrossMassInttraRequest.getOperationType())) {
            description = "Amend Requested by : " + UserContext.getUser().getUsername();
        }
        createTransactionHistory(verifiedGrossMass.getStatus().getDescription(),
                FlowType.Inbound, description, SourceSystem.CargoRunner, verifiedGrossMassInttraRequest.getId());
    }

    private boolean isBridgeServiceResponseNotValid(BridgeServiceResponse bridgeServiceResponse) {
        return bridgeServiceResponse.getExtraResponseParams().containsKey(AwbConstants.SERVICE_HTTP_STATUS_CODE) && !Objects.equals(bridgeServiceResponse.getExtraResponseParams().get(AwbConstants.SERVICE_HTTP_STATUS_CODE).toString(), "200") &&
                !Objects.equals(bridgeServiceResponse.getExtraResponseParams().get(AwbConstants.SERVICE_HTTP_STATUS_CODE).toString(), "400");
    }

    private void createTransactionHistory(String actionStatus, FlowType flowType, String description, SourceSystem sourceSystem, Long id) {
        TransactionHistory transactionHistory = TransactionHistory.builder()
                .actionStatusDescription(actionStatus)
                .flowType(flowType)
                .description(description)
                .sourceSystem(sourceSystem)
                .actualDateTime(LocalDateTime.now())
                .entityType(EntityTypeTransactionHistory.VGM)
                .entityId(id)
                .build();
        transactionHistoryDao.save(transactionHistory);
    }

    private PartiesResponse fetchRequiredParty(Parties party) {
        if (Objects.isNull(party)) {
            return null;
        }

        return PartiesResponse.builder()
                .id(party.getId())
                .entityId(party.getEntityId())
                .entityType(party.getEntityType())
                .type(party.getType())
                .orgCode(party.getOrgCode())
                .tenantId(party.getTenantId())
                .addressCode(party.getAddressCode())
                .orgId(party.getOrgId())
                .addressId(party.getAddressId())
                .orgData(party.getOrgData())
                .addressData(party.getAddressData())
                .isAddressFreeText(party.getIsAddressFreeText())
                .countryCode(party.getCountryCode())
                .build();
    }

    private String populateRequestorEmails(VerifiedGrossMass verifiedGrossMass) {

        List<String> requestorEmailsList = new ArrayList<>();
        // Add existing external emails if any
        if (Objects.nonNull(verifiedGrossMass.getExternalEmails()) && !verifiedGrossMass.getExternalEmails().isBlank()) {
            String[] externalEmails = verifiedGrossMass.getExternalEmails().split(";");
            for (String email : externalEmails) {
                if (!email.isBlank()) {
                    requestorEmailsList.add(email.trim());
                }
            }
        }

        // Add createdBy and submitBy emails if present
        if (Objects.nonNull(verifiedGrossMass.getCreateByUserEmail()) && !verifiedGrossMass.getCreateByUserEmail().isBlank()) {
            requestorEmailsList.add(verifiedGrossMass.getCreateByUserEmail().trim());
        }

        if (Objects.nonNull(verifiedGrossMass.getSubmitByUserEmail()) && !verifiedGrossMass.getSubmitByUserEmail().isBlank()) {
            requestorEmailsList.add(verifiedGrossMass.getSubmitByUserEmail().trim());
        }

        return String.join(";", requestorEmailsList);
    }
}

