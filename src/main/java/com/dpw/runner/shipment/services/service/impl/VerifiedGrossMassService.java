package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.CarrierBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IVerifiedGrossMassDao;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SubmitAmendInttraRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.VerifiedGrossMassRequest;
import com.dpw.runner.shipment.services.dto.response.FieldClassDto;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.NotificationContactResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ReferenceNumberResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.SailingInformationResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassBulkUpdateRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassInttraResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassListResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassResponse;
import com.dpw.runner.shipment.services.dto.v3.request.VgmCancelRequest;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.FlowType;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
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
import com.dpw.runner.shipment.services.kafka.dto.inttra.Error;
import com.dpw.runner.shipment.services.kafka.dto.inttra.VgmEventDto;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.projection.CarrierBookingInfoProjection;
import com.dpw.runner.shipment.services.repository.interfaces.ICommonContainersRepository;
import com.dpw.runner.shipment.services.service.interfaces.IVerifiedGrossMassService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.FieldUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingInttraUtil;
import com.dpw.runner.shipment.services.utils.v3.VerifiedGrossMassUtil;
import com.dpw.runner.shipment.services.utils.v3.VerifiedGrossMassValidationUtil;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import jakarta.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import jakarta.transaction.Transactional;
import jakarta.validation.Valid;
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
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_EMAIL_TEMPLATE;
import static com.dpw.runner.shipment.services.entity.enums.VerifiedGrossMassStatus.CancelledRequested;
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
    private final VerifiedGrossMassUtil verifiedGrossMassUtil;
    private final CarrierBookingInttraUtil carrierBookingInttraUtil;
    private final INotificationService notificationService;
    private final IContainerDao containerDao;


    public VerifiedGrossMassService(IVerifiedGrossMassDao verifiedGrossMassDao, JsonHelper jsonHelper, CarrierBookingDao carrierBookingDao, IConsolidationDetailsDao consolidationDetailsDao, CommonUtils commonUtils,
                                    MasterDataUtils masterDataUtils, @Qualifier("executorServiceMasterData") ExecutorService executorServiceMasterData, VerifiedGrossMassMasterDataHelper verifiedGrossMassMasterDataHelper,
                                    ICommonContainersRepository commonContainersRepository, VerifiedGrossMassValidationUtil verifiedGrossMassValidationUtil, INotificationService notificationService,
                                    VerifiedGrossMassUtil verifiedGrossMassUtil, CarrierBookingInttraUtil carrierBookingInttraUtil, IContainerDao containerDao) {
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
        this.notificationService = notificationService;
        this.verifiedGrossMassUtil = verifiedGrossMassUtil;
        this.carrierBookingInttraUtil = carrierBookingInttraUtil;
        this.containerDao = containerDao;
    }

    @Override
    @Transactional
    public VerifiedGrossMassResponse create(VerifiedGrossMassRequest request) {
        Object entity = verifiedGrossMassValidationUtil.validateRequest(request.getEntityType(), request.getEntityId());
        VerifiedGrossMass verifiedGrossMass = jsonHelper.convertValue(request, VerifiedGrossMass.class);
        updateReadOnlyDataToEntity(request, entity, verifiedGrossMass);
        verifiedGrossMass.setStatus(VerifiedGrossMassStatus.Draft);
        verifiedGrossMass.setCreateByUserEmail(UserContext.getUser().getEmail());
        verifiedGrossMass.setSubmitByUserEmail(UserContext.getUser().getEmail());
        verifiedGrossMass.setInternalEmails(carrierBookingInttraUtil.parseEmailListToString(request.getInternalEmailsList()));
        verifiedGrossMass.setExternalEmails(carrierBookingInttraUtil.parseEmailListToString(request.getExternalEmailsList()));
        VerifiedGrossMass savedEntity = verifiedGrossMassDao.save(verifiedGrossMass);
        VerifiedGrossMassResponse verifiedGrossMassResponse = jsonHelper.convertValue(savedEntity, VerifiedGrossMassResponse.class);
        verifiedGrossMassResponse.setInternalEmailsList(carrierBookingInttraUtil.parseEmailStringToList(verifiedGrossMass.getInternalEmails()));
        verifiedGrossMassResponse.setExternalEmailsList(carrierBookingInttraUtil.parseEmailStringToList(verifiedGrossMass.getExternalEmails()));
        return verifiedGrossMassResponse;
    }

    @Override
    public VerifiedGrossMassResponse retrieveById(Long id) {
        Optional<VerifiedGrossMass> verifiedGrossMass = verifiedGrossMassDao.findById(id);
        if (verifiedGrossMass.isEmpty()) {
            throw new ValidationException("Invalid vgm id");
        }
        VerifiedGrossMass verifiedGrossMassEntity = verifiedGrossMass.get();
        VerifiedGrossMassResponse verifiedGrossMassResponse = jsonHelper.convertValue(verifiedGrossMassEntity, VerifiedGrossMassResponse.class);
        verifiedGrossMassResponse.setInternalEmailsList(
                carrierBookingInttraUtil.parseEmailStringToList(verifiedGrossMassEntity.getInternalEmails()));
        verifiedGrossMassResponse.setExternalEmailsList(
                carrierBookingInttraUtil.parseEmailStringToList(verifiedGrossMassEntity.getExternalEmails()));
        if (EntityType.CARRIER_BOOKING.equals(verifiedGrossMassEntity.getEntityType())) {
            CarrierBookingInfoProjection carrierBookingInfo = carrierBookingDao.findCarrierBookingInfoById(verifiedGrossMassEntity.getEntityId());
            if (Objects.nonNull(carrierBookingInfo)) {
                verifiedGrossMassResponse.setBookingStatus(carrierBookingInfo.getBookingStatus() != null ? CarrierBookingStatus.valueOf(carrierBookingInfo.getBookingStatus()) : null);
                verifiedGrossMassResponse.setSiStatus(carrierBookingInfo.getSiStatus() != null ? ShippingInstructionStatus.valueOf(carrierBookingInfo.getSiStatus()) : null);
            }
        }

        // Building Container Difference Response
        if (EntityType.CONSOLIDATION.equals(verifiedGrossMassEntity.getEntityType())) {
            ConsolidationDetails consolidationDetails = carrierBookingInttraUtil.getConsolidationDetail(verifiedGrossMassEntity.getEntityId());

            // Set mismatched containers
            verifiedGrossMassResponse.setConsolContainerWarningResponseList(verifiedGrossMassUtil.compareVGMContainers(
                    verifiedGrossMassEntity.getContainersList(), null, consolidationDetails.getContainersList()));
            verifiedGrossMassResponse.setVgmContainerWarningResponseList(verifiedGrossMassUtil.compareVGMContainers(
                    verifiedGrossMassEntity.getContainersList(), verifiedGrossMassEntity.getSubmittedContainersList(), null));


        } else if (EntityType.CARRIER_BOOKING.equals(verifiedGrossMassEntity.getEntityType())) {
            Optional<CarrierBooking> optionalCarrierBooking = carrierBookingDao.findById(verifiedGrossMassEntity.getEntityId());
            if (optionalCarrierBooking.isPresent()
                    && EntityType.CONSOLIDATION.toString().equalsIgnoreCase(optionalCarrierBooking.get().getEntityType())) {
                ConsolidationDetails consolidationDetails = carrierBookingInttraUtil.getConsolidationDetail(optionalCarrierBooking.get().getEntityId());

                // Set mismatched containers
                verifiedGrossMassResponse.setConsolContainerWarningResponseList(verifiedGrossMassUtil.compareVGMContainers(
                        verifiedGrossMassEntity.getContainersList(), null, consolidationDetails.getContainersList()));
                verifiedGrossMassResponse.setVgmContainerWarningResponseList(verifiedGrossMassUtil.compareVGMContainers(
                        verifiedGrossMassEntity.getContainersList(), verifiedGrossMassEntity.getSubmittedContainersList(), null));
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

        Page<VerifiedGrossMass> verifiedGrossMassPage = getVerifiedGrossMasses(listCommonRequest);
        log.info(VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());


        List<IRunnerResponse> filteredList = convertEntityListToDtoList(verifiedGrossMassPage.getContent(), getMasterData);

        return ResponseHelper.buildListSuccessResponse(
                filteredList,
                verifiedGrossMassPage.getTotalPages(),
                verifiedGrossMassPage.getTotalElements());
    }

    @Override
    public Page<VerifiedGrossMass> getVerifiedGrossMasses(ListCommonRequest listCommonRequest) {
        Pair<Specification<VerifiedGrossMass>, Pageable> tuple = fetchData(listCommonRequest, VerifiedGrossMass.class, VerifiedGrossMassConstants.tableNames);
        return verifiedGrossMassDao.findAll(tuple.getLeft(), tuple.getRight());
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<VerifiedGrossMass> verifiedGrossMassList, boolean getMasterData) {
        List<VerifiedGrossMassListResponse> verifiedGrossMassListResponses = new ArrayList<>();

        for (VerifiedGrossMass verifiedGrossMass : verifiedGrossMassList) {
            VerifiedGrossMassListResponse verifiedGrossMassListResponse = jsonHelper.convertValue(verifiedGrossMass, VerifiedGrossMassListResponse.class);
            verifiedGrossMassListResponse.setInternalEmailsList(
                    carrierBookingInttraUtil.parseEmailStringToList(verifiedGrossMass.getInternalEmails()));
            verifiedGrossMassListResponse.setExternalEmailsList(
                    carrierBookingInttraUtil.parseEmailStringToList(verifiedGrossMass.getExternalEmails()));
            verifiedGrossMassListResponses.add(verifiedGrossMassListResponse);
        }
        List<IRunnerResponse> responseList = new ArrayList<>(verifiedGrossMassListResponses);
        verifiedGrossMassMasterDataHelper.getMasterDataForList(responseList, getMasterData, false);

        return responseList;
    }

    @Override
    @Transactional
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
        Object entity = verifiedGrossMassValidationUtil.validateRequest(request.getEntityType(), request.getEntityId());
        //update header information from existing entity
        VerifiedGrossMass verifiedGrossMass = jsonHelper.convertValue(request, VerifiedGrossMass.class);
        if (Objects.nonNull(verifiedGrossMassEntity.getContainersList())) {
            Map<Long, CommonContainers> existingVGMContainersMap = verifiedGrossMassEntity.getContainersList().stream()
                    .filter(c -> c.getId() != null)
                    .collect(Collectors.toMap(CommonContainers::getId, c -> c));

            for (CommonContainers incomingContainer : verifiedGrossMass.getContainersList()) {
                if (Objects.isNull(incomingContainer.getId()) || Objects.isNull(existingVGMContainersMap.get(incomingContainer.getId())))
                    continue;
                incomingContainer.setContainerRefGuid(existingVGMContainersMap.get(incomingContainer.getId()).getContainerRefGuid());
            }
        }

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
        //Set Internal And External Emails
        verifiedGrossMass.setInternalEmails(carrierBookingInttraUtil.parseEmailListToString(request.getInternalEmailsList()));
        verifiedGrossMass.setExternalEmails(carrierBookingInttraUtil.parseEmailListToString(request.getExternalEmailsList()));
        VerifiedGrossMass savedEntity = verifiedGrossMassDao.save(verifiedGrossMass);
        VerifiedGrossMassResponse verifiedGrossMassResponse = jsonHelper.convertValue(savedEntity, VerifiedGrossMassResponse.class);
        verifiedGrossMassResponse.setInternalEmailsList(carrierBookingInttraUtil.parseEmailStringToList(verifiedGrossMass.getInternalEmails()));
        verifiedGrossMassResponse.setExternalEmailsList(carrierBookingInttraUtil.parseEmailStringToList(verifiedGrossMass.getExternalEmails()));
        return verifiedGrossMassResponse;
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
            carrierBooking.setVerifiedGrossMass(verifiedGrossMass);
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

            verifiedGrossMassResponse.setInternalEmailsList(carrierBookingInttraUtil.parseEmailStringToList(verifiedGrossMass.getInternalEmails()));
            verifiedGrossMassResponse.setExternalEmailsList(carrierBookingInttraUtil.parseEmailStringToList(verifiedGrossMass.getExternalEmails()));
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
        // Pre-populate Internal and External emails from Carrier Booking if VGM is not standalone
        // Internal Emails
        verifiedGrossMassResponse.setInternalEmailsList(
                carrierBookingInttraUtil.parseEmailStringToList(carrierBooking.getInternalEmails()));
        // External Emails
        verifiedGrossMassResponse.setExternalEmailsList(
                carrierBookingInttraUtil.parseEmailStringToList(carrierBooking.getExternalEmails()));
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
        commonContainers.setContainerRefGuid(containers.getGuid());
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

    public void submitOrAmendVerifiedGrossMass(SubmitAmendInttraRequest submitAmendInttraRequest) throws RunnerException {

        Optional<VerifiedGrossMass> verifiedGrossMassOptional = verifiedGrossMassDao.findById(submitAmendInttraRequest.getId());
        if (verifiedGrossMassOptional.isEmpty()) {
            throw new ValidationException("Invalid VGM Id: " + submitAmendInttraRequest.getId());
        }

        List<CommonContainers> containersList =
                commonContainersRepository.findAllByIdIn(submitAmendInttraRequest.getContainerIds());
        VerifiedGrossMass verifiedGrossMass = verifiedGrossMassOptional.get();

        // Set Status to Draft
        verifiedGrossMass.setStatus(VerifiedGrossMassStatus.Draft);
        sendNotification(verifiedGrossMass);
        saveTransactionHistory(submitAmendInttraRequest, verifiedGrossMass);

        // Creating List of submittedContainers
        List<CommonContainers> submittedContainersList = new ArrayList<>();

        for (CommonContainers container : containersList) {

            VerifiedGrossMassInttraResponse verifiedGrossMassInttraResponse = new VerifiedGrossMassInttraResponse();

            // Set Message Date Time
            verifiedGrossMassInttraResponse.setMessageGuid(UUID.randomUUID());
            verifiedGrossMassInttraResponse.setMessageDateTime(LocalDateTime.now());
            verifiedGrossMassInttraResponse.setTenantId(VerifiedGrossMassConstants.INTTRA);

            // Setting Submitter Party
            verifiedGrossMassInttraResponse.setRequestor(carrierBookingInttraUtil.fetchRequiredParty(verifiedGrossMass.getRequestor()));

            NotificationContactResponse notificationContractResponse = new NotificationContactResponse();
            notificationContractResponse.setUsername(UserContext.getUser().getUsername());
            notificationContractResponse.setEmails(verifiedGrossMassUtil.populateRequestorEmails(verifiedGrossMass));
            verifiedGrossMassInttraResponse.setRequestorNotificationContact(notificationContractResponse);

            // Set Container details
            verifiedGrossMassInttraResponse.setContainer(verifiedGrossMassUtil.buildContainerResponse(container));

            // Set Other parties
            verifiedGrossMassInttraResponse.setResponsible(carrierBookingInttraUtil.fetchRequiredParty(verifiedGrossMass.getResponsible()));
            verifiedGrossMassInttraResponse.setAuthorised(carrierBookingInttraUtil.fetchRequiredParty(verifiedGrossMass.getAuthorised()));

            // Set carrier carrier booking details
            verifiedGrossMassInttraResponse.setCarrierBookingNo(verifiedGrossMass.getCarrierBookingNo());
            verifiedGrossMassInttraResponse.setSubmitterReference(verifiedGrossMass.getCarrierBookingNo());

            verifiedGrossMassUtil.populateCarrierDetails(
                    verifiedGrossMassUtil.fetchCarrierDetailsForBridgePayload(verifiedGrossMass),
                    verifiedGrossMassInttraResponse, verifiedGrossMass.getExternalEmails(), verifiedGrossMass.getOtherExternalEmails());

            // Generates number between 10000 and 99999 and set fileName
            SecureRandom random = new SecureRandom();
            int rnd = 10000 + random.nextInt(90000);
            String fileName = "VGMRequest_" + submitAmendInttraRequest.getId() + "_" + rnd + ".xml";
            verifiedGrossMassInttraResponse.setFileName(fileName);

            verifiedGrossMassInttraResponse.setIsDelegated(verifiedGrossMass.getIsDelegated());

            verifiedGrossMass.setStatus(VerifiedGrossMassStatus.Requested);
            // Create single Transaction history for single operation
            saveTransactionHistory(submitAmendInttraRequest, verifiedGrossMass);

            // Set Response State
            if (OperationType.SUBMIT.equals(submitAmendInttraRequest.getOperationType())) {
                verifiedGrossMassInttraResponse.setState(VerifiedGrossMassConstants.ORIGINAL);
                // Sending Payload To Bridge
                carrierBookingInttraUtil.sendPayloadToBridge(verifiedGrossMassInttraResponse, verifiedGrossMass.getId(),
                        VerifiedGrossMassConstants.VGM_CREATE, UUID.randomUUID().toString(), UUID.randomUUID().toString(), IntegrationType.BRIDGE_VGM_SUBMIT, EntityTypeTransactionHistory.VGM.name());
            } else if (OperationType.AMEND.equals(submitAmendInttraRequest.getOperationType())) {
                verifiedGrossMassInttraResponse.setState(VerifiedGrossMassConstants.AMEND);
                // Sending Payload To Bridge
                carrierBookingInttraUtil.sendPayloadToBridge(verifiedGrossMassInttraResponse, verifiedGrossMass.getId(),
                        VerifiedGrossMassConstants.VGM_AMEND, UUID.randomUUID().toString(), UUID.randomUUID().toString(), IntegrationType.BRIDGE_VGM_AMEND, EntityTypeTransactionHistory.VGM.name());
            }

            // Building Submit Containers
            submittedContainersList.add(verifiedGrossMassUtil.buildSubmittedContainer(container));

            sendNotification(verifiedGrossMass);
        }

        // Storing in VGM
        verifiedGrossMass.setSubmittedContainersList(submittedContainersList);
        verifiedGrossMassDao.save(verifiedGrossMass);
    }

    @Override
    public void updateVgmStatus(VgmEventDto vgm) {
        String carrierBookingNo = vgm.getBookingNumber();
        String containerNumber = vgm.getContainerNumber();
        VerifiedGrossMass verifiedGrossMass = verifiedGrossMassDao.findByCarrierBookingNo(carrierBookingNo);
        if (Objects.nonNull(verifiedGrossMass)) {
            List<CommonContainers> containersList = verifiedGrossMass.getContainersList();
            if (!CollectionUtils.isEmpty(containersList)) {
                updateVGMContainersStatus(vgm, containersList, containerNumber, verifiedGrossMass);
            }
        }
    }

    private void updateVGMContainersStatus(VgmEventDto vgm, List<CommonContainers> containersList, String containerNumber, VerifiedGrossMass verifiedGrossMass) {
        for (CommonContainers container : containersList) {
            if (container.getContainerNo() != null && container.getContainerNo().equals(containerNumber)) {
                String status = vgm.getStatus();
                VerifiedGrossMassStatus verifiedGrossMassStatus = parseIntraStatus(status);
                container.setVgmStatus(verifiedGrossMassStatus.name());
                List<Error> errors = new ArrayList<>();
                if (VerifiedGrossMassStatus.RejectedByINTTRA.equals(verifiedGrossMassStatus)) {
                    errors = vgm.getErrors();
                }
                carrierBookingInttraUtil.createTransactionHistory(verifiedGrossMassStatus.getDescription(),
                        FlowType.Outbound, generateErrorMessage(errors), SourceSystem.INTTRA,
                        verifiedGrossMass.getId(), EntityTypeTransactionHistory.VGM);
                commonContainersRepository.save(container);
                sendNotification(verifiedGrossMass);
                break;
            }
        }
    }

    public String generateErrorMessage(List<Error> errors) {
        if (errors == null || errors.isEmpty()) {
            return Constants.EMPTY_STRING;
        }

        return errors.stream()
                .map(Error::getErrorDetails)
                .collect(Collectors.joining("| "));
    }

    private static VerifiedGrossMassStatus parseIntraStatus(String type) {

        return switch (type) {
            case "ConditionallyAccepted" -> VerifiedGrossMassStatus.ConditionallyAccepted;
            case "Accepted" -> VerifiedGrossMassStatus.AcceptedByINTTRA;
            case "Rejected" -> VerifiedGrossMassStatus.RejectedByINTTRA;
            case "Replaced" -> VerifiedGrossMassStatus.ReplacedByCarrier;
            case "Changed" -> VerifiedGrossMassStatus.Changed;
            case "Acknowledged" -> VerifiedGrossMassStatus.Acknowledged;
            default -> VerifiedGrossMassStatus.PendingFromCarrier;
        };

    }

    private void saveTransactionHistory(SubmitAmendInttraRequest submitAmendInttraRequest, VerifiedGrossMass verifiedGrossMass) {
        String description = "";
        if (OperationType.SUBMIT.equals(submitAmendInttraRequest.getOperationType())) {
            description = "Booking Requested by : " + UserContext.getUser().getUsername();
        } else if (OperationType.AMEND.equals(submitAmendInttraRequest.getOperationType())) {
            description = "Amend Requested by : " + UserContext.getUser().getUsername();
        }
        carrierBookingInttraUtil.createTransactionHistory(verifiedGrossMass.getStatus().getDescription(),
                FlowType.Inbound, description, SourceSystem.CargoRunner, submitAmendInttraRequest.getId(), EntityTypeTransactionHistory.VGM);
    }

    // Called whenever the Status is Changed
    protected void sendNotification(VerifiedGrossMass verifiedGrossMass) {
        try {
            List<String> requests = new ArrayList<>(List.of(VERIFIED_GROSS_MASS_EMAIL_TEMPLATE));
            List<EmailTemplatesRequest> emailTemplates = carrierBookingInttraUtil.fetchEmailTemplate(requests);
            EmailTemplatesRequest verifiedGrossMassEmailTemplate = emailTemplates.stream()
                    .filter(Objects::nonNull)
                    .filter(template -> VERIFIED_GROSS_MASS_EMAIL_TEMPLATE.equalsIgnoreCase(template.getType()))
                    .findFirst()
                    .orElse(null);
            if (Objects.nonNull(verifiedGrossMassEmailTemplate)) {
                List<String> toEmails = verifiedGrossMassUtil.getSendEmailBaseRequest(verifiedGrossMass);
                notificationService.sendEmail(verifiedGrossMassEmailTemplate.getBody(), verifiedGrossMassEmailTemplate.getSubject(), toEmails, new ArrayList<>());
                log.info("Email Notification sent successfully for State Change of VGM Id: {}", verifiedGrossMass.getId());
            }
        } catch (Exception e) {
            log.error("Error in sending verified gross mass email: {}", e.getMessage());
        }
    }

    public List<CommonContainerResponse> syncContainersByIds(List<Long> commonContainerIds) {
        List<CommonContainers> commonContainersList = commonContainersRepository.findAllById(commonContainerIds);

        // Extract all containerRefGuids
        List<UUID> refGuids = commonContainersList.stream()
                .map(CommonContainers::getContainerRefGuid)
                .filter(Objects::nonNull)
                .toList();

        // Fetch all matching Containers by GUID
        Map<UUID, Containers> containersMap = containerDao.findAllByGuid(refGuids).stream()
                .collect(Collectors.toMap(Containers::getGuid, Function.identity()));

        for (CommonContainers commonContainers : commonContainersList) {
            UUID refGuid = commonContainers.getContainerRefGuid();
            // No matching container to sync from
            if (Objects.isNull(refGuid) || !containersMap.containsKey(refGuid)) {
                continue;
            }

            Containers container = containersMap.get(refGuid);

            // Field sync
            commonContainers.setContainerCode(container.getContainerCode());
            commonContainers.setContainerNo(container.getContainerNumber());

            // Setting Cargo Weight (i.e Net weight)
            commonContainers.setNetWeight(container.getNetWeight());

            commonContainers.setTareWeight(container.getTareWeight());
            commonContainers.setTareWeightUnit(container.getTareWeightUnit());
            commonContainers.setGrossWeight(container.getGrossWeight());
            commonContainers.setGrossWeightUnit(container.getGrossWeightUnit());
            commonContainers.setSealNumber(container.getCarrierSealNumber());
            commonContainers.setShipperSealNumber(container.getShipperSealNumber());
            commonContainers.setCustomsSealNumber(container.getCustomsSealNumber());
            commonContainers.setVeterinarySealNumber(container.getVeterinarySealNumber());
            commonContainers.setVgmWeight(container.getGrossWeight());
            commonContainers.setVgmWeightUnit(container.getGrossWeightUnit());
        }

        // Save updated entities
        commonContainersRepository.saveAll(commonContainersList);
        return commonContainersList.stream()
                .map(container -> jsonHelper.convertValue(container, CommonContainerResponse.class))
                .toList();
    }

    @Override
    public void cancelVerifiedGrossMass(@Valid VgmCancelRequest vgmCancelRequest) {
        List<Long> cancelContainerIds = vgmCancelRequest.getContainersIds();

        List<CommonContainers> commonContainers = commonContainersRepository.findAllByIdIn(cancelContainerIds);
        if (CollectionUtils.isEmpty(commonContainers)) {
            throw new ValidationException("Invalid containers id: " + cancelContainerIds);
        }
        if (cancelContainerIds.size() != commonContainers.size()) {
            throw new ValidationException("Some of the invalid containers id : " + cancelContainerIds);
        }
        List<String> cancelledContainerNumbers = commonContainers.stream()
                .filter(container -> Objects.equals(container.getVgmStatus(), VerifiedGrossMassStatus.Cancelled.name()))
                .map(CommonContainers::getContainerNo)
                .toList();
        if (!CollectionUtils.isEmpty(cancelledContainerNumbers)) {
            throw new ValidationException("Some containers are already cancelled/CancelledRequested." + String.join(",", cancelledContainerNumbers));
        }

        Long verifiedGrossMassId = commonContainers.get(0).getVerifiedGrossMassId();
        VerifiedGrossMass verifiedGrossMass = verifiedGrossMassDao.findById(verifiedGrossMassId)
                .orElseThrow(() -> new ValidationException("Invalid verified gross mass id: " + verifiedGrossMassId));
        commonContainers
                .forEach(container -> container.setVgmStatus(CancelledRequested.name()));
        List<CommonContainers> containersList = verifiedGrossMass.getContainersList();
        containersList.stream()
                .filter(container -> cancelContainerIds.contains(container.getId()))
                .forEach(container -> container.setVgmStatus(CancelledRequested.name()));
        boolean allCancelled = containersList.stream()
                .allMatch(c -> Objects.equals(c.getVgmStatus(), VerifiedGrossMassStatus.Cancelled.name()));

        if (allCancelled) {
            verifiedGrossMass.setStatus(VerifiedGrossMassStatus.Cancelled);
        }
        VerifiedGrossMass verifiedGrossMassEntity = verifiedGrossMassDao.save(verifiedGrossMass);

        log.info("Vgm with id :{} cancelled ", verifiedGrossMassEntity.getId());
        commonContainers.forEach(containers -> carrierBookingInttraUtil.createTransactionHistory(CancelledRequested.getDescription(), FlowType.Inbound, CancelledRequested.getDescription() + "by: " + UserContext.getUser().getUsername() + " for container no: " + containers.getContainerNo() + " Container Code: " + containers.getContainerCode(), SourceSystem.CargoRunner, containers.getId(), EntityTypeTransactionHistory.VGM));
    }
}

