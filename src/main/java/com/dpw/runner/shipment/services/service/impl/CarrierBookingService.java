package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.config.BridgeServiceConfig;
import com.dpw.runner.shipment.services.adapters.impl.BridgeServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITransactionHistoryDao;
import com.dpw.runner.shipment.services.dto.request.BulkUpdateRoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingBridgeRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SubmitAmendInttraRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SyncBookingToService;
import com.dpw.runner.shipment.services.dto.response.FieldClassDto;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.bridgeService.BridgeServiceResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingCloneResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingListResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ContainerMisMatchWarning;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ReferenceNumberResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.SailingInformationResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassListResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.CarrierRouting;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.FlowType;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.OperationType;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionStatus;
import com.dpw.runner.shipment.services.entity.enums.SourceSystem;
import com.dpw.runner.shipment.services.entity.enums.VerifiedGrossMassStatus;
import com.dpw.runner.shipment.services.exception.exceptions.InttraFailureException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.CarrierBookingMasterDataHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.helpers.ShippingInstructionMasterDataHelper;
import com.dpw.runner.shipment.services.helpers.VerifiedGrossMassMasterDataHelper;
import com.dpw.runner.shipment.services.kafka.dto.inttra.DateInfo;
import com.dpw.runner.shipment.services.kafka.dto.inttra.Equipment;
import com.dpw.runner.shipment.services.kafka.dto.inttra.Haulage;
import com.dpw.runner.shipment.services.kafka.dto.inttra.HaulageDate;
import com.dpw.runner.shipment.services.kafka.dto.inttra.HaulageParty;
import com.dpw.runner.shipment.services.kafka.dto.inttra.HaulagePartyDto;
import com.dpw.runner.shipment.services.kafka.dto.inttra.HaulagePoint;
import com.dpw.runner.shipment.services.kafka.dto.inttra.InttraCarrierBookingEventDto;
import com.dpw.runner.shipment.services.kafka.dto.inttra.Location;
import com.dpw.runner.shipment.services.kafka.dto.inttra.LocationDate;
import com.dpw.runner.shipment.services.kafka.dto.inttra.Reference;
import com.dpw.runner.shipment.services.kafka.dto.inttra.TransportLeg;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierBookingService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IShippingInstructionsService;
import com.dpw.runner.shipment.services.service.interfaces.IVerifiedGrossMassService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.FieldUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingInttraUtil;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingUtil;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingValidationUtil;
import com.fasterxml.jackson.databind.JsonNode;
import com.nimbusds.jose.util.Pair;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
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
import java.util.stream.StreamSupport;

import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.BOOKING_DETAILS;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_BOOKING_ADDITIONAL_PARTIES;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_REQUEST_EMPTY_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_REQUEST_NULL_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_RESPONSE_SUCCESS;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_REFERENCE_NUMBER;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.ERROR_MESSAGES;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.ERR_INTTRA_MISSING_KEY;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.INTTRA_REFERENCE;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.INVALID_CARRIER_BOOKING_ID;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.MESSAGE;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.PAYLOAD;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.SERVICE_HTTP_STATUS_CODE;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.SERVICE_RESPONSE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARRIER_BOOKING_INTTRA_AMEND;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARRIER_BOOKING_INTTRA_CREATE;
import static com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus.Changed;
import static com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus.Requested;
import static com.dpw.runner.shipment.services.entity.enums.IntegrationType.BRIDGE_CB_AMEND;
import static com.dpw.runner.shipment.services.entity.enums.IntegrationType.BRIDGE_CB_SUBMIT;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Slf4j
@Service
@RequiredArgsConstructor
public class CarrierBookingService implements ICarrierBookingService {

    private final ICarrierBookingDao carrierBookingDao;
    private final JsonHelper jsonHelper;
    private final CarrierBookingMasterDataHelper carrierBookingMasterDataHelper;
    private final CarrierBookingValidationUtil carrierBookingValidationUtil;
    private final CommonUtils commonUtils;
    private final INotificationService notificationService;
    private final CarrierBookingUtil carrierBookingUtil;
    private final MasterDataUtils masterDataUtils;
    private final ExecutorService executorServiceMasterData;
    private final IConsolidationDetailsDao consolidationDetailsDao;
    private final IConsolidationV3Service consolidationV3Service;
    private final IShipmentDao shipmentDao;
    private final IPartiesDao partiesDao;
    private final ITransactionHistoryDao transactionHistoryDao;
    private final BridgeServiceAdapter bridgeServiceAdapter;
    private final BridgeServiceConfig bridgeServiceConfig;
    private final CarrierBookingInttraUtil carrierBookingInttraUtil;
    private final IRoutingsV3Service routingsV3Service;
    private final VerifiedGrossMassMasterDataHelper verifiedGrossMassMasterDataHelper;
    private final ShippingInstructionMasterDataHelper shippingInstructionMasterDataHelper;
    @Lazy
    @Autowired
    private IVerifiedGrossMassService verifiedGrossMassService;
    @Lazy
    @Autowired
    private IShippingInstructionsService shippingInstructionsService;


    @Override
    public CarrierBookingResponse create(CarrierBookingRequest request) throws RunnerException {
        log.info("CarrierBookingService.create() called with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        carrierBookingValidationUtil.validateServiceType(request);
        Object entity = carrierBookingValidationUtil.validateRequest(request.getEntityType(), request.getEntityId());
        CarrierBooking carrierBookingEntity = jsonHelper.convertValue(request, CarrierBooking.class);
        carrierBookingEntity.setCreateByUserEmail(UserContext.getUser().getEmail());

        // Set Internal and External Emails
        setInternalExternalEmailsInDB(carrierBookingEntity, request);

        if (Constants.CONSOLIDATION.equalsIgnoreCase(request.getEntityType())) {
            ConsolidationDetails consolidationDetails = (ConsolidationDetails) entity;
            carrierBookingEntity.setEntityNumber(consolidationDetails.getConsolidationNumber());
            //read only fields
            SailingInformation sailingInformation = carrierBookingEntity.getSailingInformation();
            if (Objects.isNull(sailingInformation)) {
                sailingInformation = new SailingInformation();
            }
            if (consolidationDetails.getCarrierDetails() != null) {
                sailingInformation.setPol(consolidationDetails.getCarrierDetails().getOriginPort());
                sailingInformation.setPod(consolidationDetails.getCarrierDetails().getDestinationPort());
                sailingInformation.setCarrierReceiptPlace(consolidationDetails.getCarrierDetails().getOrigin());
                sailingInformation.setCarrierDeliveryPlace(consolidationDetails.getCarrierDetails().getDestination());
            }
            carrierBookingUtil.mapConsolidationToSailing(consolidationDetails, sailingInformation);
            carrierBookingEntity.setSailingInformation(sailingInformation);
        }
        carrierBookingEntity.setCarrierRoutingList(null);// we will get it from carrier
        carrierBookingEntity.setLoadedContainerDropOffDetails(null); // we will get it from carrier
        carrierBookingEntity.setEmptyContainerPickupDetails(null); // we will get it from carrier
        carrierBookingEntity.setCarrierComment(null); //we will get it from carrier
        carrierBookingUtil.generateBookingNumber(carrierBookingEntity);
        carrierBookingEntity.setCarrierBlNo(null);
        carrierBookingEntity.setCarrierBookingNo(null);
        carrierBookingEntity.setStatus(CarrierBookingStatus.Draft);
        carrierBookingUtil.populateLocCode(carrierBookingInttraUtil.fetchUnLocationMap(carrierBookingEntity), carrierBookingEntity);

        CarrierBooking savedEntity = carrierBookingDao.create(carrierBookingEntity);

        if (request.getAdditionalParties() != null && !request.getAdditionalParties().isEmpty()) {
            partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(request.getAdditionalParties(), Parties.class, true), savedEntity.getId(), CARRIER_BOOKING_ADDITIONAL_PARTIES);
        }
        CarrierBookingResponse carrierBookingResponse = jsonHelper.convertValue(savedEntity, CarrierBookingResponse.class);
        setInternalExternalEmails(carrierBookingResponse, carrierBookingEntity);
        log.info("CarrierBookingService.create() successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(carrierBookingResponse));
        return carrierBookingResponse;
    }

    @Override
    public CarrierBookingResponse retrieveById(Long id) {
        log.info("CarrierBookingService.getById() called with RequestId: {} and id: {}",
                LoggerHelper.getRequestIdFromMDC(), id);
        CarrierBooking carrierBooking = carrierBookingDao.findById(id)
                .orElseThrow(() -> new ValidationException("Carrier Booking does not exists with id : " + id));

        carrierBooking.setCarrierComment(carrierBookingUtil.truncate(carrierBooking.getCarrierComment(), 10000));
        // consolidation fetch container, common container properties diff
        CarrierBookingResponse carrierBookingResponse = jsonHelper.convertValue(carrierBooking, CarrierBookingResponse.class);
        setInternalExternalEmails(carrierBookingResponse, carrierBooking);
        setVgmAndSiId(carrierBooking, carrierBookingResponse);
        mismatchDetection(carrierBooking, carrierBookingResponse);
        log.info("CarrierBookingService.getById() successful with RequestId: {} and response: {}",
                LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(carrierBookingResponse));
        return carrierBookingResponse;
    }

    /**
     * Populates the carrier booking response with IDs of Vgm and Si entities.
     *
     * @param carrierBooking         Entity carrier booking data
     * @param carrierBookingResponse updated response
     */
    private void setVgmAndSiId(CarrierBooking carrierBooking, CarrierBookingResponse carrierBookingResponse) {

        Optional.ofNullable(carrierBooking.getVerifiedGrossMass())
                .map(VerifiedGrossMass::getId)
                .ifPresent(carrierBookingResponse::setVgmId);

        Optional.ofNullable(carrierBooking.getShippingInstruction())
                .map(ShippingInstruction::getId)
                .ifPresent(carrierBookingResponse::setSiId);
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel, boolean getMasterData) {
        ListCommonRequest listCommonRequest = (ListCommonRequest) commonRequestModel.getData();
        if (listCommonRequest == null) {
            log.error(CARRIER_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException(CARRIER_LIST_REQUEST_NULL_ERROR);
        }

        Pair<Specification<CarrierBooking>, Pageable> tuple = fetchData(listCommonRequest, CarrierBooking.class, CarrierBookingConstants.tableNames);
        Page<CarrierBooking> carrierBookingPage = carrierBookingDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info(CARRIER_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());

        Set<String> includeColumns = new HashSet<>(Optional.ofNullable(listCommonRequest.getIncludeColumns())
                .orElse(Collections.emptyList()));

        List<IRunnerResponse> filteredList = convertEntityListToDtoList(carrierBookingPage.getContent(), getMasterData, includeColumns);

        return ResponseHelper.buildListSuccessResponse(
                filteredList,
                carrierBookingPage.getTotalPages(),
                carrierBookingPage.getTotalElements());
    }

    public List<IRunnerResponse> convertEntityListToDtoList(List<CarrierBooking> carrierBookingList, boolean getMasterData,
                                                            Set<String> includeColumns) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<CarrierBookingListResponse> carrierBookingListResponses = new ArrayList<>();

        for (CarrierBooking carrierBooking : carrierBookingList) {
            CarrierBookingListResponse carrierBookingListResponse = jsonHelper.convertValue(carrierBooking, CarrierBookingListResponse.class);
            carrierBookingListResponse.setInternalEmailsList(
                    carrierBookingInttraUtil.parseEmailStringToList(carrierBooking.getInternalEmails()));
            carrierBookingListResponse.setExternalEmailsList(
                    carrierBookingInttraUtil.parseEmailStringToList(carrierBooking.getExternalEmails()));

            if (carrierBooking.getShippingInstruction() != null) {
                carrierBookingListResponse.setSiStatus(carrierBooking.getShippingInstruction().getStatus());
            }
            if (carrierBooking.getVerifiedGrossMass() != null) {
                carrierBookingListResponse.setVgmStatus(carrierBooking.getVerifiedGrossMass().getStatus());
            }
            if (!CollectionUtils.isEmpty(carrierBooking.getReferenceNumbersList())) {
                Optional<ReferenceNumbers> contractReferenceNumber = carrierBooking.getReferenceNumbersList()
                        .stream()
                        .filter(ref -> CarrierBookingConstants.CON.equals(ref.getType()))
                        .findFirst();
                contractReferenceNumber.ifPresent(referenceNumbers -> carrierBookingListResponse.setContractNo(referenceNumbers.getReferenceNumber()));
            }
            carrierBookingListResponses.add(carrierBookingListResponse);
        }

        carrierBookingListResponses.forEach(responseList::add);
        carrierBookingMasterDataHelper.getMasterDataForList(carrierBookingList, responseList, getMasterData, true, includeColumns);
        return responseList;
    }

    @Override
    public CarrierBookingResponse update(CarrierBookingRequest request) throws RunnerException {
        log.info("CarrierBookingService.update() called with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        CarrierBooking existingCarrierBooking = carrierBookingDao.findById(request.getId()).orElseThrow(() -> new ValidationException("Invalid carrier booking Id"));
        carrierBookingValidationUtil.validateServiceType(request);
        Object entity = carrierBookingValidationUtil.validateRequest(request.getEntityType(), request.getEntityId());
        CarrierBooking carrierBookingEntity = jsonHelper.convertValue(request, CarrierBooking.class);
        carrierBookingEntity.setCreateByUserEmail(UserContext.getUser().getEmail());

        //Set Internal And External Emails
        setInternalExternalEmailsInDB(carrierBookingEntity, request);

        if (Constants.CONSOLIDATION.equalsIgnoreCase(request.getEntityType())) {
            ConsolidationDetails consolidationDetails = (ConsolidationDetails) entity;
            carrierBookingEntity.setEntityNumber(consolidationDetails.getConsolidationNumber());
            //read only fields
            SailingInformation sailingInformation = carrierBookingEntity.getSailingInformation();
            if (sailingInformation == null) {
                sailingInformation = new SailingInformation();
            }
            if (consolidationDetails.getCarrierDetails() != null) {
                sailingInformation.setPol(consolidationDetails.getCarrierDetails().getOriginPort());
                sailingInformation.setPod(consolidationDetails.getCarrierDetails().getDestinationPort());
                sailingInformation.setCarrierReceiptPlace(consolidationDetails.getCarrierDetails().getOrigin());
                sailingInformation.setCarrierDeliveryPlace(consolidationDetails.getCarrierDetails().getDestination());
            }
            carrierBookingUtil.mapConsolidationToSailing(consolidationDetails, sailingInformation);
            carrierBookingEntity.setSailingInformation(sailingInformation);
        }
        carrierBookingEntity.setCarrierRoutingList(existingCarrierBooking.getCarrierRoutingList());// we will get it from carrier
        carrierBookingEntity.setLoadedContainerDropOffDetails(existingCarrierBooking.getLoadedContainerDropOffDetails()); // we will get it from carrier
        carrierBookingEntity.setEmptyContainerPickupDetails(existingCarrierBooking.getEmptyContainerPickupDetails()); // we will get it from carrier
        carrierBookingEntity.setCarrierComment(existingCarrierBooking.getCarrierComment()); //we will get it from carrier
        carrierBookingEntity.setBookingNo(existingCarrierBooking.getBookingNo());
        carrierBookingEntity.setCarrierBlNo(existingCarrierBooking.getCarrierBlNo());
        carrierBookingEntity.setCarrierBookingNo(existingCarrierBooking.getCarrierBookingNo());
        if (!CarrierBookingStatus.ChangeDraft.equals(carrierBookingEntity.getStatus())) {
            carrierBookingEntity.setStatus(existingCarrierBooking.getStatus());
        }
        carrierBookingUtil.populateLocCode(carrierBookingInttraUtil.fetchUnLocationMap(carrierBookingEntity), carrierBookingEntity);
        CarrierBooking savedEntity = carrierBookingDao.create(carrierBookingEntity);

        if (request.getAdditionalParties() != null && !request.getAdditionalParties().isEmpty()) {
            partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(request.getAdditionalParties(), Parties.class, false), savedEntity.getId(), CARRIER_BOOKING_ADDITIONAL_PARTIES);
        }
        CarrierBookingResponse carrierBookingResponse = jsonHelper.convertValue(savedEntity, CarrierBookingResponse.class);

        setInternalExternalEmails(carrierBookingResponse, savedEntity);
        log.info("CarrierBookingService.update() successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(carrierBookingResponse));
        return carrierBookingResponse;
    }

    private void setInternalExternalEmails(CarrierBookingResponse carrierBookingResponse, CarrierBooking carrierBooking) {
        carrierBookingResponse.setInternalEmailsList(carrierBookingInttraUtil.parseEmailStringToList(carrierBooking.getInternalEmails()));
        carrierBookingResponse.setExternalEmailsList(carrierBookingInttraUtil.parseEmailStringToList(carrierBooking.getExternalEmails()));
    }

    private void setInternalExternalEmailsInDB(CarrierBooking carrierBookingEntity, CarrierBookingRequest carrierBookingRequest) {
        carrierBookingEntity.setInternalEmails(carrierBookingInttraUtil.parseEmailListToString(carrierBookingRequest.getInternalEmailsList()));
        carrierBookingEntity.setExternalEmails(carrierBookingInttraUtil.parseEmailListToString(carrierBookingRequest.getExternalEmailsList()));
    }

    @Override
    @Transactional
    public void delete(Long id) {
        log.info("CarrierBookingService.delete() called with RequestId: {} and id: {}", LoggerHelper.getRequestIdFromMDC(), id);
        CarrierBooking carrierBooking = carrierBookingDao.findById(id)
                .orElseThrow(() -> new ValidationException("CarrierBooking not found with Id: " + id));
        carrierBookingDao.delete(carrierBooking);
        log.info("CarrierBookingService.delete() successful with RequestId: {} and id: {}",
                LoggerHelper.getRequestIdFromMDC(), id);
    }

    @Override
    public void cancel(Long id) {
        CarrierBooking carrierBooking = carrierBookingDao.findById(id)
                .orElseThrow(() -> new ValidationException(INVALID_CARRIER_BOOKING_ID + id));

        carrierBooking.setStatus(CarrierBookingStatus.Cancelled);
        CarrierBooking savedCarrierBooking = carrierBookingDao.save(carrierBooking);
        saveTransactionHistory(savedCarrierBooking, FlowType.Inbound, SourceSystem.Carrier);
        sendNotification(carrierBooking);
    }

    @Override
    public void syncCarrierBookingToService(SyncBookingToService syncBookingToService) throws RunnerException {
        if (!CarrierBookingConstants.CARRIER_BOOKING.equalsIgnoreCase(syncBookingToService.getEntityType())) {
            throw new ValidationException("Invalid entity Type : " + syncBookingToService.getEntityType());
        }
        CarrierBooking carrierBooking = carrierBookingDao.findById(syncBookingToService.getEntityId())
                .orElseThrow(() -> new ValidationException("Invalid carrier Booking Id:" + syncBookingToService.getEntityId()));

        Object entity = carrierBookingValidationUtil.validateRequest(carrierBooking.getEntityType(), carrierBooking.getEntityId());
        if (Constants.CONSOLIDATION.equalsIgnoreCase(carrierBooking.getEntityType())) {
            ConsolidationDetails consolidationDetails = (ConsolidationDetails) entity;
            setCarrierBookingSyncFields(consolidationDetails, carrierBooking);
            syncCutOffFields(consolidationDetails, carrierBooking);

            consolidationDetailsDao.save(consolidationDetails);
            //updates Routes to consol and dependent shipments
            List<CarrierRouting> carrierRoutingList = carrierBooking.getCarrierRoutingList();
            if (!CollectionUtils.isEmpty(carrierRoutingList)) {
                List<Routings> routings = routingsV3Service.getRoutingsByConsolidationId(carrierBooking.getEntityId());
                List<RoutingsRequest> routingsRequests = createRoutingsRequestList(routings, carrierRoutingList);
                BulkUpdateRoutingsRequest bulkUpdateRoutingsRequest = new BulkUpdateRoutingsRequest();
                bulkUpdateRoutingsRequest.setRoutings(routingsRequests);
                bulkUpdateRoutingsRequest.setEntityId(carrierBooking.getEntityId());
                routingsV3Service.bulkUpdateWithValidateWrapper(bulkUpdateRoutingsRequest, Constants.CONSOLIDATION);
            }
        }
    }

    private List<RoutingsRequest> createRoutingsRequestList(List<Routings> routings, List<CarrierRouting> carrierRoutings) {
        // Step 1: Convert CarrierRouting to RoutingsRequest
        List<RoutingsRequest> carrierRoutingRequests = carrierRoutings.stream()
                .map(this::convertCarrierRoutingToRequest)
                .toList();

        List<RoutingsRequest> result = new ArrayList<>(carrierRoutingRequests);

        // Step 2: Extract carriageTypes from CarrierRouting for comparison
        Set<RoutingCarriage> carrierRoutingCarriageTypes = carrierRoutings.stream()
                .map(CarrierRouting::getCarriageType)
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());

        // Step 3: Add Routings where carriage is not in carrierRoutingCarriageTypes
        List<RoutingsRequest> remainingRoutings = routings.stream()
                .filter(routing -> routing.getCarriage() == null ||
                        !carrierRoutingCarriageTypes.contains(routing.getCarriage()))
                .map(this::convertRoutingToRequest)
                .toList();

        result.addAll(remainingRoutings);

        return result;

    }

    private RoutingsRequest convertRoutingToRequest(Routings routings) {
        return jsonHelper.convertValue(routings, RoutingsRequest.class);
    }

    /**
     * Converts CarrierRouting to RoutingsRequest
     */
    private RoutingsRequest convertCarrierRoutingToRequest(CarrierRouting carrierRouting) {
        return RoutingsRequest.builder()
                .carriage(carrierRouting.getCarriageType())
                .mode(carrierRouting.getTransportMode())
                .vesselName(carrierRouting.getVesselName())
                .pol(carrierRouting.getPol())
                .pod(carrierRouting.getPod())
                .eta(carrierRouting.getEta())
                .etd(carrierRouting.getEtd())
                .voyage(carrierRouting.getVoyageNo())
                .build();
    }

    private void syncCutOffFields(ConsolidationDetails consolidationDetails, CarrierBooking carrierBooking) throws RunnerException {
        SailingInformation sailingInformation = carrierBooking.getSailingInformation();
        if (sailingInformation.getVerifiedGrossMassCutoff() != null) {
            consolidationDetails.setVerifiedGrossMassCutoff(sailingInformation.getVerifiedGrossMassCutoff());
        }
        carrierBookingUtil.mapSailingToConsolidation(sailingInformation, consolidationDetails);
        updateShipmentCutoffFields(consolidationDetails);
    }

    private void updateShipmentCutoffFields(ConsolidationDetails consolidationDetails) throws RunnerException {
        ConsolidationDetails oldConsolidation = consolidationDetailsDao.findConsolidationsById(consolidationDetails.getId());
        List<ShipmentDetails> shipmentDetailsList = Optional.ofNullable(consolidationDetails.getShipmentsList())
                .orElse(Collections.emptySet())
                .stream()
                .toList();

        consolidationV3Service.updateShipmentDetailsIfConsolidationChanged(oldConsolidation, consolidationDetails, shipmentDetailsList, false);
        shipmentDao.saveAll(shipmentDetailsList);
    }

    private void setCarrierBookingSyncFields(ConsolidationDetails consolidationDetails, CarrierBooking carrierBooking) {
        consolidationDetails.setMawb(carrierBooking.getCarrierBlNo());
        consolidationDetails.setBookingStatus(carrierBooking.getStatus().name());
        consolidationDetails.setBookingId(carrierBooking.getBookingNo());
        consolidationDetails.setCarrierBookingRef(carrierBooking.getCarrierBookingNo());
        if (carrierBooking.getShippingInstruction() != null) {
            consolidationDetails.setSiStatus(carrierBooking.getShippingInstruction().getStatus());
        }
    }

    @Override
    public void updateCarrierDataToBooking(InttraCarrierBookingEventDto inttraCarrierBookingEventDto) {
        //update the details from response to carrier booking
        Map<String, Reference> references = createReferenceMap(inttraCarrierBookingEventDto.getReferences());
        Reference reference = references.get(CarrierBookingConstants.CR_BOOKING_ID);
        if (Objects.isNull(reference)) {
            log.info("Received empty cr booking id from inttra");
            return;
        }
        CarrierBooking carrierBooking = carrierBookingDao.findByBookingNo(reference.getReferenceValue());
        if (Objects.isNull(carrierBooking)) {
            log.info("Received invalid carrier booking no from intrra {}", reference.getReferenceValue());
            return;
        }
        String bookingResponseType = inttraCarrierBookingEventDto.getBookingResponseType();
        if (StringUtility.isNotEmpty(bookingResponseType)) {
            CarrierBookingStatus carrierBookingStatus = getCarrierBookingStatus(bookingResponseType);
            carrierBooking.setStatus(carrierBookingStatus);
        }
        carrierBooking.setCarrierBookingNo(inttraCarrierBookingEventDto.getCarrierReferenceNumber());
        DateInfo vgmDueDate = inttraCarrierBookingEventDto.getVgmDueDate();
        if (Objects.nonNull(vgmDueDate)) {
            String dueDate = vgmDueDate.getDateValue();
            carrierBooking.getSailingInformation().setVerifiedGrossMassCutoff(commonUtils.convertToLocalDateTimeFromInttra(dueDate, vgmDueDate.getDateFormat()));
        } else {
            carrierBooking.getSailingInformation().setVerifiedGrossMassCutoff(null);
        }
        DateInfo siDueDate = inttraCarrierBookingEventDto.getSiDueDate();
        if (Objects.nonNull(siDueDate)) {
            String dueDate = siDueDate.getDateValue();
            carrierBooking.getSailingInformation().setShipInstructionCutoff(commonUtils.convertToLocalDateTimeFromInttra(dueDate, siDueDate.getDateFormat()));
        } else {
            carrierBooking.getSailingInformation().setShipInstructionCutoff(null);
        }
        List<String> generalComments = inttraCarrierBookingEventDto.getGeneralComments();
        if (!CollectionUtils.isEmpty(generalComments)) {
            carrierBooking.setCarrierComment(generalComments.get(0));
        } else {
            carrierBooking.setCarrierComment(null);
        }
        Reference carrierBlNo = references.get(CarrierBookingConstants.CARRIER_BL_NO);
        if (Objects.nonNull(carrierBlNo)) {
            carrierBooking.setCarrierBlNo(carrierBlNo.getReferenceValue());
        } else {
            carrierBooking.setCarrierBlNo(null);
        }
        setCarrierRoutings(inttraCarrierBookingEventDto, carrierBooking);
        setContainerEmptyAndDropOffLocationDetails(inttraCarrierBookingEventDto, carrierBooking);
        CarrierBooking savedCarrierBooking = carrierBookingDao.save(carrierBooking);
        saveTransactionHistory(savedCarrierBooking, FlowType.Outbound, SourceSystem.INTTRA);
    }

    @Override
    public ResponseEntity<IRunnerResponse> getAllMasterData(Long carrierBookingId) {
        String responseMsg;
        try {
            Optional<CarrierBooking> carrierBookingOptional = carrierBookingDao.findById(carrierBookingId);
            if (carrierBookingOptional.isEmpty()) {
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            CarrierBooking carrierBooking = carrierBookingOptional.get();
            long start = System.currentTimeMillis();
            List<String> includeColumns = FieldUtils.getMasterDataAnnotationFields(List.of(createFieldClassDto(CarrierBooking.class, null), createFieldClassDto(SailingInformation.class, "sailingInformation.")));
            includeColumns.addAll(CarrierBookingConstants.LIST_INCLUDE_COLUMNS);
            CarrierBookingResponse carrierBookingResponse = (CarrierBookingResponse) commonUtils.setIncludedFieldsToResponse(carrierBooking, new HashSet<>(includeColumns), new CarrierBookingResponse());

            setInternalExternalEmails(carrierBookingResponse, carrierBooking);
            log.info("Total time taken in setting carrier booking details response {}", (System.currentTimeMillis() - start));
            Map<String, Object> response = fetchAllMasterDataByKey(carrierBookingResponse);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public CarrierBookingResponse getDefaultCarrierBookingValues(EntityType type, Long entityId) {
        ConsolidationDetails consolidationDetails;
        CarrierBookingResponse carrierBookingResponse = new CarrierBookingResponse();
        if (EntityType.CONSOLIDATION.equals(type)) {
            consolidationDetails = consolidationDetailsDao.findConsolidationsById(entityId);
            carrierBookingResponse.setStatus(CarrierBookingStatus.Draft.name());
        } else {
            throw new ValidationException("Invalid value of Entity Type");
        }
        carrierBookingResponse.setEntityId(consolidationDetails.getId());
        carrierBookingResponse.setEntityType(type);
        carrierBookingResponse.setEntityNumber(consolidationDetails.getConsolidationNumber());
        carrierBookingResponse.setServiceType(consolidationDetails.getServiceLevel());
        SailingInformationResponse sailingInformation = new SailingInformationResponse();
        if (consolidationDetails.getCarrierDetails() != null) {
            sailingInformation.setPol(consolidationDetails.getCarrierDetails().getOriginPort());
            sailingInformation.setPod(consolidationDetails.getCarrierDetails().getDestinationPort());
            sailingInformation.setCarrierReceiptPlace(consolidationDetails.getCarrierDetails().getOrigin());
            sailingInformation.setCarrierDeliveryPlace(consolidationDetails.getCarrierDetails().getDestination());
            sailingInformation.setCarrier(consolidationDetails.getCarrierDetails().getShippingLine());
        }
        sailingInformation.setTerminalCutoff(consolidationDetails.getTerminalCutoff());
        sailingInformation.setVerifiedGrossMassCutoff(consolidationDetails.getVerifiedGrossMassCutoff());
        sailingInformation.setShipInstructionCutoff(consolidationDetails.getShipInstructionCutoff());
        sailingInformation.setReeferCutoff(consolidationDetails.getReeferCutoff());
        sailingInformation.setHazardousBookingCutoff(consolidationDetails.getHazardousBookingCutoff());
        sailingInformation.setEmptyContainerPickupCutoff(consolidationDetails.getEarliestEmptyEquPickUp());
        //no mapping for loaded container gate in
        List<CommonContainerResponse> commonContainersList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(consolidationDetails.getContainersList())) {
            for (Containers containers : consolidationDetails.getContainersList()) {
                CommonContainerResponse commonContainers = getCommonContainerResponse(containers);
                commonContainersList.add(commonContainers);
            }
        }
        carrierBookingResponse.setContainersList(commonContainersList);
        List<ReferenceNumberResponse> referenceNumberResponses = new ArrayList<>();
        if (!CollectionUtils.isEmpty(consolidationDetails.getReferenceNumbersList())) {
            for (ReferenceNumbers referenceNumbers : consolidationDetails.getReferenceNumbersList()) {
                ReferenceNumberResponse referenceNumberResponse = new ReferenceNumberResponse();
                referenceNumberResponse.setType(referenceNumbers.getType());
                referenceNumberResponse.setReferenceNumber(referenceNumbers.getReferenceNumber());
                referenceNumberResponses.add(referenceNumberResponse);
            }
        }
        carrierBookingResponse.setReferenceNumbersList(referenceNumberResponses);
        //add parties data
        //sending agent is origin agent, receivingAgent is destination agent in consol
        if (Objects.nonNull(consolidationDetails.getSendingAgent())) {
            PartiesResponse partiesResponse = jsonHelper.convertValue(consolidationDetails.getSendingAgent(), PartiesResponse.class);
            partiesResponse.setId(null);
            partiesResponse.setGuid(null);
            carrierBookingResponse.setRequester(partiesResponse);
            carrierBookingResponse.setShipper(partiesResponse);
            carrierBookingResponse.setForwardingAgent(partiesResponse);
            carrierBookingResponse.setContract(partiesResponse);
            //contract party is not given in UX
        }
        if (Objects.nonNull(consolidationDetails.getReceivingAgent())) {
            PartiesResponse partiesResponse = jsonHelper.convertValue(consolidationDetails.getReceivingAgent(), PartiesResponse.class);
            partiesResponse.setId(null);
            partiesResponse.setGuid(null);
            carrierBookingResponse.setConsignee(partiesResponse);
        }

        return carrierBookingResponse;
    }

    @Override
    public Optional<CarrierBooking> findById(Long entityId) {
        return carrierBookingDao.findById(entityId);
    }


    public void convertWeightVolumeToRequiredUnit(CarrierBookingBridgeRequest carrierBooking) throws RunnerException {
        if (carrierBooking == null || carrierBooking.getContainersList() == null || carrierBooking.getContainersList().isEmpty()) {
            return;
        }

        for (CommonContainerResponse container : carrierBooking.getContainersList()) {
            BigDecimal weight = BigDecimal.valueOf(convertUnit(Constants.MASS, container.getGrossWeight(), container.getGrossWeightUnit(), Constants.WEIGHT_UNIT_KG).doubleValue());
            BigDecimal netWeight = BigDecimal.valueOf(convertUnit(Constants.MASS, container.getNetWeight(), container.getNetWeightUnit(), Constants.WEIGHT_UNIT_KG).doubleValue());
            BigDecimal volume = BigDecimal.valueOf(convertUnit(Constants.VOLUME, container.getVolume(), container.getVolumeUnit(), Constants.VOLUME_UNIT_M3).doubleValue());
            container.setVolume(volume);
            container.setGrossWeight(weight);
            container.setNetWeight(netWeight);
        }
    }

    @Override
    @Transactional
    public void submitOrAmend(SubmitAmendInttraRequest submitAmendInttraRequest) throws RunnerException {
        CarrierBooking carrierBooking = carrierBookingDao.findById(submitAmendInttraRequest.getId())
                .orElseThrow(() -> new ValidationException("Invalid booking Id: " + submitAmendInttraRequest.getId()));

        Parties[] partiesToCheck = {carrierBooking.getRequester(), carrierBooking.getShipper(),
                carrierBooking.getForwardingAgent()};
        String remoteId = carrierBookingInttraUtil.getInttraRemoteId(partiesToCheck);

        if (null == remoteId) {
            throw new ValidationException("Atleast one party must have inttra remoteId.");
        }

        String integrationCode = CARRIER_BOOKING_INTTRA_AMEND;
        IntegrationType integrationType = BRIDGE_CB_AMEND;
        if (OperationType.SUBMIT.equals(submitAmendInttraRequest.getOperationType())) {
            integrationCode = CARRIER_BOOKING_INTTRA_CREATE;
            integrationType = BRIDGE_CB_SUBMIT;
            carrierBooking.setStatus(Requested);
            carrierBooking.setSubmitByUserEmail(UserContext.getUser().getEmail());
        } else if (OperationType.AMEND.equals(submitAmendInttraRequest.getOperationType())) {
            carrierBooking.setStatus(Changed);
        }

        CarrierBookingBridgeRequest carrierBookingBridgeRequest = jsonHelper.convertValue(carrierBooking, CarrierBookingBridgeRequest.class);
        setInternalExternalEmails(carrierBookingBridgeRequest, carrierBooking);
        carrierBookingUtil.populateCarrierDetails(carrierBookingInttraUtil.fetchCarrierDetailsForBridgePayload(carrierBookingBridgeRequest.getSailingInformation()), carrierBookingBridgeRequest);
        carrierBookingUtil.populateIntegrationCode(carrierBookingInttraUtil.addAllContainerTypesInSingleCall(carrierBookingBridgeRequest.getContainersList()), carrierBookingBridgeRequest);
        carrierBookingInttraUtil.validateContainersIntegrationCode(carrierBookingBridgeRequest.getContainersList());
        convertWeightVolumeToRequiredUnit(carrierBookingBridgeRequest);
        BridgeServiceResponse bridgeResponse = carrierBookingInttraUtil.sendPayloadToBridge(carrierBookingBridgeRequest, carrierBooking.getId(), integrationCode, UUID.randomUUID().toString(), UUID.randomUUID().toString(), integrationType, EntityTypeTransactionHistory.CARRIER_BOOKING.name());
        processInttraResponse(bridgeResponse, carrierBooking);
        CarrierBooking savedCarrierBooking = carrierBookingDao.save(carrierBooking);
        saveTransactionHistory(savedCarrierBooking, FlowType.Inbound, SourceSystem.Carrier);
        //Make it async
        sendNotification(savedCarrierBooking);
    }

    @Override
    public CarrierBookingCloneResponse cloneBooking(Long carrierBookingId) {
        CarrierBooking carrierBooking = carrierBookingDao.findById(carrierBookingId)
                .orElseThrow(() -> new ValidationException("Invalid carrier booking Id: " + carrierBookingId));
        ShippingInstruction shippingInstruction = carrierBooking.getShippingInstruction();
        if (Objects.nonNull(shippingInstruction) && (ShippingInstructionStatus.Draft.equals(shippingInstruction.getStatus()) || !ShippingInstructionStatus.Cancelled.equals(shippingInstruction.getStatus()))) {
            throw new ValidationException("SI status is " + shippingInstruction.getStatus().getDescription() + " Please cancel the submitted booking  along with SI to clone booking");
        }
        VerifiedGrossMass verifiedGrossMass = carrierBooking.getVerifiedGrossMass();
        if (Objects.nonNull(verifiedGrossMass) && (VerifiedGrossMassStatus.Draft.equals(verifiedGrossMass.getStatus()) || !VerifiedGrossMassStatus.Cancelled.equals(verifiedGrossMass.getStatus()))) {
            throw new ValidationException("VGM status is " + verifiedGrossMass.getStatus().getDescription() + " Please cancel the submitted booking  along with VGM to clone booking");
        }
        CarrierBookingCloneResponse carrierBookingResponse = jsonHelper.convertValue(carrierBooking, CarrierBookingCloneResponse.class);
        carrierBookingResponse.setStatus(CarrierBookingStatus.Draft.name());
        carrierBookingResponse.setInternalEmailsList(carrierBookingInttraUtil.parseEmailStringToList(carrierBooking.getInternalEmails()));
        carrierBookingResponse.setExternalEmailsList(carrierBookingInttraUtil.parseEmailStringToList(carrierBooking.getExternalEmails()));
        return carrierBookingResponse;
    }

    @Override
    public ResponseEntity<IRunnerResponse> consolidatedList(CommonRequestModel commonRequestModel, boolean getMasterData) {
        ListCommonRequest listCommonRequest = (ListCommonRequest) commonRequestModel.getData();
        if (listCommonRequest == null) {
            throw new ValidationException(CARRIER_LIST_REQUEST_NULL_ERROR);
        }
        Pair<Specification<CarrierBooking>, Pageable> tuple = fetchData(listCommonRequest, CarrierBooking.class, CarrierBookingConstants.tableNames);
        Page<CarrierBooking> carrierBookingPage = carrierBookingDao.findAll(tuple.getLeft(), tuple.getRight());
        List<CarrierBooking> carrierBookings = carrierBookingPage.getContent();
        List<IRunnerResponse> carrierBookingResponses = convertEntityListToDtoList(carrierBookings, getMasterData, new HashSet<>());
        List<ShippingInstruction> shippingInstructionList = new ArrayList<>();
        List<VerifiedGrossMass> verifiedGrossMassList = new ArrayList<>();
        List<IRunnerResponse> finalResponses = new ArrayList<>(carrierBookingResponses);
        Map<Long, String> siCbMap = new HashMap<>();
        Map<Long, CarrierBookingStatus> vgmCbMap = new HashMap<>();
        Map<Long, ShippingInstructionStatus> vgmSiMap = new HashMap<>();
        for (CarrierBooking carrierBooking : carrierBookings) {
            if (Objects.nonNull(carrierBooking.getShippingInstruction())) {
                shippingInstructionList.add(carrierBooking.getShippingInstruction());
                siCbMap.put(carrierBooking.getShippingInstruction().getId(), carrierBooking.getStatus().name());
            }
            if (Objects.nonNull(carrierBooking.getVerifiedGrossMass())) {
                verifiedGrossMassList.add(carrierBooking.getVerifiedGrossMass());
                vgmCbMap.put(carrierBooking.getVerifiedGrossMass().getId(), carrierBooking.getStatus());
                if(Objects.nonNull(carrierBooking.getShippingInstruction())) {
                    vgmSiMap.put(carrierBooking.getVerifiedGrossMass().getId(), carrierBooking.getShippingInstruction().getStatus());
                }
            }
        }
        Page<ShippingInstruction> shippingInstructionEntityList = shippingInstructionsService.getShippingInstructions(listCommonRequest);
        shippingInstructionList.addAll(shippingInstructionEntityList.getContent());
        if (!CollectionUtils.isEmpty(shippingInstructionList)) {
            List<ShippingInstructionResponse> shippingInstructionResponses = new ArrayList<>();
            for (ShippingInstruction shippingInstruction : shippingInstructionList) {
                ShippingInstructionResponse shippingInstructionResponse = jsonHelper.convertValue(shippingInstruction, ShippingInstructionResponse.class);
                shippingInstructionResponse.setBookingStatus(siCbMap.get(shippingInstructionResponse.getId()));
                shippingInstructionResponses.add(shippingInstructionResponse);
            }
            List<IRunnerResponse> responseList = new ArrayList<>(shippingInstructionResponses);
            shippingInstructionMasterDataHelper.getMasterDataForList(responseList, getMasterData, false);
            finalResponses.addAll(responseList);
        }
        Page<VerifiedGrossMass> vgmEntityList = verifiedGrossMassService.getVerifiedGrossMasses(listCommonRequest);
        verifiedGrossMassList.addAll(vgmEntityList.getContent());

        if (!CollectionUtils.isEmpty(verifiedGrossMassList)) {
            List<VerifiedGrossMassResponse> verifiedGrossMassListResponses = new ArrayList<>();
            for (VerifiedGrossMass verifiedGrossMass : verifiedGrossMassList) {
                VerifiedGrossMassResponse verifiedGrossMassResponse = jsonHelper.convertValue(verifiedGrossMass, VerifiedGrossMassResponse.class);
                verifiedGrossMassResponse.setBookingStatus(vgmCbMap.get(verifiedGrossMassResponse.getId()));
                verifiedGrossMassResponse.setSiStatus(vgmSiMap.get(verifiedGrossMassResponse.getId()));
                verifiedGrossMassListResponses.add(verifiedGrossMassResponse);
            }
            List<IRunnerResponse> responseList = new ArrayList<>(verifiedGrossMassListResponses);
            verifiedGrossMassMasterDataHelper.getMasterDataForList(responseList, getMasterData, false);
            finalResponses.addAll(responseList);
        }
        return ResponseHelper.buildSuccessResponse(finalResponses);
    }


    private void processInttraResponse(BridgeServiceResponse bridgeServiceResponse, CarrierBooking carrierBooking) throws InttraFailureException {

        Map<String, Object> extraParams = bridgeServiceResponse.getExtraResponseParams();

        if (extraParams == null) {
            throw new InttraFailureException(String.format(ERR_INTTRA_MISSING_KEY, "extra param"));
        }

        String serviceResponse = (String) extraParams.get(SERVICE_RESPONSE);
        String serviceHttpStatus = (String) extraParams.get(SERVICE_HTTP_STATUS_CODE);

        if (serviceResponse == null || serviceResponse.trim().isEmpty()) {
            throw new InttraFailureException(String.format(ERR_INTTRA_MISSING_KEY, SERVICE_RESPONSE));
        }

        if (serviceHttpStatus == null || serviceHttpStatus.trim().isEmpty()) {
            throw new InttraFailureException(String.format(ERR_INTTRA_MISSING_KEY, SERVICE_HTTP_STATUS_CODE));
        }

        // Parse INTTRA response
        JsonNode responseNode = jsonHelper.readTreeFromJson(serviceResponse.trim());

        // Check if bridge service call was successful
        if (!"200".equals(serviceHttpStatus)) {
            handleInttraError(responseNode, carrierBooking.getId());
        }
        setBookingReferencesFromPayload(responseNode, carrierBooking);
    }

    private void handleInttraError(JsonNode responseNode, Long id) throws InttraFailureException {
        String concatenatedErrors = "";

        if (responseNode.isArray()) {
            concatenatedErrors = StreamSupport.stream(responseNode.spliterator(), false)
                    .map(errorNode -> errorNode.path(MESSAGE).asText())
                    .collect(Collectors.joining(" | "));
        } else if (responseNode.has(ERROR_MESSAGES)) {
            JsonNode errorMessagesNode = responseNode.path(ERROR_MESSAGES);
            // Check if errorMessages is an array
            if (errorMessagesNode.isArray()) {
                concatenatedErrors = StreamSupport.stream(errorMessagesNode.spliterator(), false)
                        .map(JsonNode::asText)
                        .collect(Collectors.joining(" | "));
            } else {
                concatenatedErrors = errorMessagesNode.asText();
            }
        } else {
            throw new InttraFailureException(String.format(ERR_INTTRA_MISSING_KEY, ERROR_MESSAGES));
        }

        log.error("INTTRA booking failed - Carrier Booking ID: {}, Errors: {}",
                id, concatenatedErrors);
        throw new InttraFailureException("INTTRA booking failed: " + concatenatedErrors);
    }

    private void setBookingReferencesFromPayload(JsonNode responseNode, CarrierBooking carrierBooking) throws InttraFailureException {

        JsonNode bookingDetails = responseNode.path(BOOKING_DETAILS);
        if (!bookingDetails.isArray() || bookingDetails.isEmpty()) {
            throw new InttraFailureException(String.format(ERR_INTTRA_MISSING_KEY, BOOKING_DETAILS));
        }

        // Get payload from first bookingDetail
        JsonNode payload = bookingDetails.get(0).path(PAYLOAD);
        if (payload.isMissingNode()) {
            throw new InttraFailureException(String.format(ERR_INTTRA_MISSING_KEY, PAYLOAD));
        }

        // Helper to extract required fields
        String inttraReference = getRequiredField(payload, INTTRA_REFERENCE);
        String carrierReferenceNumber = getRequiredField(payload, CARRIER_REFERENCE_NUMBER);

        // Set values in carrierBooking
        carrierBooking.setCarrierBookingNo(carrierReferenceNumber);
        carrierBooking.setInttraReference(inttraReference);
        log.info("INTTRA booking successful - Carrier Booking ID: {}, CarrierBookingNo: {}, InttraReference: {}",
                carrierBooking.getId(), carrierBooking.getCarrierBookingNo(), carrierBooking.getInttraReference());
    }

    private String getRequiredField(JsonNode node, String fieldName) {
        String value = node.path(fieldName).asText(null);
        if (value == null || value.isEmpty()) {
            throw new InttraFailureException(String.format(ERR_INTTRA_MISSING_KEY, fieldName));
        }
        return value;
    }

    private void saveTransactionHistory(CarrierBooking carrierBooking, FlowType flowType, SourceSystem sourceSystem) {
        String description = carrierBooking.getStatus().getDescription() + "by: " + UserContext.getUser().getUsername();
        carrierBookingInttraUtil.createTransactionHistory(carrierBooking.getStatus().getDescription(),
                flowType, description, sourceSystem, carrierBooking.getId(), EntityTypeTransactionHistory.CARRIER_BOOKING);
    }

    @NotNull
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
        commonContainers.setCount(containers.getContainerCount());
        return commonContainers;
    }

    private FieldClassDto createFieldClassDto(Class<?> clazz, String parentref) {
        FieldClassDto fieldClassDto = new FieldClassDto();
        fieldClassDto.setClazz(clazz);
        fieldClassDto.setFieldRef(parentref);
        return fieldClassDto;
    }

    public Map<String, Object> fetchAllMasterDataByKey(CarrierBookingResponse carrierBookingResponse) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> carrierBookingMasterDataHelper.addAllMasterDataInSingleCall(carrierBookingResponse, masterDataResponse)), executorServiceMasterData);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> carrierBookingMasterDataHelper.addAllUnlocationDataInSingleCall(carrierBookingResponse, masterDataResponse)), executorServiceMasterData);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> carrierBookingMasterDataHelper.addAllCarrierDataInSingleCall(carrierBookingResponse, masterDataResponse)), executorServiceMasterData);
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> carrierBookingMasterDataHelper.addAllCommodityTypesInSingleCall(carrierBookingResponse, masterDataResponse)), executorServiceMasterData);
        var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> carrierBookingMasterDataHelper.addAllContainerTypesInSingleCall(carrierBookingResponse, masterDataResponse)), executorServiceMasterData);
        var vesselsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> carrierBookingMasterDataHelper.addAllVesselDataInSingleCall(carrierBookingResponse, masterDataResponse)), executorServiceMasterData);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, commodityTypesFuture, containerTypeFuture, vesselsFuture).join();

        return masterDataResponse;
    }

    protected void setCarrierRoutings(InttraCarrierBookingEventDto inttraCarrierBookingEventDto, CarrierBooking carrierBooking) {
        List<TransportLeg> transportLegs = inttraCarrierBookingEventDto.getTransportLegs();
        if (!CollectionUtils.isEmpty(transportLegs)) {
            List<CarrierRouting> carrierRoutings = new ArrayList<>();
            populateMainCarriageDataToCarrierBooking(transportLegs, carrierBooking);

            int sequence = 1;
            for (TransportLeg transportLeg : transportLegs) {
                CarrierRouting carrierRouting = new CarrierRouting();
                carrierRouting.setSequence(sequence);
                carrierRouting.setCarriageType(getRoutingCarriage(transportLeg.getStage()));
                carrierRouting.setTransportMode(getTransportMode(transportLeg.getMode()));
                carrierRouting.setVesselName(transportLeg.getVesselName());
                carrierRouting.setVoyageNo(transportLeg.getConveyanceNumber());
                Location startLocation = transportLeg.getStartLocation();
                Location endLocation = transportLeg.getEndLocation();
                if (Objects.nonNull(startLocation)) {
                    carrierRouting.setPol(startLocation.getIdentifierValue());
                }
                if (Objects.nonNull(endLocation)) {
                    carrierRouting.setPod(endLocation.getIdentifierValue());
                }
                List<LocationDate> startLocationLocationDates = startLocation.getLocationDates();
                List<LocationDate> endLocationLocationDates = endLocation.getLocationDates();

                carrierRouting.setEta(getETA(endLocationLocationDates));
                carrierRouting.setEtd(getETD(startLocationLocationDates));
                carrierRoutings.add(carrierRouting);
                sequence++;
            }
            carrierBooking.setCarrierRoutingList(carrierRoutings);
        } else {
            carrierBooking.setCarrierRoutingList(new ArrayList<>());
        }
    }

    protected void setContainerEmptyAndDropOffLocationDetails(InttraCarrierBookingEventDto inttraCarrierBookingEventDto, CarrierBooking carrierBooking) {
        Map<String, Object> loadedContainerDropOff = new HashMap<>();
        Map<String, Object> emptyContainerPickup = new HashMap<>();
        List<Equipment> equipments = inttraCarrierBookingEventDto.getEquipments();
        if (!CollectionUtils.isEmpty(equipments)) {
            Equipment equipment = equipments.get(0);
            Haulage haulage = equipment.getHaulage();
            if (Objects.nonNull(haulage)) {
                List<HaulagePoint> haulagePoints = haulage.getPoints();
                if (!CollectionUtils.isEmpty(haulagePoints)) {
                    setEmptyAndDropOffLocations(haulagePoints, loadedContainerDropOff, emptyContainerPickup);
                }
            }
        }
        carrierBooking.setLoadedContainerDropOffDetails(loadedContainerDropOff);
        carrierBooking.setEmptyContainerPickupDetails(emptyContainerPickup);
    }

    private void setEmptyAndDropOffLocations(List<HaulagePoint> haulagePoints, Map<String, Object> loadedContainerDropOff, Map<String, Object> emptyContainerPickup) {
        for (HaulagePoint haulagePoint : haulagePoints) {
            HaulageParty haulageParty = haulagePoint.getHaulageParty();
            HaulagePartyDto haulagePartyDto = new HaulagePartyDto();
            if (CarrierBookingConstants.FULL_DROP_OFF.equalsIgnoreCase(haulageParty.getPartyName())) {
                haulagePartyDto.setHaulageParty(haulageParty);
                List<HaulageDate> haulageDates = haulagePoint.getDates();
                Optional<HaulageDate> closingDate = haulageDates.stream()
                        .filter(haulageDate -> CarrierBookingConstants.CLOSING_DATE.equalsIgnoreCase(haulageDate.getHaulageDateType()))
                        .findFirst();
                closingDate.ifPresent(haulageDate -> haulagePartyDto.setContainerCutOff(commonUtils.convertToLocalDateTimeFromInttra(haulageDate.getDateValue(), haulageDate.getDateFormat())));
                loadedContainerDropOff.put(CarrierBookingConstants.HAULAGE_PARTY, haulagePartyDto);
            } else if (CarrierBookingConstants.EMPTY_PICK_UP.equalsIgnoreCase(haulageParty.getPartyName())) {
                haulagePartyDto.setHaulageParty(haulageParty);
                List<HaulageDate> haulageDates = haulagePoint.getDates();
                Optional<HaulageDate> emptyPickupDate = haulageDates.stream()
                        .filter(haulageDate -> CarrierBookingConstants.EMPTY_PICKUP_DATE.equalsIgnoreCase(haulageDate.getHaulageDateType()))
                        .findFirst();
                emptyPickupDate.ifPresent(haulageDate -> haulagePartyDto.setContainerCutOff(commonUtils.convertToLocalDateTimeFromInttra(haulageDate.getDateValue(), haulageDate.getDateFormat())));
                emptyContainerPickup.put(CarrierBookingConstants.HAULAGE_PARTY, haulagePartyDto);
            }
        }
    }

    private void populateMainCarriageDataToCarrierBooking(List<TransportLeg> transportLegs, CarrierBooking carrierBooking) {
        List<TransportLeg> mainCarriageList = transportLegs.stream()
                .filter(routing -> CarrierBookingConstants.MAIN_CARRIAGE.equalsIgnoreCase(routing.getStage()))
                .toList();
        TransportLeg firstLeg = mainCarriageList.get(0);
        TransportLeg lastLeg = mainCarriageList.get(mainCarriageList.size() - 1);
        carrierBooking.getSailingInformation().setVesselName(firstLeg.getVesselName());
        carrierBooking.getSailingInformation().setVoyageNo(firstLeg.getConveyanceNumber());

        Location startLocation = firstLeg.getStartLocation();
        if (Objects.nonNull(startLocation)) {
            carrierBooking.getSailingInformation().setPol(startLocation.getIdentifierValue());
            List<LocationDate> etd = startLocation.getLocationDates();
            carrierBooking.getSailingInformation().setEtd(getETD(etd));
        }

        Location endLocation = lastLeg.getEndLocation();
        if (Objects.nonNull(endLocation)) {
            carrierBooking.getSailingInformation().setPod(endLocation.getIdentifierValue());
            List<LocationDate> eta = endLocation.getLocationDates();
            carrierBooking.getSailingInformation().setEtd(getETD(eta));
        }
    }


    LocalDateTime getETD(List<LocationDate> startLocationLocationDates) {
        if (CollectionUtils.isEmpty(startLocationLocationDates)) {
            return null;
        }
        return startLocationLocationDates.stream()
                .filter(ld -> "EstimatedDepartureDate".equals(ld.getType()))
                .map(ld -> commonUtils.convertToLocalDateTimeFromInttra(ld.getDateValue(), ld.getDateFormat()))
                .findFirst()
                .orElse(null);
    }

    LocalDateTime getETA(List<LocationDate> endLocationLocationDates) {
        if (CollectionUtils.isEmpty(endLocationLocationDates)) {
            return null;
        }
        return endLocationLocationDates.stream()
                .filter(ld -> "EstimatedArrivalDate".equals(ld.getType()))
                .map(ld -> commonUtils.convertToLocalDateTimeFromInttra(ld.getDateValue(), ld.getDateFormat()))
                .findFirst()
                .orElse(null);
    }

    public CarrierBookingStatus getCarrierBookingStatus(String type) {
        return switch (type) {
            case "ConditionallyAccepted" -> CarrierBookingStatus.ConditionallyAccepted;
            case "Rejected" -> CarrierBookingStatus.RejectedByINTTRA;
            case "Accepted" -> CarrierBookingStatus.AcceptedByINTTRA;
            case "Confirmed" -> CarrierBookingStatus.ConfirmedByCarrier;
            case "Declined" -> CarrierBookingStatus.DeclinedByCarrier;
            case "Replaced" -> CarrierBookingStatus.ReplacedByCarrier;
            case "Cancelled", "Canceled" -> CarrierBookingStatus.CancelledByCarrier;
            case "ChangeBookingRequested" -> Changed;
            case "NewBookingRequested" -> Requested;
            default -> CarrierBookingStatus.PendingFromCarrier;
        };
    }

    public RoutingCarriage getRoutingCarriage(String stage) {
        return switch (stage) {
            case "MainCarriage" -> RoutingCarriage.MAIN_CARRIAGE;
            case "PreCarriage" -> RoutingCarriage.PRE_CARRIAGE;
            case "OnCarriage" -> RoutingCarriage.ON_CARRIAGE;
            default -> null;
        };
    }

    public String getTransportMode(String stage) {
        return switch (stage) {
            case "MaritimeTransport" -> CarrierBookingConstants.TRANSPORT_MODE_SEA;
            case "RailTransport" -> CarrierBookingConstants.TRANSPORT_MODE_RAIL;
            case "RoadTransport" -> CarrierBookingConstants.TRANSPORT_MODE_ROAD;
            case "InlandWaterTransport" -> CarrierBookingConstants.TRANSPORT_MODE_INLAND_WATER;
            case "Rail_WaterTransport" -> CarrierBookingConstants.TRANSPORT_MODE_RAIL_WATER;
            case "Road_WaterTransport" -> CarrierBookingConstants.TRANSPORT_MODE_ROAD_WATER;
            default -> null;
        };
    }

    public Map<String, Reference> createReferenceMap(List<Reference> references) {
        if (references == null) {
            return new HashMap<>();
        }

        return references.stream()
                .collect(Collectors.toMap(
                        Reference::getReferenceType,
                        Function.identity(),
                        (existing, replacement) -> replacement  // Handle duplicates by keeping the last one
                ));
    }

    //call when status Change
    protected void sendNotification(CarrierBooking carrierBooking) {
        try {
            List<String> requests = new ArrayList<>(
                    List.of(Constants.CARRIER_BOOKING_EMAIL_TEMPLATE));
            List<EmailTemplatesRequest> emailTemplates = carrierBookingInttraUtil.fetchEmailTemplate(requests);
            EmailTemplatesRequest carrierBookingTemplate = emailTemplates.stream()
                    .filter(Objects::nonNull)
                    .filter(template -> Constants.CARRIER_BOOKING_EMAIL_TEMPLATE.equalsIgnoreCase(template.getType()))
                    .findFirst()
                    .orElse(null);
            if (carrierBookingTemplate != null) {
                List<String> toEmails = carrierBookingUtil.getSendEmailBaseRequest(carrierBooking);
                notificationService.sendEmail(carrierBookingTemplate.getBody(), carrierBookingTemplate.getSubject(), toEmails, new ArrayList<>());
                log.info("Email sent with Excel attachment");
            }
        } catch (Exception e) {
            log.error("Error in  sending carrier booking email: {}", e.getMessage());
        }
    }


    private void mismatchDetection(CarrierBooking carrierBooking, CarrierBookingResponse carrierBookingResponse) {
        Object entity = carrierBookingValidationUtil.validateRequest(
                carrierBooking.getEntityType(), carrierBooking.getEntityId());

        if (carrierBooking.getEntityType().equalsIgnoreCase(Constants.CONSOLIDATION)) {
            ConsolidationDetails consolidationDetails = (ConsolidationDetails) entity;
            List<Containers> consoleContainers = consolidationDetails.getContainersList();
            List<CommonContainers> carrierBookingContainers = carrierBooking.getContainersList();


            List<ContainerMisMatchWarning> warnings = carrierBookingUtil.detectContainerMismatches(consoleContainers, carrierBookingContainers);

            // attach to response or handle as needed
            carrierBookingResponse.setContainerMismatchWarningList(warnings);
        }
    }
}

