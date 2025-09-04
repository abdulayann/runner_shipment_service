package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.dto.response.FieldClassDto;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingListResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ContainerMisMatchWarning;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.CarrierRouting;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingGenerationType;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.CarrierBookingMasterDataHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
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
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.notification.request.SendEmailBaseRequest;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierBookingService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.FieldUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingUtil;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingValidationUtil;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_INCLUDE_COLUMNS_REQUIRED_ERROR_MESSAGE;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_REQUEST_EMPTY_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_REQUEST_NULL_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_RESPONSE_SUCCESS;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class CarrierBookingService implements ICarrierBookingService {

    private final ICarrierBookingDao carrierBookingDao;
    private final JsonHelper jsonHelper;
    private final CarrierBookingMasterDataHelper carrierBookingMasterDataHelper;
    private final CarrierBookingValidationUtil carrierBookingValidationUtil;
    private final CommonUtils commonUtils;
    private final INotificationService notificationService;
    private final IV1Service iv1Service;
    private final CarrierBookingUtil carrierBookingUtil;
    private final MasterDataUtils masterDataUtils;
    private final ExecutorService executorServiceMasterData;

    @Autowired
    public CarrierBookingService(ICarrierBookingDao carrierBookingDao, JsonHelper jsonHelper, CarrierBookingMasterDataHelper carrierBookingMasterDataHelper, CarrierBookingValidationUtil carrierBookingValidationUtil, CommonUtils commonUtils, INotificationService notificationService, IV1Service iv1Service, CarrierBookingUtil carrierBookingUtil, MasterDataUtils masterDataUtils, @Qualifier("executorServiceMasterData") ExecutorService executorServiceMasterData) {
        this.carrierBookingDao = carrierBookingDao;
        this.jsonHelper = jsonHelper;
        this.carrierBookingValidationUtil = carrierBookingValidationUtil;
        this.carrierBookingMasterDataHelper = carrierBookingMasterDataHelper;
        this.commonUtils = commonUtils;
        this.notificationService = notificationService;
        this.iv1Service = iv1Service;
        this.carrierBookingUtil = carrierBookingUtil;
        this.masterDataUtils = masterDataUtils;
        this.executorServiceMasterData = executorServiceMasterData;
    }

    @Override
    public CarrierBookingResponse create(CarrierBookingRequest request) {
        log.info("CarrierBookingService.create() called with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        carrierBookingValidationUtil.validateServiceType(request);
        Object entity = carrierBookingValidationUtil.validateRequest(request.getEntityType(), request.getEntityId());
        CarrierBooking carrierBookingEntity = jsonHelper.convertValue(request, CarrierBooking.class);
        if (Constants.CONSOLIDATION.equalsIgnoreCase(request.getEntityType())) {
            ConsolidationDetails consolidationDetails = (ConsolidationDetails) entity;
            carrierBookingEntity.setEntityNumber(consolidationDetails.getConsolidationNumber());
            //read only fields
            SailingInformation sailingInformation = carrierBookingEntity.getSailingInformation();
            if (Objects.isNull(sailingInformation)) {
                sailingInformation = new SailingInformation();
            }
            sailingInformation.setPol(consolidationDetails.getCarrierDetails().getOriginPort());
            sailingInformation.setPod(consolidationDetails.getCarrierDetails().getDestinationPort());
            sailingInformation.setCarrierReceiptPlace(consolidationDetails.getCarrierDetails().getOrigin());
            sailingInformation.setCarrierDeliveryPlace(consolidationDetails.getCarrierDetails().getDestination());
            carrierBookingEntity.setSailingInformation(sailingInformation);
        }
        carrierBookingEntity.setCarrierRoutingList(null);// we will get it from carrier
        carrierBookingEntity.setLoadedContainerDropOffDetails(null); // we will get it from carrier
        carrierBookingEntity.setEmptyContainerPickupDetails(null); // we will get it from carrier
        carrierBookingEntity.setCarrierComment(null); //we will get it from carrier
        generateBookingNumber(carrierBookingEntity);
        carrierBookingEntity.setCarrierBlNo(null);
        carrierBookingEntity.setCarrierBookingNo(null);
        carrierBookingEntity.setStatus(CarrierBookingStatus.Draft);
        CarrierBooking savedEntity = carrierBookingDao.create(carrierBookingEntity);
        CarrierBookingResponse carrierBookingResponse = jsonHelper.convertValue(savedEntity, CarrierBookingResponse.class);
        log.info("CarrierBookingService.create() successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(carrierBookingResponse));
        return carrierBookingResponse;
    }

    private void generateBookingNumber(CarrierBooking carrierBookingEntity) {
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        var prefix = "";
        if (StringUtility.isNotEmpty(v1TenantSettingsResponse.getBookingPrefix())) {
            prefix = v1TenantSettingsResponse.getBookingPrefix();
        }
        if (v1TenantSettingsResponse.getBookingNumberGeneration() != null && v1TenantSettingsResponse.getBookingNumberGeneration() == CarrierBookingGenerationType.Serial.getValue()) {
            //get total count of carrier booking for that tenant + 1, prefix+count
            Long totalCarrierBookings = carrierBookingDao.getTotalCarrierBookings();
            carrierBookingEntity.setBookingNo(prefix + totalCarrierBookings + 1);
        } else {
            //generate the random number and add prefix
            String randomBookingNumber = StringUtility.getRandomString(10);
            carrierBookingEntity.setBookingNo(prefix + randomBookingNumber);
        }

    }

    @Override
    public CarrierBookingResponse retrieveById(Long id) {
        log.info("CarrierBookingService.getById() called with RequestId: {} and id: {}",
                LoggerHelper.getRequestIdFromMDC(), id);
        CarrierBooking carrierBooking = carrierBookingDao.findById(id).orElseThrow(() -> new ValidationException("Invalid id : " + id));

        carrierBooking.setCarrierComment(carrierBookingUtil.truncate(carrierBooking.getCarrierComment(), 10000));
        // consolidation fetch container, common container properties diff
        CarrierBookingResponse carrierBookingResponse = jsonHelper.convertValue(carrierBooking, CarrierBookingResponse.class);
        mismatchDetection(carrierBooking, carrierBookingResponse);
        log.info("CarrierBookingService.getById() successful with RequestId: {} and response: {}",
                LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(carrierBookingResponse));
        return carrierBookingResponse;
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel, boolean getMasterData) {
        ListCommonRequest listCommonRequest = (ListCommonRequest) commonRequestModel.getData();
        if (listCommonRequest == null) {
            log.error(CARRIER_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException(CARRIER_LIST_REQUEST_NULL_ERROR);
        }
        if (listCommonRequest.getIncludeColumns() == null || listCommonRequest.getIncludeColumns().isEmpty()) {
            throw new ValidationException(CARRIER_INCLUDE_COLUMNS_REQUIRED_ERROR_MESSAGE);
        }

        Pair<Specification<CarrierBooking>, Pageable> tuple = fetchData(listCommonRequest, CarrierBooking.class, CarrierBookingConstants.tableNames);
        Page<CarrierBooking> carrierBookingPage = carrierBookingDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info(CARRIER_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());


        List<IRunnerResponse> filteredList = convertEntityListToDtoList(carrierBookingPage.getContent(), getMasterData, listCommonRequest.getIncludeColumns().stream().collect(Collectors.toSet()));

        return ResponseHelper.buildListSuccessResponse(
                filteredList,
                carrierBookingPage.getTotalPages(),
                carrierBookingPage.getTotalElements());
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<CarrierBooking> carrierBookingList, boolean getMasterData,
                                                             Set<String> includeColumns) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<CarrierBookingListResponse> carrierBookingListResponses = new ArrayList<>();

        for (CarrierBooking carrierBooking : carrierBookingList) {
            CarrierBookingListResponse carrierBookingListResponse = jsonHelper.convertValue(carrierBooking, CarrierBookingListResponse.class);
            carrierBookingListResponses.add(carrierBookingListResponse);
        }

        carrierBookingListResponses.forEach(responseList::add);
        carrierBookingMasterDataHelper.getMasterDataForList(carrierBookingList, responseList, getMasterData, true, includeColumns);
        return responseList;
    }

    @Override
    public CarrierBookingResponse update(CarrierBookingRequest request) {
        log.info("CarrierBookingService.update() called with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        CarrierBooking existingCarrierBooking = carrierBookingDao.findById(request.getId()).orElseThrow(() -> new ValidationException("Invalid carrier booking Id"));
        carrierBookingValidationUtil.validateServiceType(request);
        Object entity = carrierBookingValidationUtil.validateRequest(request.getEntityType(), request.getEntityId());
        CarrierBooking carrierBookingEntity = jsonHelper.convertValue(request, CarrierBooking.class);
        if (Constants.CONSOLIDATION.equalsIgnoreCase(request.getEntityType())) {
            ConsolidationDetails consolidationDetails = (ConsolidationDetails) entity;
            carrierBookingEntity.setEntityNumber(consolidationDetails.getConsolidationNumber());
            //read only fields
            SailingInformation sailingInformation = carrierBookingEntity.getSailingInformation();
            if (sailingInformation == null) {
                sailingInformation = new SailingInformation();
            }
            sailingInformation.setPol(consolidationDetails.getCarrierDetails().getOriginPort());
            sailingInformation.setPod(consolidationDetails.getCarrierDetails().getDestinationPort());
            sailingInformation.setCarrierReceiptPlace(consolidationDetails.getCarrierDetails().getOrigin());
            sailingInformation.setCarrierDeliveryPlace(consolidationDetails.getCarrierDetails().getDestination());
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
        CarrierBooking savedEntity = carrierBookingDao.create(carrierBookingEntity);
        CarrierBookingResponse carrierBookingResponse = jsonHelper.convertValue(savedEntity, CarrierBookingResponse.class);
        log.info("CarrierBookingService.update() successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(carrierBookingResponse));
        return carrierBookingResponse;
    }

    @Override
    public void delete(Long id) {
        log.info("CarrierBookingService.delete() called with RequestId: {} and id: {}",
                LoggerHelper.getRequestIdFromMDC(), id);
        carrierBookingDao.delete(id);
        log.info("CarrierBookingService.delete() successful with RequestId: {} and id: {}",
                LoggerHelper.getRequestIdFromMDC(), id);
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
        carrierBookingDao.save(carrierBooking);
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

    private void setCarrierRoutings(InttraCarrierBookingEventDto inttraCarrierBookingEventDto, CarrierBooking carrierBooking) {
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

    private void setContainerEmptyAndDropOffLocationDetails(InttraCarrierBookingEventDto inttraCarrierBookingEventDto, CarrierBooking carrierBooking) {
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


    private LocalDateTime getETD(List<LocationDate> startLocationLocationDates) {
        if (CollectionUtils.isEmpty(startLocationLocationDates)) {
            return null;
        }
        return startLocationLocationDates.stream()
                .filter(ld -> "EstimatedDepartureDate".equals(ld.getType()))
                .map(ld -> commonUtils.convertToLocalDateTimeFromInttra(ld.getDateValue(), ld.getDateFormat()))
                .findFirst()
                .orElse(null);
    }

    private LocalDateTime getETA(List<LocationDate> endLocationLocationDates) {
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
            case "ChangeBookingRequested" -> CarrierBookingStatus.Changed;
            case "NewBookingRequested" -> CarrierBookingStatus.Requested;
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

    private void sendNotification(CarrierBooking carrierBooking) {
        try {
            List<String> requests = new ArrayList<>(
                    List.of(Constants.CARRIER_BOOKING_EMAIL_TEMPLATE));
            List<EmailTemplatesRequest> emailTemplates = getCarrierBookingEmailTemplate(requests);
            EmailTemplatesRequest carrierBookingTemplate = emailTemplates.stream()
                    .filter(Objects::nonNull)
                    .filter(template -> Constants.CARRIER_BOOKING_EMAIL_TEMPLATE.equalsIgnoreCase(template.getType()))
                    .findFirst()
                    .orElse(null);
            if (carrierBookingTemplate != null) {
                SendEmailBaseRequest request = new SendEmailBaseRequest();
                request.setTo(carrierBooking.getInternalEmails());
                request.setSubject(carrierBookingTemplate.getSubject());
                request.setTemplateName(carrierBookingTemplate.getName());
                request.setHtmlBody(carrierBookingTemplate.getBody());
                notificationService.sendEmail(request);
                log.info("Email sent with Excel attachment");
            }
        } catch (Exception e) {
            log.error("Error in  sending carrier booking email: {}", e.getMessage());
        }
    }

    public List<EmailTemplatesRequest> getCarrierBookingEmailTemplate(List<String> templateCodes) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of(Constants.TYPE));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(templateCodes)));
        request.setCriteriaRequests(criteria);
        V1DataResponse v1DataResponse = iv1Service.getEmailTemplates(request);
        if (v1DataResponse != null && v1DataResponse.entities != null) {
            return jsonHelper.convertValueToList(v1DataResponse.entities, EmailTemplatesRequest.class);
        }
        return new ArrayList<>();
    }

    private void mismatchDetection(CarrierBooking carrierBooking, CarrierBookingResponse carrierBookingResponse) {
        Object entity = carrierBookingValidationUtil.validateRequest(
                carrierBooking.getEntityType(), carrierBooking.getEntityId());

        if (carrierBooking.getEntityType().equalsIgnoreCase(Constants.CONSOLIDATION)) {
            ConsolidationDetails consolidationDetails = (ConsolidationDetails) entity;
            List<Containers> consoleContainers = consolidationDetails.getContainersList();
            List<CommonContainers> carrierBookingContainers = carrierBooking.getContainersList();


            List<ContainerMisMatchWarning> warnings =  carrierBookingUtil.detectContainerMismatches(consoleContainers, carrierBookingContainers);

            // attach to response or handle as needed
            carrierBookingResponse.setContainerMismatchWarningList(warnings);
        }
    }
}

