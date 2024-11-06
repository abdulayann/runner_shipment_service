package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.CarrierBookingResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import com.dpw.runner.shipment.services.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierBookingService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.jetbrains.annotations.NotNull;
import org.modelmapper.ModelMapper;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.lang.reflect.InvocationTargetException;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class CarrierBookingService implements ICarrierBookingService {
    ExecutorService executorService = Executors.newFixedThreadPool(10);
    private static final Random rnd = new SecureRandom();

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private ICarrierBookingDao carrierBookingDao;

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private IBookingCarriageDao bookingCarriageDao;

    @Autowired
    private IBookingPaymentDao bookingPaymentDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private IEventDao eventDao;

    @Autowired
    private IReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private AuditLogService auditLogService;

    private final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("origin", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("origin").build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destination").build()),
            Map.entry("originPort", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("originPort").build()),
            Map.entry("destinationPort", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destinationPort").build()),
            Map.entry("bookingNumber", RunnerEntityMapping.builder().tableName(Constants.CARRIER_BOOKING).dataType(String.class).fieldName("bookingNumber").isContainsText(true).build()),
            Map.entry("status", RunnerEntityMapping.builder().tableName(Constants.CARRIER_BOOKING).dataType(CarrierBookingStatus.class).fieldName("status").build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName(Constants.CARRIER_BOOKING).dataType(String.class).fieldName("createdBy").build()),
            Map.entry("id", RunnerEntityMapping.builder().tableName(Constants.CARRIER_BOOKING).dataType(String.class).fieldName("id").build())
    );

    @Override
    @Transactional(rollbackFor = RunnerException.class)
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) throws RunnerException {
        CarrierBookingRequest request = (CarrierBookingRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Carrier Booking Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }
        CarrierBooking carrierBookingDetails = jsonHelper.convertValue(request, CarrierBooking.class);
        beforeSave(carrierBookingDetails, Boolean.TRUE);
        carrierBookingDetails = carrierBookingDao.save(carrierBookingDetails);
        afterSave(carrierBookingDetails, request, Boolean.TRUE);

        createAuditMetaData(carrierBookingDetails, DBOperationType.CREATE.name(), null);
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(carrierBookingDetails, CarrierBookingResponse.class));
    }

    private void createAuditMetaData(CarrierBooking carrierBookingDetails, String operation, CarrierBooking prevData){
        try {
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(carrierBookingDetails)
                            .prevData(prevData)
                            .parent(CarrierBooking.class.getSimpleName())
                            .parentId(carrierBookingDetails.getId())
                            .operation(operation).build()
            );
        } catch (Exception e) {
            log.error(e.getMessage());
        }

    }

    private void beforeSave(CarrierBooking carrierBooking, Boolean isCreate){
        if(Boolean.TRUE.equals(isCreate) && StringUtility.isEmpty(carrierBooking.getReferenceNo()))
            carrierBooking.setReferenceNo(carrierBooking.getIntraBookingId());

        if (Objects.isNull(carrierBooking.getSourceTenantId()))
            carrierBooking.setSourceTenantId(UserContext.getUser().TenantId);

        if(Boolean.TRUE.equals(isCreate) && StringUtility.isEmpty(carrierBooking.getIntraBookingId())){
            var tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
            String prefix = (tenantSettingsResponse != null && StringUtility.isNotEmpty(tenantSettingsResponse.getBookingPrefix()))
                    ? tenantSettingsResponse.getBookingPrefix() : "";

            if (tenantSettingsResponse != null
                    && ObjectUtils.isNotEmpty(tenantSettingsResponse.getBookingNumberGeneration())
                    && tenantSettingsResponse.getBookingNumberGeneration() == GenerationType.Serial.getValue()
            ) {

                Optional<Long> maxDbId = carrierBookingDao.getMaxId();
                Long maxId = maxDbId.map(aLong -> aLong + 1).orElse(1L);
                String intraBookingId = prefix + maxId;
                carrierBooking.setIntraBookingId(intraBookingId);

            } else {
                String intraBookingId;
                do {
                    intraBookingId = prefix + StringUtility.getRandomString(10);
                } while (carrierBookingDao.existsByIntraBookingId(intraBookingId));
                carrierBooking.setIntraBookingId(intraBookingId);
            }
        }

    }

    private void afterSave(CarrierBooking carrierBooking, CarrierBookingRequest request, Boolean isCreate) throws RunnerException {
        Long bookingId = carrierBooking.getId();
        List<PackingRequest> packingRequest = request.getPackingList();
        List<ReferenceNumbersRequest> referenceNumbersRequest = request.getReferenceNumbersList();
        List<ContainerRequest> containerRequest = request.getContainersList();
        List<BookingCarriageRequest> bookingCarriageRequest = request.getBookingCarriagesList();
        List<BookingPaymentRequest> bookingPaymentRequest = request.getBookingPaymentsList();
        List<PartiesRequest> consolidationAddressRequest = request.getConsolidationAddresses();
        List<EventsRequest> eventsRequestList = request.getEventsList();

        if (packingRequest != null)
            carrierBooking.setPackingList(packingDao.updateEntityFromCarrierBooking(commonUtils.convertToEntityList(packingRequest, Packing.class), bookingId));

        if (referenceNumbersRequest != null)
            carrierBooking.setReferenceNumbersList(referenceNumbersDao.updateEntityFromCarrierBooking(commonUtils.convertToEntityList(referenceNumbersRequest, ReferenceNumbers.class), bookingId));

        if (containerRequest != null) {
            List<Containers> containers = containerDao.updateEntityFromCarrierBooking(commonUtils.convertToEntityList(containerRequest, Containers.class), bookingId);
            carrierBooking.setContainersList(containers);
        }

        if (bookingCarriageRequest != null) {
            List<BookingCarriage> bookingCarriages = bookingCarriageDao.updateEntityFromCarrierBooking(commonUtils.convertToEntityList(bookingCarriageRequest, BookingCarriage.class), bookingId);
            carrierBooking.setBookingCarriagesList(bookingCarriages);
        }

        if (bookingPaymentRequest!=null){
            List<BookingPayment> bookingPayments = bookingPaymentDao.updateEntityFromCarrierBooking(commonUtils.convertToEntityList(bookingPaymentRequest, BookingPayment.class), bookingId);
            carrierBooking.setBookingPaymentsList(bookingPayments);
        }

        if (consolidationAddressRequest != null) {
            List<Parties> updatedConsolidationAddresses = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(consolidationAddressRequest, Parties.class, false), bookingId, Constants.CONSOLIDATION_ADDRESSES);
            carrierBooking.setConsolidationAddresses(updatedConsolidationAddresses);
        }

        if (eventsRequestList != null) {
            List<Events> eventsList = commonUtils.convertToEntityList(eventsRequestList, Events.class, isCreate);
            eventDao.updateEntityFromOtherEntity(eventsList, bookingId, Constants.CARRIER_BOOKING);
            carrierBooking.setEventsList(eventsList);
        }

        if(Boolean.TRUE.equals(isCreate) && ObjectUtils.isNotEmpty(request.getConsolidationGuid()))
            autoGenerateCreateEvent(carrierBooking);
    }

    private void autoGenerateCreateEvent(CarrierBooking carrierBooking) {
        Events response;
        String description = "Carrier Booking created by " + TenantContext.getCurrentTenant().toString();
        response = createAutomatedEvents(carrierBooking, EventConstants.BKCRT, description);

        if (carrierBooking.getEventsList() == null) {
            carrierBooking.setEventsList(new ArrayList<>());
        }
        carrierBooking.getEventsList().add(response);
    }

    private Events createAutomatedEvents(CarrierBooking carrierBooking, String eventCode, String description) {
        Optional<Long> consolidationDetails = consolidationDetailsDao.findIdByGuid(carrierBooking.getConsolidationGuid());
        Events events = new Events();
        if (consolidationDetails.isPresent()) {
            events.setActual(LocalDateTime.now());
            events.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
            events.setIsPublicTrackingEvent(false);
            events.setEntityType(Constants.CONSOLIDATION);
            events.setEntityId(consolidationDetails.get());
            events.setTenantId(TenantContext.getCurrentTenant());
            events.setEventCode(eventCode);
            events.setDescription(description);
            // Persist the event
            eventDao.save(events);
        }
        return events;
    }


    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null || request.getId() == null) {
                log.debug("Request is empty for Carrier Booking delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            long id = request.getId();
            Optional<CarrierBooking> carrierBooking = carrierBookingDao.findById(id);
            if (carrierBooking.isEmpty()) {
                log.debug(CarrierBookingConstants.BOOKING_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            carrierBookingDao.delete(carrierBooking.get());
            log.info("Deleted Carrier Booking details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Carrier Booking list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Pair<Specification<CarrierBooking>, Pageable> tuple = fetchData(request, CarrierBooking.class, tableNames);
            Page<CarrierBooking> carrierBookingPage = carrierBookingDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Carrier Booking list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(carrierBookingPage.getContent()),
                    carrierBookingPage.getTotalPages(),
                    carrierBookingPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }


    @NotNull
    private List<IRunnerResponse> convertEntityListToDtoList(List<CarrierBooking> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(carrierBooking -> {
            CarrierBookingResponse response = modelMapper.map(carrierBooking, CarrierBookingResponse.class);
            responseList.add(response);
        });
        masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID);
        return responseList;
    }

    @Override
    @Transactional(rollbackFor = RunnerException.class)
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        CarrierBookingRequest request = (CarrierBookingRequest) commonRequestModel.getData();
        if (request == null || request.getId() == null) {
            log.error("Request is empty for Carrier Booking update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }
        long id = request.getId();
        Optional<CarrierBooking> oldEntity = carrierBookingDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug(CarrierBookingConstants.BOOKING_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        CarrierBooking carrierBooking = jsonHelper.convertValue(request, CarrierBooking.class);
        carrierBooking.setCreatedAt(oldEntity.get().getCreatedAt());
        carrierBooking.setCreatedBy(oldEntity.get().getCreatedBy());

        beforeSave(carrierBooking, Boolean.FALSE);
        carrierBooking = carrierBookingDao.save(carrierBooking);

        createAuditMetaData(carrierBooking, DBOperationType.UPDATE.name(), oldEntity.get());

        afterSave(carrierBooking, request, Boolean.FALSE);

        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(carrierBooking, CarrierBooking.class));
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            double _start = System.currentTimeMillis();
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null || (request.getId() == null && request.getGuid() == null)) {
                log.error("Request Id and Guid are null for Carrier Booking retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Long id = request.getId();
            Optional<CarrierBooking> carrierBooking;
            if(id != null) {
                carrierBooking = carrierBookingDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                carrierBooking = carrierBookingDao.findByGuid(guid);
            }

            if (carrierBooking.isEmpty()) {
                log.debug(CarrierBookingConstants.BOOKING_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            double current = System.currentTimeMillis();
            log.info("Carrier Booking details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            log.info("Time taken to fetch booking details from db: {} Request Id {}", current - _start, LoggerHelper.getRequestIdFromMDC());
            CarrierBookingResponse carrierBookingResponse = jsonHelper.convertValue(carrierBooking.get(), CarrierBookingResponse.class);
            double _next = System.currentTimeMillis();
            log.info("Time taken to fetch details from db: {} Request Id {}", _next - current, LoggerHelper.getRequestIdFromMDC());
            createCarrierBookingResponse(carrierBooking.get(), carrierBookingResponse);
            return ResponseHelper.buildSuccessResponse(carrierBookingResponse);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    /**
     * To be used to fetch dependent master-data
     *
     * @param carrierBooking:
     * @param carrierBookingResponse:
     */
    private void createCarrierBookingResponse(CarrierBooking carrierBooking, CarrierBookingResponse carrierBookingResponse) {
        try {
            double _start = System.currentTimeMillis();
            var masterListFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllMasterDataInSingleCall(carrierBookingResponse)), executorService);
            var unLocationsFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllLocationDataInSingleCall(carrierBookingResponse)), executorService);
            var vesselsFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllVesselDataInSingleCall(carrierBookingResponse)), executorService);
            var carrierFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllCarrierDataInSingleCall(carrierBookingResponse)), executorService);
            var containerTypeFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllContainerTypesInSingleCall(carrierBookingResponse)), executorService);
            var chargeTypeFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllChargeTypesInSingleCall(carrierBookingResponse)), executorService);
            CompletableFuture.allOf(masterListFuture, unLocationsFuture, vesselsFuture, carrierFuture, containerTypeFuture, chargeTypeFuture).join();
            log.info("Time taken to fetch Master-data from V1: {} ms. || RequestId: {}", System.currentTimeMillis() - _start, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception ex) {
            log.error("Exception during fetching master data in retrieve API for booking number: {} with exception: {}", carrierBooking.getBookingNumber(), ex.getMessage());
        }
    }


    public Runnable withMdc(Runnable runnable) {
        Map<String, String> mdc = MDC.getCopyOfContextMap();
        String token = RequestAuthContext.getAuthToken();
        var userContext = UserContext.getUser();
        return () -> {
            try {
                MDC.setContextMap(mdc);
                RequestAuthContext.setAuthToken(token);
                UserContext.setUser(userContext);
                runnable.run();
            } finally {
                RequestAuthContext.removeToken();
                MDC.clear();
                UserContext.removeUser();
            }
        };
    }


//    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllMasterDataInSingleCall(CarrierBookingResponse carrierBookingResponse) {
        try {
            // Preprocessing
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>(masterDataUtils.createInBulkMasterListRequest(carrierBookingResponse, CarrierBooking.class, fieldNameKeyMap, CarrierBooking.class.getSimpleName(), cacheMap));
            if (!Objects.isNull(carrierBookingResponse.getContainersList()))
                carrierBookingResponse.getContainersList().forEach(c -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(c, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + c.getId(), cacheMap)));
            if (!Objects.isNull(carrierBookingResponse.getPackingList()))
                carrierBookingResponse.getPackingList().forEach(c -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(c, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + c.getId(), cacheMap)));
            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            // fetching from V1 in single call
            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            // Postprocessing
            carrierBookingResponse.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierBooking.class.getSimpleName()), CacheConstants.MASTER_LIST, false, cacheMap));

            if (!Objects.isNull(carrierBookingResponse.getContainersList()))
                carrierBookingResponse.getContainersList().forEach(c -> c.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + c.getId() ), CacheConstants.MASTER_LIST, false, cacheMap)));
            if (!Objects.isNull(carrierBookingResponse.getPackingList()))
                carrierBookingResponse.getPackingList().forEach(c -> c.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Packing.class.getSimpleName() + c.getId() ), CacheConstants.MASTER_LIST, false, cacheMap)));
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CarrierBookingService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }

    }

//    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllLocationDataInSingleCall(CarrierBookingResponse carrierBookingResponse) {
        try {
            // Preprocessing
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> locationCodes = new HashSet<>();
            if (!Objects.isNull(carrierBookingResponse.getCarrierDetails()))
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(carrierBookingResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap)));

            if (!Objects.isNull(carrierBookingResponse.getBookingPaymentsList()))
                carrierBookingResponse.getBookingPaymentsList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, BookingPayment.class, fieldNameKeyMap, BookingPayment.class.getSimpleName() + r.getId(), cacheMap)));

            // fetching from V1 in single call
            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations(), cacheMap);

            // Postprocessing
            if (!Objects.isNull(carrierBookingResponse.getCarrierDetails()))
                carrierBookingResponse.getCarrierDetails().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS, false, cacheMap));
            if (!Objects.isNull(carrierBookingResponse.getBookingPaymentsList()))
                carrierBookingResponse.getBookingPaymentsList().forEach(r -> r.setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(BookingPayment.class.getSimpleName() + r.getId()), CacheConstants.UNLOCATIONS, false, cacheMap)));


            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllLocationDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CarrierBookingService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

//    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllContainerTypesInSingleCall(CarrierBookingResponse carrierBookingResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> containerTypes = new HashSet<>();
            if (!Objects.isNull(carrierBookingResponse.getContainersList()))
                carrierBookingResponse.getContainersList().forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId(), cacheMap)));

            Map<String, EntityTransferContainerType> v1Data = masterDataUtils.fetchInBulkContainerTypes(containerTypes);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CONTAINER_TYPE, containerTypes, new EntityTransferContainerType(), cacheMap);
            
            if (!Objects.isNull(carrierBookingResponse.getContainersList()))
                carrierBookingResponse.getContainersList().forEach(r -> r.setContainerCodeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + r.getId()), CacheConstants.CONTAINER_TYPE, false, cacheMap)));

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        }  catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllContainerTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CarrierBookingService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

//    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllVesselDataInSingleCall(CarrierBookingResponse carrierBookingResponse) {
        try {
            if (!Objects.isNull(carrierBookingResponse.getCarrierDetails())) {
                Map<String, Object> cacheMap = new HashMap<>();
                Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
                Set<String> vesselList = new HashSet<>(masterDataUtils.createInBulkVesselsRequest(carrierBookingResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap));
                Map v1Data = masterDataUtils.fetchInBulkVessels(vesselList);
                masterDataUtils.pushToCache(v1Data, CacheConstants.VESSELS, vesselList, new EntityTransferVessels(), cacheMap);
                carrierBookingResponse.getCarrierDetails().setVesselsMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.VESSELS, false, cacheMap));
            }
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(Arrays.asList()));
        }  catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllVesselDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CarrierBookingService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

//    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCarrierDataInSingleCall(CarrierBookingResponse carrierBookingResponse) {
        try {
            if (!Objects.isNull(carrierBookingResponse.getCarrierDetails())) {
                Map<String, Object> cacheMap = new HashMap<>();
                Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
                Set<String> carriersList = new HashSet<>(masterDataUtils.createInBulkCarriersRequest(carrierBookingResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap));
                Map<String, EntityTransferCarrier> v1Data = masterDataUtils.fetchInBulkCarriers(carriersList);
                masterDataUtils.pushToCache(v1Data, CacheConstants.CARRIER, carriersList, new EntityTransferCarrier(), cacheMap);
                carrierBookingResponse.getCarrierDetails().setCarrierMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.CARRIER, false, cacheMap));
            }
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(Arrays.asList()));
        }  catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCarrierDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CarrierBookingService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }


//    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllChargeTypesInSingleCall(CarrierBookingResponse carrierBookingResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> chargeTypes = new HashSet<>();

            if (!Objects.isNull(carrierBookingResponse.getBookingPaymentsList()))
                carrierBookingResponse.getBookingPaymentsList().forEach(r -> chargeTypes.addAll(masterDataUtils.createInBulkChargeTypeRequest(r, BookingPayment.class, fieldNameKeyMap, BookingCharges.class.getSimpleName() + r.getId(), cacheMap)));
            Map<String, EntityTransferChargeType> v1Data = masterDataUtils.fetchInBulkChargeTypes(chargeTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.CHARGE_TYPE, chargeTypes, new EntityTransferChargeType(), cacheMap);

            if (!Objects.isNull(carrierBookingResponse.getBookingPaymentsList()))
                carrierBookingResponse.getBookingPaymentsList().forEach(r -> r.setChargeTypeMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(BookingPayment.class.getSimpleName() + r.getId()), CacheConstants.CHARGE_TYPE, false, cacheMap)));

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllChargeTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CarrierBookingService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

}
