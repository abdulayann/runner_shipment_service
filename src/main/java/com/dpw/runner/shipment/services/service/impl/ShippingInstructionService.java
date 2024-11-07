package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.ContainerDao;
import com.dpw.runner.shipment.services.dao.impl.EventDao;
import com.dpw.runner.shipment.services.dao.impl.PackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShippingInstructionDao;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.SIStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.service.interfaces.IShippingInstructionService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
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
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;


@Service
@Slf4j
public class ShippingInstructionService implements IShippingInstructionService {
    ExecutorService executorService = Executors.newFixedThreadPool(10);
    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IShippingInstructionDao shippingInstructionDao;

    @Autowired
    private PackingDao packingDao;

    @Autowired
    private ContainerDao containerDao;

    @Autowired
    private EventDao eventDao;

    @Autowired
    private AuditLogService auditLogService;

    @Autowired
    private MasterDataUtils masterDataUtils;

    private final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("origin", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("origin").build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destination").build()),
            Map.entry("originPort", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("originPort").build()),
            Map.entry("destinationPort", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destinationPort").build()),
            Map.entry("status", RunnerEntityMapping.builder().tableName(Constants.SHIPPING_INSTRUCTION).dataType(SIStatus.class).fieldName("status").build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName(Constants.SHIPPING_INSTRUCTION).dataType(String.class).fieldName("createdBy").build()),
            Map.entry("id", RunnerEntityMapping.builder().tableName(Constants.SHIPPING_INSTRUCTION).dataType(String.class).fieldName("id").build())
    );

    @Override
    @Transactional(rollbackFor = RunnerException.class)
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) throws RunnerException {
        ShippingInstructionRequest request = (ShippingInstructionRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for ShippingInstruction Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }
        ShippingInstruction shippingInstructionDetails = jsonHelper.convertValue(request, ShippingInstruction.class);
        shippingInstructionDetails = shippingInstructionDao.save(shippingInstructionDetails);
        afterSave(shippingInstructionDetails, request, Boolean.TRUE);
        createAuditMetaData(shippingInstructionDetails, DBOperationType.CREATE.name(), null);
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(shippingInstructionDetails, ShippingInstructionResponse.class));
    }


    private void createAuditMetaData(ShippingInstruction shippingInstruction, String operation, ShippingInstruction prevData){
        try {
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(shippingInstruction)
                            .prevData(prevData)
                            .parent(ShippingInstruction.class.getSimpleName())
                            .parentId(shippingInstruction.getId())
                            .operation(operation).build()
            );
        } catch (Exception e) {
            log.error(e.getMessage());
        }

    }

    private void afterSave(ShippingInstruction shippingInstruction, ShippingInstructionRequest request, Boolean isCreate) throws RunnerException {
        Long shippingInstructionId = shippingInstruction.getId();
        List<ContainerRequest> containerRequestList = request.getContainersList();
        List<PackingRequest> packingRequestList = request.getPackingList();
        List<EventsRequest> eventsRequestList = request.getEventsList();

        if (packingRequestList != null)
            shippingInstruction.setPackingList(packingDao.updateEntityFromShippingInstruction(commonUtils.convertToEntityList(packingRequestList, Packing.class), shippingInstructionId));

        if (containerRequestList != null) {
            List<Containers> containers = containerDao.updateEntityFromShippingInstruction(commonUtils.convertToEntityList(containerRequestList, Containers.class), shippingInstructionId);
            shippingInstruction.setContainersList(containers);
        }

        if (eventsRequestList != null) {
            List<Events> eventsList = commonUtils.convertToEntityList(eventsRequestList, Events.class, isCreate);
            eventDao.updateEntityFromOtherEntity(eventsList, shippingInstructionId, Constants.SHIPPING_INSTRUCTION);
            shippingInstruction.setEventsList(eventsList);
        }

    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null || request.getId() == null) {
                log.debug("Request is empty for shippingInstruction delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            long id = request.getId();
            Optional<ShippingInstruction> shippingInstruction = shippingInstructionDao.findById(id);
            if (shippingInstruction.isEmpty()) {
                log.debug(ShippingInstructionConstants.SHIPPING_INSTRUCTIONS_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            shippingInstructionDao.delete(shippingInstruction.get());
            log.info("Deleted shippingInstruction details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
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
                log.error("Request is empty for ShippingInstruction list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Pair<Specification<ShippingInstruction>, Pageable> tuple = fetchData(request, ShippingInstruction.class, tableNames);
            Page<ShippingInstruction> shippingInstructionPage = shippingInstructionDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("ShippingInstruction list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(shippingInstructionPage.getContent()),
                    shippingInstructionPage.getTotalPages(),
                    shippingInstructionPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }


    @NotNull
    private List<IRunnerResponse> convertEntityListToDtoList(List<ShippingInstruction> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(shippingInstruction -> {
            ShippingInstructionResponse response = modelMapper.map(shippingInstruction, ShippingInstructionResponse.class);
            responseList.add(response);
        });
        masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID);
        return responseList;
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShippingInstructionRequest request = (ShippingInstructionRequest) commonRequestModel.getData();
        if (request == null || request.getId() == null) {
            log.error("Request is empty for shipping_instruction update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }
        long id = request.getId();
        Optional<ShippingInstruction> oldEntity = shippingInstructionDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug(ShippingInstructionConstants.SHIPPING_INSTRUCTIONS_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ShippingInstruction shippingInstruction = jsonHelper.convertValue(request, ShippingInstruction.class);
        shippingInstruction.setCreatedAt(oldEntity.get().getCreatedAt());
        shippingInstruction.setCreatedBy(oldEntity.get().getCreatedBy());

        shippingInstruction = shippingInstructionDao.save(shippingInstruction);
        createAuditMetaData(shippingInstruction, DBOperationType.UPDATE.name(), oldEntity.get());
        afterSave(shippingInstruction, request, Boolean.FALSE);
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(shippingInstruction, ShippingInstruction.class));
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            double _start = System.currentTimeMillis();
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null || (request.getId() == null && request.getGuid() == null)) {
                log.error("Request Id and Guid are null for ShippingInstruction retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Long id = request.getId();
            Optional<ShippingInstruction> shippingInstruction;
            if(id != null) {
                shippingInstruction = shippingInstructionDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                shippingInstruction = shippingInstructionDao.findByGuid(guid);
            }

            if (shippingInstruction.isEmpty()) {
                log.debug(ShippingInstructionConstants.SHIPPING_INSTRUCTIONS_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            double current = System.currentTimeMillis();
            log.info("ShippingInstruction details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            log.info("Time taken to fetch shipping_instruction from db: {} Request Id {}", current - _start, LoggerHelper.getRequestIdFromMDC());
            ShippingInstructionResponse shippingInstructionResponse = jsonHelper.convertValue(shippingInstruction.get(), ShippingInstructionResponse.class);
            double _next = System.currentTimeMillis();
            log.info("Time taken to fetch details from db: {} Request Id {}", _next - current, LoggerHelper.getRequestIdFromMDC());
            createShippingInstructionResponse(shippingInstruction.get(), shippingInstructionResponse);
            return ResponseHelper.buildSuccessResponse(shippingInstructionResponse);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    /**
     * To be used to fetch dependent master-data
     *
     * @param shippingInstruction:
     * @param shippingInstructionResponse:
     */
    private void createShippingInstructionResponse(ShippingInstruction shippingInstruction, ShippingInstructionResponse shippingInstructionResponse) {
        try {
            double _start = System.currentTimeMillis();
            var masterListFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllMasterDataInSingleCall(shippingInstructionResponse)), executorService);
            var unLocationsFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllLocationDataInSingleCall(shippingInstructionResponse)), executorService);
            var vesselsFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllVesselDataInSingleCall(shippingInstructionResponse)), executorService);
            var carrierFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllCarrierDataInSingleCall(shippingInstructionResponse)), executorService);
            var containerTypeFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllContainerTypesInSingleCall(shippingInstructionResponse)), executorService);
            CompletableFuture.allOf(masterListFuture, unLocationsFuture, vesselsFuture, carrierFuture, containerTypeFuture).join();
            log.info("Time taken to fetch Master-data from V1: {} ms. || RequestId: {}", System.currentTimeMillis() - _start, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception ex) {
            log.error("Exception during fetching master data in retrieve API for booking number: {} with exception: {}", shippingInstruction.getCarrierBookingNumber(), ex.getMessage());
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
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllMasterDataInSingleCall(ShippingInstructionResponse shippingInstructionResponse) {
        try {
            // Preprocessing
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>(masterDataUtils.createInBulkMasterListRequest(shippingInstructionResponse, ShippingInstruction.class, fieldNameKeyMap, ShippingInstruction.class.getSimpleName(), cacheMap));
            if (!Objects.isNull(shippingInstructionResponse.getContainersList()))
                shippingInstructionResponse.getContainersList().forEach(c -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(c, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + c.getId(), cacheMap)));
            if (!Objects.isNull(shippingInstructionResponse.getPackingList()))
                shippingInstructionResponse.getPackingList().forEach(c -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(c, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + c.getId(), cacheMap)));
            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            // fetching from V1 in single call
            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            // Postprocessing
            shippingInstructionResponse.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CustomerBooking.class.getSimpleName()), CacheConstants.MASTER_LIST, false, cacheMap));

            if (!Objects.isNull(shippingInstructionResponse.getContainersList()))
                shippingInstructionResponse.getContainersList().forEach(c -> c.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + c.getId() ), CacheConstants.MASTER_LIST, false, cacheMap)));
            if (!Objects.isNull(shippingInstructionResponse.getPackingList()))
                shippingInstructionResponse.getPackingList().forEach(c -> c.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Packing.class.getSimpleName() + c.getId() ), CacheConstants.MASTER_LIST, false, cacheMap)));
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ShippingInstructionService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }

    }

    //    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllLocationDataInSingleCall(ShippingInstructionResponse shippingInstructionResponse) {
        try {
            // Preprocessing
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> locationCodes = new HashSet<>();
            if (!Objects.isNull(shippingInstructionResponse.getCarrierDetails()))
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(shippingInstructionResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap)));

            // fetching from V1 in single call
            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations(), cacheMap);

            // Postprocessing
            if (!Objects.isNull(shippingInstructionResponse.getCarrierDetails()))
                shippingInstructionResponse.getCarrierDetails().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS, false, cacheMap));

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllLocationDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ShippingInstructionService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    //    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllContainerTypesInSingleCall(ShippingInstructionResponse shippingInstructionResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> containerTypes = new HashSet<>();
            if (!Objects.isNull(shippingInstructionResponse.getContainersList()))
                shippingInstructionResponse.getContainersList().forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId(), cacheMap)));

            Map<String, EntityTransferContainerType> v1Data = masterDataUtils.fetchInBulkContainerTypes(containerTypes);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CONTAINER_TYPE, containerTypes, new EntityTransferCarrier(), cacheMap);

            if (!Objects.isNull(shippingInstructionResponse.getContainersList()))
                shippingInstructionResponse.getContainersList().forEach(r -> r.setContainerCodeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + r.getId()), CacheConstants.CONTAINER_TYPE, false, cacheMap)));

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        }  catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllContainerTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ShippingInstructionService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    //    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllVesselDataInSingleCall(ShippingInstructionResponse shippingInstructionResponse) {
        try {
            if (!Objects.isNull(shippingInstructionResponse.getCarrierDetails())) {
                Map<String, Object> cacheMap = new HashMap<>();
                Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
                Set<String> vesselList = new HashSet<>(masterDataUtils.createInBulkVesselsRequest(shippingInstructionResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap));
                Map v1Data = masterDataUtils.fetchInBulkVessels(vesselList);
                masterDataUtils.pushToCache(v1Data, CacheConstants.VESSELS, vesselList, new EntityTransferCarrier(), cacheMap);
                shippingInstructionResponse.getCarrierDetails().setVesselsMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.VESSELS, false, cacheMap));
            }
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(Arrays.asList()));
        }  catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllVesselDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ShippingInstructionService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    //    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCarrierDataInSingleCall(ShippingInstructionResponse shippingInstructionResponse) {
        try {
            if (!Objects.isNull(shippingInstructionResponse.getCarrierDetails())) {
                Map<String, Object> cacheMap = new HashMap<>();
                Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
                Set<String> carriersList = new HashSet<>(masterDataUtils.createInBulkCarriersRequest(shippingInstructionResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap));
                Map v1Data = masterDataUtils.fetchInBulkCarriers(carriersList);
                masterDataUtils.pushToCache(v1Data, CacheConstants.CARRIER, carriersList, new EntityTransferCarrier(), cacheMap);
                shippingInstructionResponse.getCarrierDetails().setCarrierMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.CARRIER, false, cacheMap));
            }
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(Arrays.asList()));
        }  catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCarrierDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), ShippingInstructionService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }
}
