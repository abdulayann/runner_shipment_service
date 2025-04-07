package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.RAKCDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.interfaces.IKafkaAsyncService;
import com.dpw.runner.shipment.services.service.interfaces.IPickupDeliveryDetailsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class PickupDeliveryDetailsService implements IPickupDeliveryDetailsService {
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    private JsonHelper jsonHelper;

    private AuditLogService auditLogService;
    private IPartiesDao partiesDao;

    private CommonUtils commonUtils;
    private MasterDataUtils masterDataUtils;
    private MasterDataKeyUtils masterDataKeyUtils;
    private ExecutorService executorService;

    private IKafkaAsyncService kafkaAsyncService;
    private IV1Service v1Service;

    @Autowired
    public PickupDeliveryDetailsService(CommonUtils commonUtils, IPartiesDao partiesDao, IPickupDeliveryDetailsDao pickupDeliveryDetailsDao, JsonHelper jsonHelper, AuditLogService auditLogService, MasterDataUtils masterDataUtils, MasterDataKeyUtils masterDataKeyUtils, ExecutorService executorService,
                                        KafkaAsyncService kafkaAsyncService, IV1Service v1Service) {
        this.pickupDeliveryDetailsDao = pickupDeliveryDetailsDao;
        this.jsonHelper = jsonHelper;
        this.auditLogService = auditLogService;
        this.partiesDao = partiesDao;
        this.commonUtils = commonUtils;
        this.masterDataUtils = masterDataUtils;
        this.masterDataKeyUtils = masterDataKeyUtils;
        this.executorService = executorService;
        this.kafkaAsyncService = kafkaAsyncService;
        this.v1Service = v1Service;
    }

    @Transactional
    @Override
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        return createPickupDeliveryDetails(commonRequestModel);
    }

    private ResponseEntity<IRunnerResponse> createPickupDeliveryDetails(CommonRequestModel commonRequestModel) {
        String responseMsg;
        PickupDeliveryDetailsRequest request = (PickupDeliveryDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            String resp = "Request is empty for Pickup Delivery create with Request Id " + LoggerHelper.getRequestIdFromMDC();
            log.debug("Request is empty for Pickup Delivery create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse(resp);
        }
        PickupDeliveryDetails pickupDeliveryDetails = convertRequestToEntity(request);
        try {
            pickupDeliveryDetails = pickupDeliveryDetailsDao.save(pickupDeliveryDetails);
            afterSave(pickupDeliveryDetails, true, request);
            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(pickupDeliveryDetails)
                            .prevData(null)
                            .parent(PickupDeliveryDetails.class.getSimpleName())
                            .parentId(pickupDeliveryDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );
            log.info("Pickup Delivery Details created successfully for Id {} with Request Id {}", pickupDeliveryDetails.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(pickupDeliveryDetails));
    }

    private void beforeSave(PickupDeliveryDetails entity) {
        Long id = entity.getId();
        // Set proper references
        CommonUtils.emptyIfNull(entity.getTiLegsList()).forEach(leg -> {
            Long legId = leg.getId();
            leg.setPickupDeliveryDetailsId(id);
            CommonUtils.emptyIfNull(leg.getTiReferences()).forEach(i -> i.setTiLegId(legId));
            CommonUtils.emptyIfNull(leg.getTiPackages()).forEach(i -> i.setTiLegId(legId));
            CommonUtils.emptyIfNull(leg.getTiContainers()).forEach(i -> i.setTiLegId(legId));
            CommonUtils.emptyIfNull(leg.getTiTruckDriverDetails()).forEach(i -> i.setTiLegId(legId));
        });
    }

    private void afterSave(PickupDeliveryDetails pickupDeliveryDetails, boolean isCreate, PickupDeliveryDetailsRequest request) throws RunnerException {
        List<Parties> partiesList = request.getPartiesList();
        Long id = pickupDeliveryDetails.getId();

        if (partiesList != null) {
            List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(partiesList, Parties.class, isCreate), id, Constants.PICKUP_DELIVERY);
            pickupDeliveryDetails.setPartiesList(updatedParties);
        }
        if (pickupDeliveryDetails.getShipmentId() != null) {
            List<PickupDeliveryDetails> pickupDeliveryDetailsList = pickupDeliveryDetailsDao.findByShipmentId(pickupDeliveryDetails.getShipmentId());
            if (!CommonUtils.listIsNullOrEmpty(pickupDeliveryDetailsList)) {
                List<IRunnerResponse> pickupDeliveryDetailsResponses = convertEntityListToDtoList(pickupDeliveryDetailsList, false);
                CompletableFuture.runAsync(masterDataUtils.withMdc(()-> kafkaAsyncService.pushToKafkaTI(pickupDeliveryDetailsResponses, isCreate, pickupDeliveryDetails.getShipmentId())), executorService);
            }
        }
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        return updatePickupDeliveryDetails(commonRequestModel);
    }

    private ResponseEntity<IRunnerResponse> updatePickupDeliveryDetails(CommonRequestModel commonRequestModel) throws RunnerException{
        String responseMsg;
        PickupDeliveryDetailsRequest request = (PickupDeliveryDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Pickup Delivery update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse("Request is empty for Pickup Delivery update with Request Id " + LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.debug("Request Id is null for Pickup Delivery update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse("Request Id is null for Pickup Delivery update with Request Id " + LoggerHelper.getRequestIdFromMDC());
        }

        long id = request.getId();
        Optional<PickupDeliveryDetails> oldEntity = pickupDeliveryDetailsDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Pickup Delivery Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        PickupDeliveryDetails pickupDeliveryDetails = convertRequestToEntity(request);
        pickupDeliveryDetails.setId(oldEntity.get().getId());
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            beforeSave(pickupDeliveryDetails);
            pickupDeliveryDetails = pickupDeliveryDetailsDao.save(pickupDeliveryDetails);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(pickupDeliveryDetails)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, PickupDeliveryDetails.class))
                            .parent(PickupDeliveryDetails.class.getSimpleName())
                            .parentId(pickupDeliveryDetails.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );

            log.info("Updated the pickup delivery details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        afterSave(pickupDeliveryDetails, false, request);

        return ResponseHelper.buildSuccessResponse(convertEntityToDto(pickupDeliveryDetails));
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Pickup Delivery list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse("Request is empty for Pickup Delivery list with Request Id " + LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<PickupDeliveryDetails>, Pageable> tuple = fetchData(request, PickupDeliveryDetails.class);
            Page<PickupDeliveryDetails> pickupDeliveryDetailsPage = pickupDeliveryDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Pickup Delivery list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(pickupDeliveryDetailsPage.getContent(), request.getPopulateRAKC()),
                    pickupDeliveryDetailsPage.getTotalPages(),
                    pickupDeliveryDetailsPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.debug("Request is empty for Pickup Delivery delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse("Request is empty for Pickup Delivery delete with Request Id " + LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for Pickup Delivery delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse("Request Id null for Pickup Delivery delete with Request Id " + LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            Optional<PickupDeliveryDetails> pickupDeliveryDetails = pickupDeliveryDetailsDao.findById(id);
            if (!pickupDeliveryDetails.isPresent()) {
                log.debug("pickup Delivery Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            String oldEntityJsonString = jsonHelper.convertToJson(pickupDeliveryDetails.get());
            pickupDeliveryDetailsDao.delete(pickupDeliveryDetails.get());

            if (pickupDeliveryDetails.get().getShipmentId() != null) {
                List<PickupDeliveryDetails> pickupDeliveryDetailsList = pickupDeliveryDetailsDao.findByShipmentId(pickupDeliveryDetails.get().getShipmentId());
                if (!CommonUtils.listIsNullOrEmpty(pickupDeliveryDetailsList)) {
                    List<IRunnerResponse> pickupDeliveryDetailsResponses = convertEntityListToDtoList(pickupDeliveryDetailsList, false);
                    CompletableFuture.runAsync(masterDataUtils.withMdc(()-> kafkaAsyncService.pushToKafkaTI(pickupDeliveryDetailsResponses, false, pickupDeliveryDetails.get().getShipmentId())), executorService);
                }
            }
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, PickupDeliveryDetails.class))
                            .parent(PickupDeliveryDetails.class.getSimpleName())
                            .parentId(pickupDeliveryDetails.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );
            log.info("Deleted pickup delivery details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Pickup Delivery retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse("Request is empty for Pickup Delivery retrieve with Request Id " + LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Pickup Delivery retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse("Request Id is null for Pickup Delivery retrieve with Request Id " + LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<PickupDeliveryDetails> pickupDeliveryDetails = pickupDeliveryDetailsDao.findById(id);
            if (!pickupDeliveryDetails.isPresent()) {
                log.debug("Pickup Delivery Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Pickup Delivery details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            PickupDeliveryDetailsResponse response = convertEntityToDto(pickupDeliveryDetails.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }


    private PickupDeliveryDetailsResponse convertEntityToDto(PickupDeliveryDetails pickupDeliveryDetails) {
        return jsonHelper.convertValue(pickupDeliveryDetails, PickupDeliveryDetailsResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<PickupDeliveryDetails> lst, Boolean populateRAKC) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        if (CommonUtils.listIsNullOrEmpty(lst))
            return responseList;
        Map<String, RAKCDetailsResponse> rakcDetailsMap;
        if (Boolean.TRUE.equals(populateRAKC)) {
            Set<String> addressIds = new HashSet<>();
            lst.forEach(pickupDeliveryDetails -> {
                if (CommonUtils.checkAddressNotNull(pickupDeliveryDetails.getTransporterDetail())) {
                    addressIds.add(pickupDeliveryDetails.getTransporterDetail().getAddressId());
                }

                if (CommonUtils.checkAddressNotNull(pickupDeliveryDetails.getSourceDetail())) {
                    addressIds.add(pickupDeliveryDetails.getSourceDetail().getAddressId());
                }

                if (CommonUtils.checkAddressNotNull(pickupDeliveryDetails.getDestinationDetail())) {
                    addressIds.add(pickupDeliveryDetails.getDestinationDetail().getAddressId());
                }
            });
            rakcDetailsMap = this.getRAKCDetailsMap(addressIds.stream().toList());
        } else {
            rakcDetailsMap = new HashMap<>();
        }
        lst.forEach(pickupDeliveryDetail -> {
            PickupDeliveryDetailsResponse response = convertEntityToDto(pickupDeliveryDetail);
            if (Boolean.TRUE.equals(populateRAKC) && !rakcDetailsMap.isEmpty()) {
                this.populateRAKCDetails(response, rakcDetailsMap);
            }
            responseList.add(response);
        });
        return responseList;
    }

    public PickupDeliveryDetails convertRequestToEntity(PickupDeliveryDetailsRequest request) {
        return jsonHelper.convertValue(request, PickupDeliveryDetails.class);
    }

    // V2 Endpoints

    // create
    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> createV2(CommonRequestModel commonRequestModel) {
        return createPickupDeliveryDetails(commonRequestModel);
    }

    // update
    @Override
    public ResponseEntity<IRunnerResponse> updateV2(CommonRequestModel commonRequestModel) throws RunnerException {
        return updatePickupDeliveryDetails(commonRequestModel);
    }

    // list
    @Override
    public ResponseEntity<IRunnerResponse> listV2(CommonRequestModel commonRequestModel) {
        return this.list(commonRequestModel);
    }

    // delete
    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> deleteV2(CommonRequestModel commonRequestModel) {
        return this.delete(commonRequestModel);
    }

    // retrieve
    @Override
    public ResponseEntity<IRunnerResponse> retrieveByIdV2(CommonRequestModel commonRequestModel) {
        return this.retrieveById(commonRequestModel);
    }


    private Map<String, Object> fetchAllMasterDataByKey(PickupDeliveryDetails pickupDeliveryDetails, PickupDeliveryDetailsResponse pickupDeliveryDetailsResponse) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllMasterDataInSingleCall(pickupDeliveryDetails, pickupDeliveryDetailsResponse, masterDataResponse)), executorService);
        CompletableFuture.allOf(masterListFuture).join();
        return masterDataResponse;
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllMasterDataInSingleCall (PickupDeliveryDetails pickupDeliveryDetails, PickupDeliveryDetailsResponse pickupDeliveryDetailsResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            AtomicInteger count = new AtomicInteger();
            Set<MasterListRequest> listRequests = new HashSet<>(masterDataUtils.createInBulkMasterListRequest(pickupDeliveryDetailsResponse, PickupDeliveryDetails.class, fieldNameKeyMap, PickupDeliveryDetails.class.getSimpleName(), cacheMap));
            // Populate all the master data in inner objects
            if (!CommonUtils.listIsNullOrEmpty(pickupDeliveryDetails.getTiLegsList())) {
                pickupDeliveryDetailsResponse.getTiLegsList().forEach(leg -> {
                    listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(leg, TiLegs.class, fieldNameKeyMap, TiLegs.class.getSimpleName() + count.incrementAndGet(), cacheMap));
                    // Add master data fields for sub entities
                    if(!CommonUtils.listIsNullOrEmpty(leg.getTiReferences())) {
                        leg.getTiReferences().forEach(i -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(i, TiReferences.class, fieldNameKeyMap, TiReferences.class.getSimpleName() + count.incrementAndGet(), cacheMap)));
                    }
                    if(!CommonUtils.listIsNullOrEmpty(leg.getTiPackages())) {
                        leg.getTiPackages().forEach(i -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(i, TiPackages.class, fieldNameKeyMap, TiPackages.class.getSimpleName() + count.incrementAndGet(), cacheMap)));
                    }
                    if(!CommonUtils.listIsNullOrEmpty(leg.getTiContainers())) {
                        leg.getTiContainers().forEach(i -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(i, TiContainers.class, fieldNameKeyMap, TiContainers.class.getSimpleName() + count.incrementAndGet(), cacheMap)));
                    }
                });
            }

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, "ItemDescription", "ValuenDesc", "Cascade"));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            if(masterDataResponse != null) {
                pickupDeliveryDetailsResponse.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(TiContainers.class.getSimpleName() + count.get()), CacheConstants.MASTER_LIST, cacheMap));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occured in CompletableFuture: addAllMasterDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), AwbService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }

    }

    private void populateRAKCDetails(PickupDeliveryDetailsResponse pickupDeliveryDetails, Map<String, RAKCDetailsResponse> rakcDetailsMap) {
        if (CommonUtils.checkAddressNotNull(pickupDeliveryDetails.getTransporterDetail())) {
            pickupDeliveryDetails.getTransporterDetail().setRAKCDetails(rakcDetailsMap.getOrDefault(pickupDeliveryDetails.getTransporterDetail().getAddressId(), null));
        }

        if (CommonUtils.checkAddressNotNull(pickupDeliveryDetails.getSourceDetail())) {
            pickupDeliveryDetails.getSourceDetail().setRAKCDetails(rakcDetailsMap.getOrDefault(pickupDeliveryDetails.getSourceDetail().getAddressId(), null));
        }

        if (CommonUtils.checkAddressNotNull(pickupDeliveryDetails.getDestinationDetail())) {
            pickupDeliveryDetails.getDestinationDetail().setRAKCDetails(rakcDetailsMap.getOrDefault(pickupDeliveryDetails.getDestinationDetail().getAddressId(), null));
        }
    }

    private Map<String, RAKCDetailsResponse> getRAKCDetailsMap(List<String> addressIds) {
        if (CommonUtils.listIsNullOrEmpty(addressIds)) {
            return Collections.emptyMap();
        }

        CommonV1ListRequest request = convertV1InCriteriaRequest("Id", addressIds);
        V1DataResponse addressResponse = v1Service.addressList(request);

        List<RAKCDetailsResponse> rakcDetailsList =
                jsonHelper.convertValueToList(addressResponse.getEntities(), RAKCDetailsResponse.class);

        return CommonUtils.listIsNullOrEmpty(rakcDetailsList)
                ? Collections.emptyMap()
                : rakcDetailsList.stream().collect(Collectors.toMap(rakc -> String.valueOf(rakc.getId()), Function.identity()));
    }

    private CommonV1ListRequest convertV1InCriteriaRequest(String filterValue, List<?> values) {
        List<String> itemType = new ArrayList<>();
        itemType.add(filterValue);
        List<List<?>> param = new ArrayList<>();
        param.add(values);
        List<Object> criteria = new ArrayList<>(Arrays.asList(itemType, "in", param));
        return CommonV1ListRequest.builder().criteriaRequests(criteria).build();
    }
}
