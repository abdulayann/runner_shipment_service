package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.MdmConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.MDMServiceResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dto.request.mdm.MdmListCriteriaRequest;
import com.dpw.runner.shipment.services.dto.request.mdm.MdmTaskApproveOrRejectRequest;
import com.dpw.runner.shipment.services.dto.request.mdm.MdmTaskCreateRequest;
import com.dpw.runner.shipment.services.dto.response.mdm.MDMTaskRetrieveResponse;
import com.dpw.runner.shipment.services.dto.response.mdm.MdmTaskCreateResponse;
import com.dpw.runner.shipment.services.dto.v1.request.ApprovalPartiesRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CompanyDetailsRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CreateShipmentTaskFromBookingTaskRequest;
import com.dpw.runner.shipment.services.exception.exceptions.GenericException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.http.*;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.retry.RetryCallback;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
public class MDMServiceAdapter implements IMDMServiceAdapter {
    private final RestTemplate restTemplate;
    private final String baseUrl;

    @Autowired
    JsonHelper jsonHelper;

    @Autowired
    ObjectMapper objectMapper;

    @Autowired
    CacheManager cacheManager;

    @Autowired
    CustomKeyGenerator keyGenerator;

    @Autowired
    MasterDataUtils masterDataUtils;

    @Value("${mdm.creditDetails}")
    String creditConfigUrl;

    @Value("${mdm.createShipmentTaskFromBooking}")
    String createShipmentTaskFromBookingUrl;

    @Value("${mdm.createNonBillableCustomer}")
    String createNonBillableCustomer;

    @Value("${mdm.departmentListUrl}")
    String departmentListUrl;

    @Value("${mdm.containerTypeListUrl}")
    String containerTypeListUrl;

    @Value("${mdm.createTaskUrl}")
    String createTaskUrl;

    @Value("${mdm.approveOrRejectTaskUrl}")
    String approveOrRejectTaskUrl;

    @Value("${mdm.listTaskUrl}")
    String listTaskUrl;

    @Value("${mdm.getTaskUrl}")
    String getTaskUrl;

    @Value("${mdm.govtIdListUrl}")
    String govtIdListUrl;

    RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    public MDMServiceAdapter(@Qualifier("restTemplateForMDM") RestTemplate restTemplate,
                                @Value("${mdm.baseUrl}") String baseUrl) {
        this.restTemplate = restTemplate;
        this.baseUrl = baseUrl;
    }

    @Override
    public ResponseEntity<IRunnerResponse> getCreditInfo(CommonRequestModel commonRequestModel) throws RunnerException {
        String url = baseUrl + creditConfigUrl;
        ApprovalPartiesRequest request = (ApprovalPartiesRequest) commonRequestModel.getData();
        log.info("Request id {} MDM Request {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        try {
            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            ResponseEntity<?> response = restTemplate.exchange(RequestEntity.post(URI.create(url)).headers(headers).body(jsonHelper.convertToJson(request)), Object.class);
            log.info("Request id {} MDM Response {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(response));
            return ResponseHelper.buildDependentServiceResponse(response.getBody(), 0, 0);
        }catch (Exception ex){
            log.error("Request id {} MDM Credit Details Failed due to : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(ex.getMessage()));
            throw new RunnerException("Error from MDM while fetching credit limit: " + ex.getMessage());
        }
    }

    @Override
    public String getApprovalStausForParties(CommonRequestModel commonRequestModel) throws RunnerException {
        ResponseEntity<?> response = this.getCreditInfo(commonRequestModel);
        if(response.getStatusCode().equals(HttpStatus.OK)) {
            try {
                DependentServiceResponse responseBody = objectMapper.convertValue(response.getBody(), DependentServiceResponse.class);
                LinkedHashMap<String,Object> dataList = (LinkedHashMap<String, Object>) responseBody.getData();
                String finalStatus = null;
                if (dataList != null && !dataList.isEmpty()) {
                    Map<String, Object> firstDataObject = ((List<Map<String, Object>>) dataList.get("data")).get(0);
                    if (Boolean.FALSE.equals(firstDataObject.get("isRestricted")))
                        return CustomerBookingConstants.MDM_FINAL_STATUS_NO_APPROVAL_NEEDED;
                    finalStatus = (String) firstDataObject.get("finalStatus");
                }
                log.info("Request id {} MDM Response {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(responseBody));
                return finalStatus;
            }catch (Exception ex){
                log.error("Error getting the approval status for the parties : {}", commonRequestModel.getData());
                log.error("ERROR : {}", ex.getMessage());
            }
        }
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> createShipmentTaskFromBooking(CommonRequestModel commonRequestModel) throws RunnerException {
        String url = baseUrl + createShipmentTaskFromBookingUrl;
        CreateShipmentTaskFromBookingTaskRequest request = (CreateShipmentTaskFromBookingTaskRequest) commonRequestModel.getData();
        try {
            log.info("Calling MDM createShipmentTaskFromBooking api for requestId : {} Request for {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
            var resp = retryTemplate.execute((RetryCallback<ResponseEntity<IRunnerResponse>, Exception>) context -> {
                ResponseEntity<DependentServiceResponse> response = restTemplate.exchange(
                        RequestEntity.post(URI.create(url)).body(jsonHelper.convertToJson(request)),
                        DependentServiceResponse.class
                );
                return ResponseHelper.buildDependentServiceResponse(response.getBody(), 0, 0);
            });
            log.info("MDM createShipmentTaskFromBooking api response for requestId - {} : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(jsonHelper.convertToJson(resp)));
            return resp;
        } catch (Exception ex) {
            log.error("MDM Credit Details Failed due to: {}", jsonHelper.convertToJson(ex.getMessage()));
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> createNonBillableCustomer(CommonRequestModel commonRequestModel) throws RunnerException {
        String url = baseUrl + createNonBillableCustomer;
        CompanyDetailsRequest request =  jsonHelper.convertValueWithJsonNullable(commonRequestModel.getDependentData(), CompanyDetailsRequest.class);
        try {
            log.info("Calling MDM createNonBillableCustomer api for requestId : {} Request for {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
            restTemplate.getMessageConverters().add(0, new StringHttpMessageConverter(StandardCharsets.UTF_8));
            ResponseEntity<DependentServiceResponse> response = restTemplate.exchange(
                    RequestEntity.post(URI.create(url)).body(jsonHelper.convertToJsonWithNulls(request)),
                    DependentServiceResponse.class
                );
            log.info("MDM createNonBillableCustomer api response for requestId - {} : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(jsonHelper.convertToJson(response.getBody())));
            return ResponseHelper.buildSuccessResponse(response.getBody());
        } catch (Exception ex) {
            String errorMessage = ex.getMessage();
            if(ex instanceof HttpClientErrorException) {
                String json = ((HttpClientErrorException) ex).getResponseBodyAsString();
                String msg = jsonHelper.readFromJson(json, MDMServiceResponse.class).getMessage();
                errorMessage = msg != null ? msg : errorMessage;
            }
            log.error("MDM createNonBillableCustomer Failed due to: {}", jsonHelper.convertToJson(errorMessage));
            return ResponseHelper.buildFailedResponse(errorMessage);
        }
    }

    @Override
    public List<Map<String, Object>> getDepartmentList(String transportMode, String shipmentType, String module) {
        String url = baseUrl + departmentListUrl;
        try {
            MdmListCriteriaRequest listCriteriaRequest = MdmListCriteriaRequest.builder().pageNo(0).pageSize(100).searchCriteriaList(
            List.of(
                    MdmListCriteriaRequest.SearchCriteria.builder().field(MdmConstants.MODULES_FIELD).operator(MdmConstants.LIKE_OPERATOR).value(module).build(),
                    MdmListCriteriaRequest.SearchCriteria.builder().field(MdmConstants.TRANSPORT_MODE_FIELD).operator(MdmConstants.LIKE_OPERATOR).value(transportMode).build(),
                    MdmListCriteriaRequest.SearchCriteria.builder().field(MdmConstants.SHIPMENT_TYPE_FIELD).operator(MdmConstants.LIKE_OPERATOR).value(shipmentType).build()
            )).build();

            ResponseEntity<DependentServiceResponse> responseEntity = restTemplate.postForEntity(url, jsonHelper.convertToJson(listCriteriaRequest), DependentServiceResponse.class);
            DependentServiceResponse dependentServiceResponse = Optional.ofNullable(responseEntity.getBody()).orElse(new DependentServiceResponse());
            log.info("MDM getDepartmentList response for requestId - {} : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(jsonHelper.convertToJson(responseEntity)));
            return jsonHelper.convertValue(dependentServiceResponse.getData(), new TypeReference<List<Map<String, Object>>>() {});
        }
        catch (Exception e) {
            log.error("MDM Service - error while fetching departments list", e);
        }
        return Collections.emptyList();
    }

    @Override
    public List<Map<String, Object>> getTaskList(String entityUuid, String entityType, String status, String taskType) {
        String url = baseUrl + listTaskUrl;
        try {
            MdmListCriteriaRequest listCriteriaRequest = MdmListCriteriaRequest.builder().pageNo(0).pageSize(100).searchCriteriaList(
                List.of(
                    MdmListCriteriaRequest.SearchCriteria.builder().field(MdmConstants.ENTITY_UUID).operator(MdmConstants.EQ).value(entityUuid).build(),
                    MdmListCriteriaRequest.SearchCriteria.builder().field(MdmConstants.STATUS).operator(MdmConstants.EQ).value(status).build(),
                    MdmListCriteriaRequest.SearchCriteria.builder().field(MdmConstants.ENTITY_TYPE).operator(MdmConstants.EQ).value(entityType).build(),
                    MdmListCriteriaRequest.SearchCriteria.builder().field(MdmConstants.TASK_TYPE).operator(MdmConstants.EQ).value(taskType).build()
                )).build();

            ResponseEntity<DependentServiceResponse> responseEntity = restTemplate.postForEntity(url, jsonHelper.convertToJson(listCriteriaRequest), DependentServiceResponse.class);
            DependentServiceResponse dependentServiceResponse = Optional.ofNullable(responseEntity.getBody()).orElse(new DependentServiceResponse());
            log.info("MDM getTask response for requestId - {} : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(jsonHelper.convertToJson(responseEntity)));
            return jsonHelper.convertValue(dependentServiceResponse.getData(), new TypeReference<List<Map<String, Object>>>() {});
        }
        catch (Exception e) {
            log.error("MDM Service - error while fetching task list", e);
        }
        return Collections.emptyList();
    }

    @Override
    public DependentServiceResponse getContainerTypes() throws RunnerException {
        String url = baseUrl + containerTypeListUrl;
        try{
            MdmListCriteriaRequest listCriteriaRequest = MdmListCriteriaRequest.builder().pageNo(0).pageSize(100).build();
            ResponseEntity<DependentServiceResponse> responseEntity = restTemplate.postForEntity(url, jsonHelper.convertToJson(listCriteriaRequest), DependentServiceResponse.class);
            return Optional.ofNullable(responseEntity.getBody()).orElse(new DependentServiceResponse());
        } catch (Exception ex){
            log.error("MDM Service - error while fetching container type list", ex.getMessage());
            throw new RunnerException("Error while fetching container type list: " + ex.getMessage());
        }
    }

    @Override
    public MdmTaskCreateResponse createTask(MdmTaskCreateRequest request) throws RunnerException {
        String url = baseUrl + createTaskUrl;
        try {
            log.info("Calling MDM createTask api for requestId : {} Request for {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
            ResponseEntity<DependentServiceResponse> response =  restTemplate.postForEntity(url, jsonHelper.convertToJson(request), DependentServiceResponse.class);
            log.info("MDM createTask api response for requestId - {} : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(jsonHelper.convertToJson(response.getBody())));
            Object data = Objects.requireNonNull(response.getBody()).getData();

            if (data instanceof List<?> dataList && !dataList.isEmpty()) {
                Object firstElement = dataList.get(0);
                return jsonHelper.convertValue(firstElement, MdmTaskCreateResponse.class);
            }
            return jsonHelper.convertValue(Objects.requireNonNull(response.getBody()).getData(), MdmTaskCreateResponse.class);
        } catch (Exception ex) {
            String errorMessage = ex.getMessage();
            log.error("MDM createTask Failed due to: {}", jsonHelper.convertToJson(errorMessage));
            throw new RunnerException(errorMessage);
        }
    }

    @Override
    public void approveOrRejectTask(MdmTaskApproveOrRejectRequest request) throws RunnerException {
        String url = baseUrl + approveOrRejectTaskUrl;
        try {
            log.info("Calling MDM approveOrReject api for requestId : {} Request for {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
            ResponseEntity<DependentServiceResponse> response = restTemplate.exchange(
                RequestEntity.post(URI.create(url)).body(jsonHelper.convertToJson(request)),
                DependentServiceResponse.class
            );
            log.info("MDM approveOrReject api response for requestId - {} : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(jsonHelper.convertToJson(response.getBody())));
        } catch (Exception ex) {
            String errorMessage = ex.getMessage();
            log.error("MDM approveOrReject Failed due to: {}", jsonHelper.convertToJson(errorMessage));
            throw new RunnerException(errorMessage);
        }
    }

    @Override
    public MDMTaskRetrieveResponse getTask(String taskUuid, Long id) throws RunnerException {
        String url = baseUrl + getTaskUrl + "?uuid=" + taskUuid;
        if(StringUtility.isEmpty(taskUuid)){
            url = baseUrl + getTaskUrl + "?id=" + id;
        }
        try {
            log.info("Calling MDM Get Task api for requestId : {} Request for {}", LoggerHelper.getRequestIdFromMDC());
            ResponseEntity<DependentServiceResponse> response =  restTemplate.getForEntity(url, DependentServiceResponse.class);
            log.info("MDM getTask api response for requestId - {} : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(jsonHelper.convertToJson(response.getBody())));
            Object data = Objects.requireNonNull(response.getBody()).getData();

            if (data instanceof List<?> dataList && !dataList.isEmpty()) {
                Object firstElement = dataList.get(0);
                return jsonHelper.convertValue(firstElement, MDMTaskRetrieveResponse.class);
            }
            return jsonHelper.convertValue(Objects.requireNonNull(response.getBody()).getData(), MDMTaskRetrieveResponse.class);
        } catch (Exception ex) {
            String errorMessage = ex.getMessage();
            log.error("MDM getTask Failed due to: {}", jsonHelper.convertToJson(errorMessage));
            throw new RunnerException(errorMessage);
        }
    }

    private List<Map<String, Object>> getFirmsCodeList(Set<String> orgIds) {
        String url = baseUrl + govtIdListUrl;
        if (orgIds == null || orgIds.isEmpty()) {
            return Collections.emptyList();
        }
        try {
            orgIds = orgIds.stream().filter(orgId -> orgId != null && !orgId.trim().isEmpty()).collect(Collectors.toSet());
            List<MdmListCriteriaRequest.SearchCriteria> criteriaList = List.of(
                    MdmListCriteriaRequest.SearchCriteria.builder().field("entityType").operator("eq").value("ORGANIZATION").build(),
                    MdmListCriteriaRequest.SearchCriteria.builder().field("entityId").operator("in").values(new ArrayList<>(orgIds)).build()
            );
            MdmListCriteriaRequest listCriteriaRequest = MdmListCriteriaRequest.builder().pageNo(0).pageSize(100).searchCriteriaList(criteriaList).build();

            ResponseEntity<DependentServiceResponse> responseEntity = restTemplate.postForEntity(url, jsonHelper.convertToJson(listCriteriaRequest), DependentServiceResponse.class);
            DependentServiceResponse dependentServiceResponse = Optional.ofNullable(responseEntity.getBody()).orElse(new DependentServiceResponse());
            log.info("MDM getGovtId(FirmsCode) response for requestId - {} : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(jsonHelper.convertToJson(responseEntity)));
            return jsonHelper.convertValue(dependentServiceResponse.getData(), new TypeReference<List<Map<String, Object>>>() {});
        }
        catch (Exception e) {
            log.error("MDM Service - error while fetching GovtId(FirmsCode) list", e);
        }
        return Collections.emptyList();
    }

    public Map<String, String> getFirmsCodeListFromCache(Set<String> orgIds) {
        if(Objects.isNull(orgIds))
            return new HashMap<>();
        orgIds = orgIds.stream().filter(orgId -> orgId != null && !orgId.trim().isEmpty()).collect(Collectors.toSet());
        Map<String, String> responseMap = new HashMap<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        if (cache == null) {
            log.warn("Cache '{}' not found, fetching all data from MDM for FIRMSCode", CacheConstants.CACHE_KEY_MASTER_DATA);
            List<Map<String, Object>> firmsCodeResp = getFirmsCodeList(orgIds);
            return getFirmsCodeMap(firmsCodeResp);
        }
        try{
            Set<String> fetchFirmsCodeFromMDM = new HashSet<>();
            String customCacheKey = CacheConstants.FIRMS_CODE;
            for(String orgId : orgIds) {
                Cache.ValueWrapper value = cache.get(keyGenerator.customCacheKeyForMasterData(customCacheKey, orgId));
                if(Objects.isNull(value))
                    fetchFirmsCodeFromMDM.add(orgId);
                else
                    responseMap.put(orgId, (String) value.get());
            }
            if(!fetchFirmsCodeFromMDM.isEmpty()) {
                List<Map<String, Object>> firmsCodeResp = getFirmsCodeList(fetchFirmsCodeFromMDM);
                Map<String, String> orgToFirmsCodeMap = getFirmsCodeMap(firmsCodeResp);
                responseMap.putAll(orgToFirmsCodeMap);
                masterDataUtils.pushToCache(orgToFirmsCodeMap, customCacheKey, fetchFirmsCodeFromMDM, "", null);
            }
            return responseMap;
        }
        catch (Exception e) {
            throw new GenericException(e);
        }
    }

    private Map<String, String> getFirmsCodeMap(List<Map<String,Object>> orgToFirmsCodeResp){
        if (orgToFirmsCodeResp == null || orgToFirmsCodeResp.isEmpty()) {
            return Collections.emptyMap();
        }

        return orgToFirmsCodeResp.stream()
                .map(m -> {
                    Object entityId = m.get("entityId");
                    Object firmsCode = m.get("govtIdNumber");
                    if (entityId == null || firmsCode == null) return null;

                    String key = String.valueOf(entityId);
                    String val = String.valueOf(firmsCode);
                    if (key.isBlank() || val.isBlank()) return null;

                    return Map.entry(key, val);
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        Map.Entry::getValue,
                        (existing, replacement) -> existing,
                        LinkedHashMap::new
                ));
    }
}
