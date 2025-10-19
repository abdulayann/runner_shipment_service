package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
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
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@PropertySource("classpath:application-qa.properties")
@Execution(ExecutionMode.CONCURRENT)
class MDMServiceAdapterTest {
    @Mock
    private RestTemplate restTemplate;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ObjectMapper objectMapper;

    @InjectMocks
    private MDMServiceAdapter mdmServiceAdapter;

    @Mock
    private CacheManager cacheManager;

    @Mock
    private Cache cache;

    @Mock
    private CustomKeyGenerator keyGenerator;

    @Mock
    private MasterDataUtils masterDataUtils;

    private String baseUrl = "http://localhost:8080";
    private String creditDetailsUrl = "/credit-details";

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        mdmServiceAdapter = new MDMServiceAdapter(restTemplate, baseUrl);
        mdmServiceAdapter.jsonHelper = this.jsonHelper;
        mdmServiceAdapter.objectMapper = this.objectMapper;
        mdmServiceAdapter.cacheManager = this.cacheManager;
        mdmServiceAdapter.keyGenerator = this.keyGenerator;
        mdmServiceAdapter.masterDataUtils = this.masterDataUtils;
    }

    @Test
    void testGetCreditInfo_Success() throws RunnerException {
        ApprovalPartiesRequest approvalPartiesRequest = new ApprovalPartiesRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(approvalPartiesRequest).build();


        Map<String, Object> responseBody = Map.of("key", "value");
        ResponseEntity<Object> responseEntity = new ResponseEntity<>(responseBody, HttpStatus.OK);

        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.exchange(any(RequestEntity.class), eq(Object.class))).thenReturn(responseEntity);

        ResponseEntity<IRunnerResponse> result = mdmServiceAdapter.getCreditInfo(commonRequestModel);

        assertNotNull(result);
        assertEquals(HttpStatus.OK, result.getStatusCode());
    }

    @Test
    void testGetCreditInfo_Exception() {
        ApprovalPartiesRequest approvalPartiesRequest = new ApprovalPartiesRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(approvalPartiesRequest).build();


        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.exchange(any(RequestEntity.class), eq(Object.class))).thenThrow(new RuntimeException("Test Exception"));

        RunnerException exception = assertThrows(RunnerException.class, () -> {
            mdmServiceAdapter.getCreditInfo(commonRequestModel);
        });

        assertEquals("Error from MDM while fetching credit limit: Test Exception", exception.getMessage());
    }

    @Test
    void testGetApprovalStatusForParties_Success() throws RunnerException {
        ApprovalPartiesRequest approvalPartiesRequest = new ApprovalPartiesRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(approvalPartiesRequest).build();


        LinkedHashMap<String, Object> dataMap = new LinkedHashMap<>();
        dataMap.put("data", List.of(Map.of("finalStatus", "APPROVED")));

        DependentServiceResponse dependentServiceResponse = new DependentServiceResponse();
        dependentServiceResponse.setData(dataMap);

        ResponseEntity<Object> responseEntity = new ResponseEntity<>(dependentServiceResponse, HttpStatus.OK);

        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.exchange(any(RequestEntity.class), eq(Object.class))).thenReturn(responseEntity);
        when(objectMapper.convertValue(any(), eq(DependentServiceResponse.class))).thenReturn(dependentServiceResponse);

        String result = mdmServiceAdapter.getApprovalStausForParties(commonRequestModel);

        assertNotNull(result);
        assertEquals("APPROVED", result);
    }

    @Test
    void testGetApprovalStatusForParties_NullData() throws RunnerException {
        ApprovalPartiesRequest approvalPartiesRequest = new ApprovalPartiesRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(approvalPartiesRequest).build();


        LinkedHashMap<String, Object> dataMap = new LinkedHashMap<>();
        dataMap.put("data", null);

        DependentServiceResponse dependentServiceResponse = new DependentServiceResponse();
        dependentServiceResponse.setData(dataMap);

        ResponseEntity<Object> responseEntity = new ResponseEntity<>(dependentServiceResponse, HttpStatus.OK);

        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.exchange(any(RequestEntity.class), eq(Object.class))).thenReturn(responseEntity);
        when(objectMapper.convertValue(any(), eq(DependentServiceResponse.class))).thenReturn(dependentServiceResponse);

        String result = mdmServiceAdapter.getApprovalStausForParties(commonRequestModel);

        assertNull(result);
    }

    @Test
    void testGetApprovalStatusForParties_Exception() throws RunnerException {
        ApprovalPartiesRequest approvalPartiesRequest = new ApprovalPartiesRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(approvalPartiesRequest).build();


        LinkedHashMap<String, Object> dataMap = new LinkedHashMap<>();
        dataMap.put("data", List.of(Map.of("finalStatus", "APPROVED")));

        DependentServiceResponse dependentServiceResponse = new DependentServiceResponse();
        dependentServiceResponse.setData(dataMap);

        ResponseEntity<Object> responseEntity = new ResponseEntity<>(dependentServiceResponse, HttpStatus.OK);

        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.exchange(any(RequestEntity.class), eq(Object.class))).thenReturn(responseEntity);
        when(objectMapper.convertValue(any(), eq(DependentServiceResponse.class))).thenThrow(new RuntimeException("Test Exception"));

        String result = mdmServiceAdapter.getApprovalStausForParties(commonRequestModel);

        assertNull(result);
    }


    @Test
    void createShipmentTaskFromBooking_Success() throws Exception {
        // Arrange
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        CreateShipmentTaskFromBookingTaskRequest request = CreateShipmentTaskFromBookingTaskRequest.builder().build();
        when(commonRequestModel.getData()).thenReturn(request);
        String jsonRequest = "{}";
        when(jsonHelper.convertToJson(any())).thenReturn(jsonRequest);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().build();
        ResponseEntity<DependentServiceResponse> responseEntity = new ResponseEntity<>(dependentServiceResponse, HttpStatus.OK);
        when(restTemplate.exchange(any(RequestEntity.class), eq(DependentServiceResponse.class)))
                .thenReturn(responseEntity);
        IRunnerResponse runnerResponse = mock(IRunnerResponse.class);

        // Act
        ResponseEntity<IRunnerResponse> response = mdmServiceAdapter.createShipmentTaskFromBooking(commonRequestModel);

        // Assert
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(jsonHelper, times(2)).convertToJson(request);
        verify(restTemplate).exchange(any(RequestEntity.class), eq(DependentServiceResponse.class));
    }

    @Test
    void createShipmentTaskFromBooking_Exception() throws Exception {
        // Arrange
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        CreateShipmentTaskFromBookingTaskRequest request = CreateShipmentTaskFromBookingTaskRequest.builder().build();
        when(commonRequestModel.getData()).thenReturn(request);
        String jsonRequest = "{}";
        when(jsonHelper.convertToJson(any())).thenReturn(jsonRequest);
        when(restTemplate.exchange(any(RequestEntity.class), eq(DependentServiceResponse.class)))
                .thenThrow(new RuntimeException("MDM Service Error"));

        // Act & Assert
        mdmServiceAdapter.createShipmentTaskFromBooking(commonRequestModel);
        verify(jsonHelper,times(4)).convertToJson(request);
        verify(restTemplate,times(3)).exchange(any(RequestEntity.class), eq(DependentServiceResponse.class));
    }

    @Test
    void createNonBillableCustomer_Success() throws Exception {
        // Arrange
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        CompanyDetailsRequest request = new CompanyDetailsRequest();
        when(commonRequestModel.getDependentData()).thenReturn(request);
        String jsonRequest = "{}";
        when(jsonHelper.convertToJson(any())).thenReturn(jsonRequest);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().build();
        ResponseEntity<DependentServiceResponse> responseEntity = new ResponseEntity<>(dependentServiceResponse, HttpStatus.OK);
        when(restTemplate.exchange(any(RequestEntity.class), eq(DependentServiceResponse.class)))
            .thenReturn(responseEntity);

        // Act
        ResponseEntity<IRunnerResponse> response = mdmServiceAdapter.createNonBillableCustomer(commonRequestModel);

        // Assert
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void createNonBillableCustomer_Exception() throws Exception {
        // Arrange
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        CompanyDetailsRequest request = new CompanyDetailsRequest();
        when(commonRequestModel.getDependentData()).thenReturn(request);
        String jsonRequest = "{}";
        when(jsonHelper.convertToJson(any())).thenReturn(jsonRequest);
        when(restTemplate.exchange(any(RequestEntity.class), eq(DependentServiceResponse.class)))
            .thenThrow(new RuntimeException("MDM Service Error"));

        // Act & Assert
        mdmServiceAdapter.createNonBillableCustomer(commonRequestModel);
        verify(restTemplate,times(1)).exchange(any(RequestEntity.class), eq(DependentServiceResponse.class));
    }

    @Test
    void getDepartmentListReturnsListOfDepartments() throws Exception {
        String transportMode = "AIR";
        String shipmentType = "EXP";
        String module = "SHP";
        // Arrange
        MdmListCriteriaRequest mdmListCriteriaRequest = MdmListCriteriaRequest.builder().build();
        String jsonRequest = "{}";
        when(jsonHelper.convertToJson(any())).thenReturn(jsonRequest);
        when(restTemplate.postForEntity(anyString(), any(), any())).thenReturn(ResponseEntity.of(
                Optional.of(new DependentServiceResponse())
        ));
        ArgumentCaptor<TypeReference<List<Map<String, Object>>>> captor = ArgumentCaptor.forClass(TypeReference.class);
        when(jsonHelper.convertValue(any(), captor.capture())).thenReturn(new ArrayList<>());

        // Act & Assert
        var response = mdmServiceAdapter.getDepartmentList(transportMode, shipmentType, module);
        assertNotNull(response);
    }

    @Test
    void getDepartmentListReturnsEmptyListInCaseOfException() throws Exception {
        String transportMode = "AIR";
        String shipmentType = "EXP";
        String module = "SHP";
        // Arrange
        String jsonRequest = "{}";
        when(jsonHelper.convertToJson(any())).thenReturn(jsonRequest);
        when(restTemplate.postForEntity(anyString(), any(), any())).thenThrow(new RuntimeException("MDM Service Error"));

        // Act & Assert
        var response = mdmServiceAdapter.getDepartmentList(transportMode, shipmentType, module);
        assertEquals(0, response.size());
    }

    @Test
    void testCreateTask_Success() throws RunnerException {
        MdmTaskCreateRequest request = new MdmTaskCreateRequest();
        MdmTaskCreateResponse expectedResponse = new MdmTaskCreateResponse();
        DependentServiceResponse mockServiceResponse = new DependentServiceResponse();
        mockServiceResponse.setData(expectedResponse);

        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.postForEntity(anyString(), any(), any()))
                .thenReturn(ResponseEntity.ok(mockServiceResponse));
        when(jsonHelper.convertValue(mockServiceResponse.getData(), MdmTaskCreateResponse.class)).thenReturn(expectedResponse);
        com.dpw.runner.shipment.services.dto.response.mdm.MdmTaskCreateResponse actualResponse = mdmServiceAdapter.createTask(request);

       assertNotNull(actualResponse);
    }

    @Test
    void testCreateTask_Success1() throws RunnerException {
        MdmTaskCreateRequest request = new MdmTaskCreateRequest();
        MdmTaskCreateResponse expectedResponse = new MdmTaskCreateResponse();
        DependentServiceResponse mockServiceResponse = new DependentServiceResponse();
        mockServiceResponse.setData(expectedResponse);

        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.postForEntity(anyString(), any(), any()))
                .thenReturn(ResponseEntity.ok(mockServiceResponse));
        when(jsonHelper.convertValue(mockServiceResponse.getData(), MdmTaskCreateResponse.class)).thenReturn(expectedResponse);
        com.dpw.runner.shipment.services.dto.response.mdm.MdmTaskCreateResponse actualResponse = mdmServiceAdapter.createTask(request);

        assertNotNull(actualResponse);
    }

    @Test
    void testCreateTask_ExceptionThrown() {
        MdmTaskCreateRequest request = new MdmTaskCreateRequest();
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.postForEntity(anyString(), any(), any()))
                .thenThrow(new RuntimeException("MDM service down"));

        assertThrows(RunnerException.class, () -> {
            mdmServiceAdapter.createTask(request);
        });
    }

    @Test
    void testApproveOrRejectTask_Success() throws RunnerException {
        MdmTaskApproveOrRejectRequest request = new MdmTaskApproveOrRejectRequest();
        DependentServiceResponse mockServiceResponse = new DependentServiceResponse();

        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.exchange(any(RequestEntity.class), eq(DependentServiceResponse.class)))
                .thenReturn(ResponseEntity.ok(mockServiceResponse));

        assertDoesNotThrow(() -> mdmServiceAdapter.approveOrRejectTask(request));
    }

    @Test
    void testApproveOrRejectTask_ExceptionThrown() {
        MdmTaskApproveOrRejectRequest request = new MdmTaskApproveOrRejectRequest();
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.exchange(any(RequestEntity.class), eq(DependentServiceResponse.class)))
                .thenThrow(new RuntimeException("Approve failed"));

        RunnerException exception = assertThrows(RunnerException.class, () -> {
            mdmServiceAdapter.approveOrRejectTask(request);
        });

        assertTrue(exception.getMessage().contains("Approve failed"));
    }

    @Test
    void testGetTaskList_Success() {
        String entityUuid = "uuid-123";
        String entityType = "SHIPMENT";
        String status = "PENDING";
        String taskType = "DG_OCEAN_APPROVAL";

        // Prepare mock response
        List<Map<String, Object>> mockData = new ArrayList<>();
        Map<String, Object> task = new HashMap<>();
        task.put("uuid", "task-001");
        task.put("userEmail", "test@example.com");
        mockData.add(task);

        DependentServiceResponse response = new DependentServiceResponse();
        response.setData(mockData);

        ResponseEntity<DependentServiceResponse> responseEntity = ResponseEntity.ok(response);

        // Mocks
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.postForEntity(anyString(), any(), eq(DependentServiceResponse.class)))
                .thenReturn(responseEntity);
        when(jsonHelper.convertValue(eq(mockData), any(TypeReference.class))).thenReturn(mockData);

        // Call method
        List<Map<String, Object>> result = mdmServiceAdapter.getTaskList(entityUuid, entityType, status, taskType);

        // Assertions
        assertEquals(1, result.size());
        assertEquals("task-001", result.get(0).get("uuid"));
        assertEquals("test@example.com", result.get(0).get("userEmail"));
    }

    @Test
    void testGetTaskList_ExceptionThrown_ReturnsEmptyList() {
        String entityUuid = "uuid-123";
        String entityType = "SHIPMENT";
        String status = "PENDING";
        String taskType = "DG_OCEAN_APPROVAL";

        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.postForEntity(anyString(), any(), eq(DependentServiceResponse.class)))
                .thenThrow(new RuntimeException("MDM error"));

        List<Map<String, Object>> result = mdmServiceAdapter.getTaskList(entityUuid, entityType, status, taskType);

        assertTrue(result.isEmpty());
    }

    
    @Test
    void getTask_Failure(){
        when(restTemplate.getForEntity(any(), any())).thenThrow(new RuntimeException("ex"));
        assertThrows(RunnerException.class , () -> mdmServiceAdapter.getTask(null, 1L));
    }

    @Test
    void getFirmsCodeListFromCache_HandlesNullAndEmptyInput() {
        // Act & Assert for null input
        Map<String, String> resultNull = mdmServiceAdapter.getFirmsCodeListFromCache(null);
        assertTrue(resultNull.isEmpty(), "Should return an empty map for null input");

        // Act & Assert for empty input
        Map<String, String> resultEmpty = mdmServiceAdapter.getFirmsCodeListFromCache(Collections.emptySet());
        assertTrue(resultEmpty.isEmpty(), "Should return an empty map for empty set input");

        // Verify no external calls were made
        verifyNoInteractions(restTemplate, masterDataUtils);
    }

    @Test
    void getFirmsCodeListFromCache_ReturnsAllItemsFromCacheWhenAvailable() {
        // Arrange
        Set<String> orgIds = Set.of("ORG1", "ORG2");
        Cache.ValueWrapper wrapper1 = mock(Cache.ValueWrapper.class);
        when(wrapper1.get()).thenReturn("FIRMS1");
        Cache.ValueWrapper wrapper2 = mock(Cache.ValueWrapper.class);
        when(wrapper2.get()).thenReturn("FIRMS2");

        StringBuilder key1 = new StringBuilder("key_ORG1");
        StringBuilder key2 = new StringBuilder("key_ORG2");

        when(cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA)).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(CacheConstants.FIRMS_CODE, "ORG1")).thenReturn(key1);
        when(keyGenerator.customCacheKeyForMasterData(CacheConstants.FIRMS_CODE, "ORG2")).thenReturn(key2);
        when(cache.get(key1)).thenReturn(wrapper1);
        when(cache.get(key2)).thenReturn(wrapper2);

        // Act
        Map<String, String> result = mdmServiceAdapter.getFirmsCodeListFromCache(orgIds);

        // Assert
        assertEquals(2, result.size());
        assertEquals("FIRMS1", result.get("ORG1"));
        assertEquals("FIRMS2", result.get("ORG2"));

        // Verify no external calls were made as everything was in cache
        verifyNoInteractions(restTemplate, masterDataUtils);
    }

    @Test
    void getFirmsCodeListFromCache_HandlesMixedCacheHitsAndMdmLookups() {
        // Arrange: Setup a complex scenario with cache hits, misses, and various MDM data responses
        Set<String> orgIds = new HashSet<>(Arrays.asList("ORG_CACHED", "ORG_VALID_MDM", "ORG_MALFORMED_MDM", "ORG_MISSING_MDM", null, " "));

        StringBuilder keyCached = new StringBuilder("key_ORG_CACHED");
        StringBuilder keyValidMdm = new StringBuilder("key_ORG_VALID_MDM");
        StringBuilder keyMalformedMdm = new StringBuilder("key_ORG_MALFORMED_MDM");
        StringBuilder keyMissingMdm = new StringBuilder("key_ORG_MISSING_MDM");
        StringBuilder keyNull = new StringBuilder("key_null");
        StringBuilder keySpace = new StringBuilder("key_space");

        when(cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA)).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(CacheConstants.FIRMS_CODE, "ORG_CACHED")).thenReturn(keyCached);
        when(keyGenerator.customCacheKeyForMasterData(CacheConstants.FIRMS_CODE, "ORG_VALID_MDM")).thenReturn(keyValidMdm);
        when(keyGenerator.customCacheKeyForMasterData(CacheConstants.FIRMS_CODE, "ORG_MALFORMED_MDM")).thenReturn(keyMalformedMdm);
        when(keyGenerator.customCacheKeyForMasterData(CacheConstants.FIRMS_CODE, "ORG_MISSING_MDM")).thenReturn(keyMissingMdm);
        // Mock for null and space values that are in the input set

        // 1. Item in cache
        Cache.ValueWrapper cachedWrapper = mock(Cache.ValueWrapper.class);
        when(cachedWrapper.get()).thenReturn("FIRMS_CACHED");
        when(cache.get(keyCached)).thenReturn(cachedWrapper);

        // 2. Items not in cache (will trigger MDM call)
        when(cache.get(keyValidMdm)).thenReturn(null);
        when(cache.get(keyMalformedMdm)).thenReturn(null);
        when(cache.get(keyMissingMdm)).thenReturn(null);

        // 3. Mock MDM response with valid, malformed, and missing data
        Map<String, Object> malformedMap = new HashMap<>();
        malformedMap.put("entityId", "ORG_MALFORMED_MDM");
        malformedMap.put("govtIdNumber", null); // Malformed data

        List<Map<String, Object>> mdmResponseData = List.of(
                Map.of("entityId", "ORG_VALID_MDM", "govtIdNumber", "FIRMS_VALID_MDM"),
                malformedMap
        );
        DependentServiceResponse dependentServiceResponse = new DependentServiceResponse();
        dependentServiceResponse.setData(mdmResponseData);
        ResponseEntity<DependentServiceResponse> responseEntity = new ResponseEntity<>(dependentServiceResponse, HttpStatus.OK);

        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.postForEntity(anyString(), any(), eq(DependentServiceResponse.class))).thenReturn(responseEntity);
        when(jsonHelper.convertValue(eq(mdmResponseData), any(TypeReference.class))).thenReturn(mdmResponseData);

        // Act
        Map<String, String> result = mdmServiceAdapter.getFirmsCodeListFromCache(orgIds);

        // Assert: Final result should contain only valid data from both cache and MDM
        assertEquals(2, result.size());
        assertEquals("FIRMS_CACHED", result.get("ORG_CACHED"));
        assertEquals("FIRMS_VALID_MDM", result.get("ORG_VALID_MDM"));
        assertNull(result.get("ORG_MALFORMED_MDM"));
        assertNull(result.get("ORG_MISSING_MDM"));

        // Verify: MDM was called only for non-cached, valid orgIds
        ArgumentCaptor<String> jsonCaptor = ArgumentCaptor.forClass(String.class);
        verify(restTemplate, times(1)).postForEntity(anyString(), jsonCaptor.capture(), eq(DependentServiceResponse.class));
        // Ensure null/blank orgIds were filtered before the MDM call
        assertFalse(jsonCaptor.getValue().contains("null"));
        assertFalse(jsonCaptor.getValue().contains(" "));

        // Verify: Only new, valid data was pushed to cache
        ArgumentCaptor<Map<String, String>> mapCaptor = ArgumentCaptor.forClass(Map.class);
        ArgumentCaptor<Set<String>> setCaptor = ArgumentCaptor.forClass(Set.class);
        verify(masterDataUtils, times(1)).pushToCache(mapCaptor.capture(), anyString(), setCaptor.capture(), anyString(), isNull());
        Set<String> actualMdmOrgIds = setCaptor.getValue();
        // The set should contain all orgIds that were not found in cache (including null and space values)
        assertEquals(3, actualMdmOrgIds.size());
        assertTrue(actualMdmOrgIds.contains("ORG_VALID_MDM"));
        assertTrue(actualMdmOrgIds.contains("ORG_MALFORMED_MDM"));
        assertTrue(actualMdmOrgIds.contains("ORG_MISSING_MDM"));
        Map<String, String> pushedMap = mapCaptor.getValue();
        assertEquals(1, pushedMap.size());
        assertEquals("FIRMS_VALID_MDM", pushedMap.get("ORG_VALID_MDM"));
    }

    @Test
    void getFirmsCodeListFromCache_HandlesMdmApiFailureGracefully() {
        // Arrange
        Set<String> orgIds = Set.of("ORG_CACHED", "ORG_NEW");
        Cache.ValueWrapper cachedWrapper = mock(Cache.ValueWrapper.class);
        when(cachedWrapper.get()).thenReturn("FIRMS_CACHED");

        StringBuilder keyCached = new StringBuilder("key_ORG_CACHED");
        StringBuilder keyNew = new StringBuilder("key_ORG_NEW");

        when(cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA)).thenReturn(cache);
        when(keyGenerator.customCacheKeyForMasterData(CacheConstants.FIRMS_CODE, "ORG_CACHED")).thenReturn(keyCached);
        when(keyGenerator.customCacheKeyForMasterData(CacheConstants.FIRMS_CODE, "ORG_NEW")).thenReturn(keyNew);
        when(cache.get(keyCached)).thenReturn(cachedWrapper);
        when(cache.get(keyNew)).thenReturn(null);

        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.postForEntity(anyString(), any(), eq(DependentServiceResponse.class)))
                .thenThrow(new RuntimeException("MDM API is down"));
        // Act
        Map<String, String> result = mdmServiceAdapter.getFirmsCodeListFromCache(orgIds);

        // Assert:
        assertEquals(1, result.size());
        assertEquals("FIRMS_CACHED", result.get("ORG_CACHED"));
        assertNull(result.get("ORG_NEW"));

        // Verify: An empty map is pushed to cache even when the API call fails
        ArgumentCaptor<Map<String, String>> mapCaptor = ArgumentCaptor.forClass(Map.class);
        verify(masterDataUtils, times(1)).pushToCache(mapCaptor.capture(), anyString(), any(), anyString(), isNull());
        Map<String, String> pushedMap = mapCaptor.getValue();
        assertTrue(pushedMap.isEmpty(), "Should push an empty map when API call fails");
    }

    @Test
    void getFirmsCodeListFromCache_HandlesCacheUnavailable() {
        // Arrange
        Set<String> orgIds = Set.of("ORG1", "ORG2");

        // Mock cache manager to return null (cache unavailable)
        when(cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA)).thenReturn(null);

        // Mock MDM response
        Map<String, Object> org1Map = new HashMap<>();
        org1Map.put("entityId", "ORG1");
        org1Map.put("govtIdNumber", "FIRMS1");

        Map<String, Object> org2Map = new HashMap<>();
        org2Map.put("entityId", "ORG2");
        org2Map.put("govtIdNumber", "FIRMS2");

        List<Map<String, Object>> mdmResponse = List.of(org1Map, org2Map);

        DependentServiceResponse dependentServiceResponse = new DependentServiceResponse();
        dependentServiceResponse.setData(mdmResponse);

        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.postForEntity(anyString(), any(), eq(DependentServiceResponse.class)))
                .thenReturn(ResponseEntity.ok(dependentServiceResponse));
        when(jsonHelper.convertValue(eq(mdmResponse), any(TypeReference.class))).thenReturn(mdmResponse);

        // Act
        Map<String, String> result = mdmServiceAdapter.getFirmsCodeListFromCache(orgIds);

        // Assert
        assertEquals(2, result.size());
        assertEquals("FIRMS1", result.get("ORG1"));
        assertEquals("FIRMS2", result.get("ORG2"));

        // Verify that keyGenerator and cache.get were never called since cache is null
        verify(keyGenerator, never()).customCacheKeyForMasterData(any(), any());
        verify(cache, never()).get(any());

        // Verify that MDM API was called directly
        verify(restTemplate, times(1)).postForEntity(anyString(), any(), eq(DependentServiceResponse.class));

        // Verify that pushToCache was never called since cache is unavailable
        verify(masterDataUtils, never()).pushToCache(any(), any(), any(), any(), any());
    }
    @Test
    void testGetContainerTypes_Success() throws RunnerException {
        // Arrange
        DependentServiceResponse mockResponse = new DependentServiceResponse();
        mockResponse.setData(List.of(Map.of("code", "20GP", "description", "20ft General Purpose")));
        ResponseEntity<DependentServiceResponse> responseEntity = ResponseEntity.ok(mockResponse);

        when(jsonHelper.convertToJson(any(MdmListCriteriaRequest.class))).thenReturn("{}");
        when(restTemplate.postForEntity(anyString(), any(), eq(DependentServiceResponse.class)))
                .thenReturn(responseEntity);

        // Act
        DependentServiceResponse result = mdmServiceAdapter.getContainerTypes();

        // Assert
        assertNotNull(result);
        assertEquals(mockResponse, result);
        verify(restTemplate, times(1)).postForEntity(anyString(), any(), eq(DependentServiceResponse.class));
    }

    @Test
    void testGetContainerTypes_Success_WhenBodyIsNull() throws RunnerException {
        // Arrange
        ResponseEntity<DependentServiceResponse> responseEntity = ResponseEntity.ok(null);

        when(jsonHelper.convertToJson(any(MdmListCriteriaRequest.class))).thenReturn("{}");
        when(restTemplate.postForEntity(anyString(), any(), eq(DependentServiceResponse.class)))
                .thenReturn(responseEntity);

        // Act
        DependentServiceResponse result = mdmServiceAdapter.getContainerTypes();

        // Assert
        assertNotNull(result);
        assertNull(result.getData()); // A new empty response should be returned
        verify(restTemplate, times(1)).postForEntity(anyString(), any(), eq(DependentServiceResponse.class));
    }

    @Test
    void testGetContainerTypes_Failure_ThrowsRunnerException() {
        // Arrange
        String errorMessage = "MDM service is down";
        when(jsonHelper.convertToJson(any(MdmListCriteriaRequest.class))).thenReturn("{}");
        when(restTemplate.postForEntity(anyString(), any(), eq(DependentServiceResponse.class)))
                .thenThrow(new RuntimeException(errorMessage));

        // Act & Assert
        RunnerException exception = assertThrows(RunnerException.class, () -> {
            mdmServiceAdapter.getContainerTypes();
        });

        assertTrue(exception.getMessage().contains("Error while fetching container type list"));
        assertTrue(exception.getMessage().contains(errorMessage));
    }

    @Test
    void getTask_whenResponseDataIsList_returnsFirstElementConverted() throws Exception {
        // given
        String taskUuid = "task-uuid";
        Long id = 11L;

        DependentServiceResponse depResp = mock(DependentServiceResponse.class);
        // simulate the body data being a non-empty list
        Object firstElement = Map.of("someKey", "someValue");
        List<Object> dataList = List.of(firstElement);
        when(depResp.getData()).thenReturn(dataList);

        ResponseEntity<DependentServiceResponse> responseEntity = ResponseEntity.ok(depResp);
        when(restTemplate.getForEntity(anyString(), eq(DependentServiceResponse.class))).thenReturn(responseEntity);

        MDMTaskRetrieveResponse converted = new MDMTaskRetrieveResponse();
        when(jsonHelper.convertValue(firstElement, MDMTaskRetrieveResponse.class)).thenReturn(converted);

        // when
        MDMTaskRetrieveResponse result = mdmServiceAdapter.getTask(taskUuid, id);

        // then
        assertNotNull(result);
        assertSame(converted, result);
        verify(restTemplate).getForEntity(contains("uuid="), eq(DependentServiceResponse.class));
        verify(jsonHelper).convertValue(firstElement, MDMTaskRetrieveResponse.class);
    }

    @Test
    void getTask_whenResponseDataIsSingleObject_convertsAndReturnsIt() throws Exception {
        // given
        String taskUuid = ""; // empty -> method will use id param path
        Long id = 22L;

        DependentServiceResponse depResp = mock(DependentServiceResponse.class);
        // simulate the body data being a single object (not a list)
        Object bodyData = Map.of("field", "value");
        when(depResp.getData()).thenReturn(bodyData);

        ResponseEntity<DependentServiceResponse> responseEntity = ResponseEntity.ok(depResp);
        when(restTemplate.getForEntity(anyString(), eq(DependentServiceResponse.class))).thenReturn(responseEntity);

        MDMTaskRetrieveResponse converted = new MDMTaskRetrieveResponse();
        when(jsonHelper.convertValue(bodyData, MDMTaskRetrieveResponse.class)).thenReturn(converted);

        // when
        MDMTaskRetrieveResponse result = mdmServiceAdapter.getTask(taskUuid, id);

        // then
        assertNotNull(result);
        assertSame(converted, result);
        // because taskUuid is empty your implementation will call URL with "?id="
        verify(restTemplate).getForEntity(contains("?id="), eq(DependentServiceResponse.class));
        verify(jsonHelper).convertValue(bodyData, MDMTaskRetrieveResponse.class);
    }

}