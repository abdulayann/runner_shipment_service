package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.mdm.MdmListCriteriaRequest;
import com.dpw.runner.shipment.services.dto.request.mdm.MdmTaskApproveOrRejectRequest;
import com.dpw.runner.shipment.services.dto.request.mdm.MdmTaskCreateRequest;
import com.dpw.runner.shipment.services.dto.request.mdm.MdmTaskCreateResponse;
import com.dpw.runner.shipment.services.dto.v1.request.ApprovalPartiesRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CompanyDetailsRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CreateShipmentTaskFromBookingTaskRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
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

    private String baseUrl = "http://localhost:8080";
    private String creditDetailsUrl = "/credit-details";

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        mdmServiceAdapter = new MDMServiceAdapter(restTemplate, baseUrl);
        mdmServiceAdapter.jsonHelper = this.jsonHelper;
        mdmServiceAdapter.objectMapper = this.objectMapper;
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
        MdmTaskCreateResponse actualResponse = mdmServiceAdapter.createTask(request);

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
        MdmTaskCreateResponse actualResponse = mdmServiceAdapter.createTask(request);

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

}