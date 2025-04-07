package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.LicenseRequest;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.LicenseResponse;
import com.dpw.runner.shipment.services.dto.request.mdm.MdmListCriteriaRequest;
import com.dpw.runner.shipment.services.dto.v1.request.ApprovalPartiesRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CompanyDetailsRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CreateShipmentTaskFromBookingTaskRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.net.URI;
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
    void testValidateLicense_Success() throws RunnerException {
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        LicenseRequest licenseRequest = new LicenseRequest();
        when(commonRequestModel.getDependentData()).thenReturn(licenseRequest);

        when(jsonHelper.convertValueWithJsonNullable(any(), eq(LicenseRequest.class))).thenReturn(licenseRequest);
        when(jsonHelper.convertToJson(licenseRequest)).thenReturn("{} ");
        LicenseResponse licenseResponse = new LicenseResponse();

        ResponseEntity<DependentServiceResponse> dependentResponse = new ResponseEntity<>(new DependentServiceResponse(), HttpStatus.OK);
        when(restTemplate.exchange(any(), eq(DependentServiceResponse.class))).thenReturn(dependentResponse);

        ResponseEntity<LicenseResponse> responseEntity = new ResponseEntity<>(licenseResponse, HttpStatus.OK);

        LicenseResponse response = mdmServiceAdapter.validateLicense(commonRequestModel);

        assertNotNull(response);
    }


    @Test
    void testValidateLicense_Exception() throws RunnerException {
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        LicenseRequest licenseRequest = new LicenseRequest();
        when(commonRequestModel.getDependentData()).thenReturn(licenseRequest);

        when(jsonHelper.convertValueWithJsonNullable(any(), eq(LicenseRequest.class))).thenReturn(
            licenseRequest);
        when(jsonHelper.convertToJson(licenseRequest)).thenReturn("{} ");
        LicenseResponse licenseResponse = new LicenseResponse();
        ResponseEntity<LicenseResponse> responseEntity = new ResponseEntity<>(licenseResponse,
            HttpStatus.OK);
        when(restTemplate.exchange(any(RequestEntity.class), eq(LicenseResponse.class)))
            .thenThrow(new RuntimeException());

        LicenseResponse response = mdmServiceAdapter.validateLicense(
            commonRequestModel);

        assertNotNull(response);
    }

}