package com.dpw.runner.shipment.services.adapters.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillingBulkSummaryRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ChargeTypeFilterRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ExternalBillPayloadRequest;
import com.dpw.runner.shipment.services.dto.request.billing.LastPostedInvoiceDateRequest;
import com.dpw.runner.shipment.services.dto.response.billing.BillingEntityResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingListResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummary;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummaryResponse;
import com.dpw.runner.shipment.services.dto.response.billing.ChargeTypeBaseResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.billing.BillingException;
import java.lang.reflect.Type;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class BillingServiceAdapterTest {

    @Mock
    private RestTemplate restTemplate;

    @Mock
    private BillingServiceUrlConfig billingServiceUrlConfig;

    @InjectMocks
    private BillingServiceAdapter billingServiceAdapter;

    @Mock
    private ModelMapper modelMapper;

    private LastPostedInvoiceDateRequest lastPostedInvoiceDateRequest;
    private ChargeTypeFilterRequest chargeTypeFilterRequest;
    private BillingBulkSummaryRequest billingBulkSummaryRequest;
    private ExternalBillPayloadRequest externalBillPayloadRequest;
    private BillingEntityResponse billingEntityResponse;
    private String baseUrl;
    private String createOrUpdateEndpoint;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.initMocks(this);
        lastPostedInvoiceDateRequest = new LastPostedInvoiceDateRequest();
        chargeTypeFilterRequest = new ChargeTypeFilterRequest();
        billingBulkSummaryRequest = new BillingBulkSummaryRequest();
        externalBillPayloadRequest = new ExternalBillPayloadRequest();
        billingEntityResponse = new BillingEntityResponse();
        baseUrl = "http://mockurl.com";
    }

    @Test
    void sendBillCreationRequest_Success() {

        createOrUpdateEndpoint = "/createOrUpdate";

        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getExternalCreateOrUpdate()).thenReturn(createOrUpdateEndpoint);
        ResponseEntity<BillingEntityResponse> mockResponse = new ResponseEntity<>(billingEntityResponse, HttpStatus.OK);

        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(BillingEntityResponse.class)))
                .thenReturn(mockResponse);

        ResponseEntity<BillingEntityResponse> response = billingServiceAdapter.sendBillCreationRequest(externalBillPayloadRequest);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(billingEntityResponse, response.getBody());
        verify(restTemplate, times(1)).postForEntity(anyString(), any(HttpEntity.class), eq(BillingEntityResponse.class));
    }

    @Test
    void sendBillCreationRequest_ResponseWithErrors() {
        createOrUpdateEndpoint = "/createOrUpdate";

        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getExternalCreateOrUpdate()).thenReturn(createOrUpdateEndpoint);

        billingEntityResponse.setErrors(List.of("Error 1", "Error 2"));
        ResponseEntity<BillingEntityResponse> mockResponseEntity = new ResponseEntity<>(billingEntityResponse, HttpStatus.OK);
        when(restTemplate.postForEntity(eq(baseUrl + "/createOrUpdate"), any(HttpEntity.class), eq(BillingEntityResponse.class)))
                .thenReturn(mockResponseEntity);

        assertThrows(BillingException.class, () -> billingServiceAdapter.sendBillCreationRequest(externalBillPayloadRequest));
    }

    @Test
    void sendBillCreationRequest_Exception() {
        createOrUpdateEndpoint = "/createOrUpdate";

        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getExternalCreateOrUpdate()).thenReturn(createOrUpdateEndpoint);
        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(BillingEntityResponse.class)))
                .thenThrow(new RuntimeException("Error occurred"));

        BillingException exception = assertThrows(BillingException.class, () -> {
            billingServiceAdapter.sendBillCreationRequest(externalBillPayloadRequest);
        });

        assertEquals("Error occurred", exception.getMessage());
        verify(restTemplate, times(1)).postForEntity(anyString(), any(HttpEntity.class), eq(BillingEntityResponse.class));
    }


    @Test
    void fetchBillingBulkSummary_Success() {
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getBillingBulkSummary()).thenReturn("/billing-bulk-summary");

        BillingEntityResponse billingEntityResponse = new BillingEntityResponse();
        BillingSummary billingSummary = new BillingSummary();
        billingEntityResponse.setData(Map.of("billingSummary", List.of(Map.of("totalCount", 1, "totalRevenue", 100.0))));

        ResponseEntity<BillingEntityResponse> responseEntity = ResponseEntity.ok(billingEntityResponse);
        when(restTemplate.postForEntity(any(String.class), any(HttpEntity.class), any(Class.class)))
                .thenReturn(responseEntity);

        List<BillingSummary> billingSummaries = List.of(billingSummary);
        when(modelMapper.map(anyList(), any(Type.class)))
                .thenReturn(billingSummaries);

        List<BillingSummary> result = billingServiceAdapter.fetchBillingBulkSummary(billingBulkSummaryRequest);
        assertEquals(billingSummaries, result);
    }

    @Test
    void fetchBillingBulkSummary_NullResponse() {
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getBillingBulkSummary()).thenReturn("/billing-bulk-summary");

        ResponseEntity<BillingEntityResponse> responseEntity = ResponseEntity.ok(null);
        when(restTemplate.postForEntity(any(String.class), any(HttpEntity.class), any(Class.class)))
                .thenReturn(responseEntity);

        List<BillingSummary> result = billingServiceAdapter.fetchBillingBulkSummary(billingBulkSummaryRequest);
        assertTrue(result.isEmpty());
    }

    @Test
    void fetchBillingBulkSummary_NullData() {
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getBillingBulkSummary()).thenReturn("/billing-bulk-summary");

        BillingEntityResponse billingEntityResponse = new BillingEntityResponse();
        billingEntityResponse.setData(null);

        ResponseEntity<BillingEntityResponse> responseEntity = ResponseEntity.ok(billingEntityResponse);
        when(restTemplate.postForEntity(any(String.class), any(HttpEntity.class), any(Class.class)))
                .thenReturn(responseEntity);

        List<BillingSummary> result = billingServiceAdapter.fetchBillingBulkSummary(billingBulkSummaryRequest);
        assertTrue(result.isEmpty());
    }

    @Test
    void fetchBillingBulkSummary_EmptyData() {
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getBillingBulkSummary()).thenReturn("/billing-bulk-summary");

        BillingEntityResponse billingEntityResponse = new BillingEntityResponse();
        billingEntityResponse.setData(Map.of("billingSummary", Collections.emptyList()));

        ResponseEntity<BillingEntityResponse> responseEntity = ResponseEntity.ok(billingEntityResponse);
        when(restTemplate.postForEntity(any(String.class), any(HttpEntity.class), any(Class.class)))
                .thenReturn(responseEntity);

        List<BillingSummary> result = billingServiceAdapter.fetchBillingBulkSummary(billingBulkSummaryRequest);
        assertTrue(result.isEmpty());
    }

    @Test
    void fetchBillingBulkSummary_ResponseContainsErrors() {
        String url = "http://example.com";
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(url);
        when(billingServiceUrlConfig.getBillingBulkSummary()).thenReturn("/billing-bulk-summary");

        BillingEntityResponse billingEntityResponse = new BillingEntityResponse();
        billingEntityResponse.setErrors(List.of("Some error"));

        ResponseEntity<BillingEntityResponse> responseEntity = ResponseEntity.ok(billingEntityResponse);
        when(restTemplate.postForEntity(any(String.class), any(HttpEntity.class), any(Class.class)))
                .thenReturn(responseEntity);

        assertThrows(BillingException.class, () -> billingServiceAdapter.fetchBillingBulkSummary(billingBulkSummaryRequest));
    }

    @Test
    void fetchBillingBulkSummary_ExceptionThrown() {
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getBillingBulkSummary()).thenReturn("/billing-bulk-summary");

        when(restTemplate.postForEntity(any(String.class), any(HttpEntity.class), any(Class.class)))
                .thenThrow(new RuntimeException("Runtime exception"));

        assertThrows(BillingException.class, () -> billingServiceAdapter.fetchBillingBulkSummary(billingBulkSummaryRequest));
    }

    @Test
    void fetchChargeTypes_Success() {
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getChargeTypeFilter()).thenReturn("/charge-type-filter");

        BillingListResponse<ChargeTypeBaseResponse> billingListResponse = new BillingListResponse<>();
        ChargeTypeBaseResponse chargeTypeBaseResponse = new ChargeTypeBaseResponse();
        billingListResponse.setData(List.of(chargeTypeBaseResponse));

        ResponseEntity<BillingListResponse<ChargeTypeBaseResponse>> responseEntity = ResponseEntity.ok(billingListResponse);
        when(restTemplate.exchange(any(String.class), any(HttpMethod.class), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        List<ChargeTypeBaseResponse> chargeTypeList = List.of(chargeTypeBaseResponse);
        Type listType = new TypeToken<List<ChargeTypeBaseResponse>>() {
        }.getType();
        when(modelMapper.map(billingListResponse.getData(), listType)).thenReturn(chargeTypeList);

        List<ChargeTypeBaseResponse> result = billingServiceAdapter.fetchChargeTypes(chargeTypeFilterRequest);
        assertEquals(chargeTypeList, result);
    }

    @Test
    void fetchChargeTypes_NullResponse() {
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getChargeTypeFilter()).thenReturn("/charge-type-filter");

        ResponseEntity<BillingListResponse<ChargeTypeBaseResponse>> responseEntity = ResponseEntity.ok(null);
        when(restTemplate.exchange(any(String.class), any(HttpMethod.class), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        assertThrows(BillingException.class, () -> billingServiceAdapter.fetchChargeTypes(chargeTypeFilterRequest));
    }

    @Test
    void fetchChargeTypes_NullData() {
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getChargeTypeFilter()).thenReturn("/charge-type-filter");

        BillingListResponse<ChargeTypeBaseResponse> billingListResponse = new BillingListResponse<>();
        billingListResponse.setData(null);

        ResponseEntity<BillingListResponse<ChargeTypeBaseResponse>> responseEntity = ResponseEntity.ok(billingListResponse);
        when(restTemplate.exchange(any(String.class), any(HttpMethod.class), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        assertThrows(BillingException.class, () -> billingServiceAdapter.fetchChargeTypes(chargeTypeFilterRequest));
    }

    @Test
    void fetchChargeTypes_EmptyData() {
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getChargeTypeFilter()).thenReturn("/charge-type-filter");

        BillingListResponse<ChargeTypeBaseResponse> billingListResponse = new BillingListResponse<>();
        billingListResponse.setData(List.of());

        ResponseEntity<BillingListResponse<ChargeTypeBaseResponse>> responseEntity = ResponseEntity.ok(billingListResponse);
        when(restTemplate.exchange(any(String.class), any(HttpMethod.class), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        assertThrows(BillingException.class, () -> billingServiceAdapter.fetchChargeTypes(chargeTypeFilterRequest));
    }

    @Test
    void fetchChargeTypes_ResponseContainsErrors() {
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getChargeTypeFilter()).thenReturn("/charge-type-filter");

        BillingListResponse<ChargeTypeBaseResponse> billingListResponse = new BillingListResponse<>();
        billingListResponse.setErrors(List.of("Some error"));

        ResponseEntity<BillingListResponse<ChargeTypeBaseResponse>> responseEntity = ResponseEntity.ok(billingListResponse);
        when(restTemplate.exchange(any(String.class), any(HttpMethod.class), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        assertThrows(BillingException.class, () -> billingServiceAdapter.fetchChargeTypes(chargeTypeFilterRequest));
    }

    @Test
    void fetchLastPostedInvoiceDate_Success() {
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getLastPostedInvoiceDate()).thenReturn("/last-posted-invoice-date");

        Map<String, Object> data = new HashMap<>();
        data.put("lastPostedInvoiceDate", "2024-07-26 15:40:45");
        BillingEntityResponse billingEntityResponse = new BillingEntityResponse();
        billingEntityResponse.setData(data);

        ResponseEntity<BillingEntityResponse> responseEntity = ResponseEntity.ok(billingEntityResponse);
        when(restTemplate.exchange(any(String.class), any(HttpMethod.class), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        LocalDateTime result = billingServiceAdapter.fetchLastPostedInvoiceDate(lastPostedInvoiceDateRequest);
        assertEquals(LocalDateTime.parse("2024-07-26 15:40:45", DateTimeFormatter.ofPattern(Constants.DATE_TIME_FORMAT)), result);
    }

    @Test
    void fetchLastPostedInvoiceDate_NullResponse() {
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getLastPostedInvoiceDate()).thenReturn("/last-posted-invoice-date");

        ResponseEntity<BillingEntityResponse> responseEntity = ResponseEntity.ok(null);
        when(restTemplate.exchange(any(String.class), any(HttpMethod.class), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        assertThrows(BillingException.class, () -> billingServiceAdapter.fetchLastPostedInvoiceDate(lastPostedInvoiceDateRequest));
    }

    @Test
    void fetchLastPostedInvoiceDate_NullData() {
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getLastPostedInvoiceDate()).thenReturn("/last-posted-invoice-date");

        BillingEntityResponse billingEntityResponse = new BillingEntityResponse();
        billingEntityResponse.setData(null);

        ResponseEntity<BillingEntityResponse> responseEntity = ResponseEntity.ok(billingEntityResponse);
        when(restTemplate.exchange(any(String.class), any(HttpMethod.class), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        assertThrows(BillingException.class, () -> billingServiceAdapter.fetchLastPostedInvoiceDate(lastPostedInvoiceDateRequest));
    }

    @Test
    void fetchLastPostedInvoiceDate_NullLastPostedInvoiceDate() {
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getLastPostedInvoiceDate()).thenReturn("/last-posted-invoice-date");

        Map<String, Object> data = new HashMap<>();
        data.put("lastPostedInvoiceDate", null);
        BillingEntityResponse billingEntityResponse = new BillingEntityResponse();
        billingEntityResponse.setData(data);

        ResponseEntity<BillingEntityResponse> responseEntity = ResponseEntity.ok(billingEntityResponse);
        when(restTemplate.exchange(any(String.class), any(HttpMethod.class), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        assertThrows(BillingException.class, () -> billingServiceAdapter.fetchLastPostedInvoiceDate(lastPostedInvoiceDateRequest));
    }

    @Test
    void fetchLastPostedInvoiceDate_ResponseContainsErrors() {
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getLastPostedInvoiceDate()).thenReturn("/last-posted-invoice-date");

        BillingEntityResponse billingEntityResponse = new BillingEntityResponse();
        billingEntityResponse.setErrors(List.of("Some error"));

        ResponseEntity<BillingEntityResponse> responseEntity = ResponseEntity.ok(billingEntityResponse);
        when(restTemplate.exchange(any(String.class), any(HttpMethod.class), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        assertThrows(BillingException.class, () -> billingServiceAdapter.fetchLastPostedInvoiceDate(lastPostedInvoiceDateRequest));
    }


    @Test
    void fetchActiveInvoicesTestDouble() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06").build();

        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
         (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().totalRevenue(0.001).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(commonGetRequest));
    }

    @Test
    void fetchActiveInvoicesTestInteger() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06").build();

        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().totalCount(1).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(commonGetRequest));
    }

    @Test
    void fetchActiveInvoicesTestNull() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06").build();

        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().build());

        assertEquals(false, billingServiceAdapter.fetchActiveInvoices(commonGetRequest));
    }

    @Test
    void testCheckActiveChargesWithTotalCount() throws RunnerException {
        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().totalCount(1).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(CommonGetRequest.builder().guid("guid").build()));
    }

    @Test
    void testCheckActiveChargesWithTotalRevenue() throws RunnerException {
        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().totalRevenue(0.01).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(CommonGetRequest.builder().guid("guid").build()));
    }

    @Test
    void testCheckActiveChargesWithTotalCost() throws RunnerException {
        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().totalCost(0.01).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(CommonGetRequest.builder().guid("guid").build()));
    }

    @Test
    void testCheckActiveChargesWithAccruedRevenue() throws RunnerException {
        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().accruedRevenue(0.01).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(CommonGetRequest.builder().guid("guid").build()));
    }

    @Test
    void testCheckActiveChargesWithAccruedCost() throws RunnerException {
        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().accruedCost(0.01).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(CommonGetRequest.builder().guid("guid").build()));
    }

    @Test
    void testCheckActiveChargesWithInvoicedRevenue() throws RunnerException {
        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().invoicedRevenue(0.01).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(CommonGetRequest.builder().guid("guid").build()));
    }

    @Test
    void testCheckActiveChargesWithInvoicedCost() throws RunnerException {
        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().invoicedCost(0.01).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(CommonGetRequest.builder().guid("guid").build()));
    }

    @Test
    void testCheckActiveChargesWithDisbursementAccruedRevenue() throws RunnerException {
        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().disbursementAccruedRevenue(0.01).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(CommonGetRequest.builder().guid("guid").build()));
    }

    @Test
    void testCheckActiveChargesWithDisbursementAccruedCost() throws RunnerException {
        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().disbursementAccruedCost(0.01).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(CommonGetRequest.builder().guid("guid").build()));
    }

    @Test
    void testCheckActiveChargesWithDisbursementInvoicedRevenue() throws RunnerException {
        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().disbursementInvoicedRevenue(0.01).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(CommonGetRequest.builder().guid("guid").build()));
    }

    @Test
    void testCheckActiveChargesWithDisbursementInvoicedCost() throws RunnerException {
        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().disbursementInvoicedCost(0.01).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(CommonGetRequest.builder().guid("guid").build()));
    }

    @Test
    void testCheckActiveChargesWithDisbursementRevenue() throws RunnerException {
        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().disbursementRevenue(0.01).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(CommonGetRequest.builder().guid("guid").build()));
    }

    @Test
    void testCheckActiveChargesWithDisbursementCost() throws RunnerException {
        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().disbursementCost(0.01).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(CommonGetRequest.builder().guid("guid").build()));
    }

    @Test
    void testCheckActiveChargesWithCumulativeGP() throws RunnerException{
        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().cumulativeGP(0.01).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(CommonGetRequest.builder().guid("guid").build()));
    }

    @Test
    void testCheckActiveChargesWithCumulativeGPPercentage() throws RunnerException {
        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().cumulativeGPPercentage(0.01).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(CommonGetRequest.builder().guid("guid").build()));
    }

}
