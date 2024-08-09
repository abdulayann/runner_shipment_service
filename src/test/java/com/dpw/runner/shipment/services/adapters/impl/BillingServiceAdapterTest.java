package com.dpw.runner.shipment.services.adapters.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ChargeTypeFilterRequest;
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

    @BeforeEach
    void setUp() {
        MockitoAnnotations.initMocks(this);
        lastPostedInvoiceDateRequest = new LastPostedInvoiceDateRequest();
        chargeTypeFilterRequest = new ChargeTypeFilterRequest();
    }

    @Test
    void fetchChargeTypes_Success() {
        String url = "http://example.com";
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(url);
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
        String url = "http://example.com";
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(url);
        when(billingServiceUrlConfig.getChargeTypeFilter()).thenReturn("/charge-type-filter");

        ResponseEntity<BillingListResponse<ChargeTypeBaseResponse>> responseEntity = ResponseEntity.ok(null);
        when(restTemplate.exchange(any(String.class), any(HttpMethod.class), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        assertThrows(BillingException.class, () -> billingServiceAdapter.fetchChargeTypes(chargeTypeFilterRequest));
    }

    @Test
    void fetchChargeTypes_NullData() {
        String url = "http://example.com";
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(url);
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
        String url = "http://example.com";
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(url);
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
        String url = "http://example.com";
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(url);
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
        String url = "http://example.com";
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(url);
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
        String url = "http://example.com";
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(url);
        when(billingServiceUrlConfig.getLastPostedInvoiceDate()).thenReturn("/last-posted-invoice-date");

        ResponseEntity<BillingEntityResponse> responseEntity = ResponseEntity.ok(null);
        when(restTemplate.exchange(any(String.class), any(HttpMethod.class), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        assertThrows(BillingException.class, () -> billingServiceAdapter.fetchLastPostedInvoiceDate(lastPostedInvoiceDateRequest));
    }

    @Test
    void fetchLastPostedInvoiceDate_NullData() {
        String url = "http://example.com";
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(url);
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
        String url = "http://example.com";
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(url);
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
        String url = "http://example.com";
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(url);
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
