package com.dpw.runner.shipment.services.adapters.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.reportingservice.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.request.CreateBookingModuleInV1;
import com.dpw.runner.shipment.services.dto.request.CreateBookingModuleInV1.BookingEntity;
import com.dpw.runner.shipment.services.dto.request.CreateBookingModuleInV1.BookingEntity.BillCharge;
import com.dpw.runner.shipment.services.dto.request.billing.BillChargesFilterRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillRetrieveRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillingBulkSummaryBranchWiseRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillingBulkSummaryRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ChargeTypeFilterRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ExternalBillPayloadRequest;
import com.dpw.runner.shipment.services.dto.request.billing.LastPostedInvoiceDateRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingDueSummary;
import com.dpw.runner.shipment.services.dto.response.billing.BillingEntityResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingListResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummary;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummaryResponse;
import com.dpw.runner.shipment.services.dto.response.billing.ChargeTypeBaseResponse;
import com.dpw.runner.shipment.services.dto.v1.request.ShipmentBillingListRequest;
import com.dpw.runner.shipment.services.dto.v1.response.ShipmentBillingListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.ShipmentBillingListResponse.BillingData;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.enums.MeasurementBasis;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.billing.BillingException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import com.dpw.runner.shipment.services.utils.V2AuthHelper;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
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
    private V2AuthHelper v2AuthHelper;
    @Mock
    private BillingServiceUrlConfig billingServiceUrlConfig;
    @InjectMocks
    private BillingServiceAdapter billingServiceAdapter;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IV1Service v1Service;
    @Mock
    private V1ServiceUtil v1ServiceUtil;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private V1AuthHelper v1AuthHelper;

    private LastPostedInvoiceDateRequest lastPostedInvoiceDateRequest;
    private ChargeTypeFilterRequest chargeTypeFilterRequest;
    private BillingBulkSummaryRequest billingBulkSummaryRequest;
    private BillingBulkSummaryBranchWiseRequest billingBulkSummaryBranchWiseRequest;
    private ExternalBillPayloadRequest externalBillPayloadRequest;
    private BillingEntityResponse billingEntityResponse;
    private String baseUrl;
    private String createOrUpdateEndpoint;
    private BillChargesFilterRequest billChargesFilterRequest;
    private BillingListResponse<BillChargesBaseResponse> billChargesBaseResponseBillingListResponse;
    private BillRetrieveRequest billRetrieveRequest;
    private BillBaseResponse billBaseResponse;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.initMocks(this);
        lastPostedInvoiceDateRequest = new LastPostedInvoiceDateRequest();
        chargeTypeFilterRequest = new ChargeTypeFilterRequest();
        billingBulkSummaryRequest = new BillingBulkSummaryRequest();
        billingBulkSummaryBranchWiseRequest = new BillingBulkSummaryBranchWiseRequest();
        externalBillPayloadRequest = new ExternalBillPayloadRequest();
        billingEntityResponse = new BillingEntityResponse();
        baseUrl = "http://mockurl.com";
        billChargesFilterRequest = new BillChargesFilterRequest();
        billChargesBaseResponseBillingListResponse = new BillingListResponse<>();
    }

    @Test
    void testFetchBillingBulkSummaryBranchWise_SuccessfulResponse() {
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getBillingBulkSummaryBranchWise()).thenReturn("/billing-bulk-summary");

        BillingEntityResponse billingEntityResponse = new BillingEntityResponse();
        BillingSummary billingSummary = new BillingSummary();
        billingEntityResponse.setData(Map.of("billingSummary", List.of(Map.of("totalCount", 1, "totalRevenue", 100.0))));

        ResponseEntity<BillingEntityResponse> responseEntity = ResponseEntity.ok(billingEntityResponse);
        when(restTemplate.postForEntity(any(String.class), any(HttpEntity.class), any(Class.class)))
                .thenReturn(responseEntity);

        List<BillingSummary> billingSummaries = List.of(billingSummary);
        when(modelMapper.map(anyList(), any(Type.class)))
                .thenReturn(billingSummaries);

        List<BillingSummary> result = billingServiceAdapter.fetchBillingBulkSummaryBranchWise(billingBulkSummaryBranchWiseRequest);
        assertEquals(billingSummaries, result);
    }

    @Test
    void testFetchBillingDueSummary_SuccessfulResponse() {
        // Mock configuration values
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getBillingBulkDueSummaryBranchWise()).thenReturn("/billing-bulk-summary");

        // Create mock response data
        BillingEntityResponse billingEntityResponse = new BillingEntityResponse();
        billingEntityResponse.setData(Map.of("billingSummary", List.of(Map.of("branchId", "branchId", "moduleGuid", "moduleGuid", "dueRemaining", true))));
        ResponseEntity<BillingEntityResponse> responseEntity = ResponseEntity.ok(billingEntityResponse);

        // Mock restTemplate.exchange instead of postForEntity
        when(restTemplate.exchange(
                any(String.class),
                eq(HttpMethod.POST),
                any(HttpEntity.class),
                any(ParameterizedTypeReference.class))
        ).thenReturn(responseEntity);

        // Prepare expected mapped results
        BillingDueSummary dueSummary = new BillingDueSummary();
        List<BillingDueSummary> billingSummaries = List.of(dueSummary);

        // Mock modelMapper
        when(modelMapper.map(anyList(), any(Type.class))).thenReturn(billingSummaries);

        // Call the method under test
        List<BillingDueSummary> result = billingServiceAdapter.fetchBillingDueSummary(billingBulkSummaryBranchWiseRequest);

        // Verify results
        assertEquals(billingSummaries, result);
    }

    @Test
    void testFetchBillingDueSummary_NullData() {
        // Mock configuration values
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getBillingBulkDueSummaryBranchWise()).thenReturn("/billing-bulk-summary");

        // Create a mock response with null data
        BillingEntityResponse billingEntityResponse = new BillingEntityResponse();
        billingEntityResponse.setData(null); // Set data to null to simulate the test case
        ResponseEntity<BillingEntityResponse> responseEntity = ResponseEntity.ok(billingEntityResponse);

        // Mock restTemplate.exchange instead of postForEntity
        when(restTemplate.exchange(
                any(String.class),
                eq(HttpMethod.POST),
                any(HttpEntity.class),
                any(ParameterizedTypeReference.class))
        ).thenReturn(responseEntity);

        // Call the method under test
        List<BillingDueSummary> result = billingServiceAdapter.fetchBillingDueSummary(billingBulkSummaryBranchWiseRequest);

        // Verify that the result is an empty list
        assertEquals(Collections.emptyList(), result);
    }

    @Test
    void testFetchBillingDueSummary_NullResponse() {
        // Mock configuration values
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getBillingBulkDueSummaryBranchWise()).thenReturn("/billing-bulk-summary");

        // Create a mock response with null data and errors
        BillingEntityResponse billingEntityResponse = new BillingEntityResponse();
        billingEntityResponse.setData(null);
        billingEntityResponse.setErrors(List.of("Error_1")); // Set errors to simulate an error response

        ResponseEntity<BillingEntityResponse> responseEntity = ResponseEntity.ok(billingEntityResponse);

        // Mock restTemplate.exchange instead of postForEntity
        when(restTemplate.exchange(
                any(String.class),
                eq(HttpMethod.POST),
                any(HttpEntity.class),
                any(ParameterizedTypeReference.class))
        ).thenReturn(responseEntity);

        // Assert that a BillingException is thrown
        assertThrows(BillingException.class, () -> billingServiceAdapter.fetchBillingDueSummary(billingBulkSummaryBranchWiseRequest));
    }

    @Test
    void fetchShipmentBillingData_Success() {

        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getBillingBulkSummary()).thenReturn("/billing-bulk-summary");

        BillingEntityResponse billingEntityResponse = new BillingEntityResponse();
        BillingSummary billingSummary = BillingSummary.builder()
                .moduleGuid("123e4567-e89b-12d3-a456-426614174000")
                .totalEstimatedCost(BigDecimal.valueOf(100))
                .totalEstimatedRevenue(BigDecimal.valueOf(200))
                .totalEstimatedProfit(BigDecimal.valueOf(100))
                .totalEstimatedProfitPercent(BigDecimal.valueOf(50))
                .totalCost(90D)
                .totalRevenue(180D)
                .totalProfit(BigDecimal.valueOf(90))
                .totalProfitPercent(BigDecimal.valueOf(50))
                .totalPostedCost(BigDecimal.valueOf(85))
                .totalPostedRevenue(BigDecimal.valueOf(170))
                .totalPostedProfit(BigDecimal.valueOf(85))
                .totalPostedProfitPercent(BigDecimal.valueOf(50))
                .build();
        billingEntityResponse.setData(Map.of("billingSummary", List.of(Map.of("totalCount", 1, "totalRevenue", 100.0))));

        ResponseEntity<BillingEntityResponse> responseEntity = ResponseEntity.ok(billingEntityResponse);
        when(restTemplate.postForEntity(any(String.class), any(HttpEntity.class), any(Class.class)))
                .thenReturn(responseEntity);

        List<BillingSummary> billingSummaries = List.of(billingSummary);
        when(modelMapper.map(anyList(), any(Type.class)))
                .thenReturn(billingSummaries);


        BillingBulkSummaryRequest bulkSummaryRequest = BillingBulkSummaryRequest.builder()
                .moduleGuids(List.of("123e4567-e89b-12d3-a456-426614174000"))
                .moduleType(Constants.SHIPMENT).build();

        // Arrange
        when(billingServiceAdapter.fetchBillingBulkSummary(bulkSummaryRequest))
                .thenReturn(List.of(
                        BillingSummary.builder()
                                .moduleGuid("123e4567-e89b-12d3-a456-426614174000")
                                .totalEstimatedCost(BigDecimal.valueOf(100))
                                .totalEstimatedRevenue(BigDecimal.valueOf(200))
                                .totalEstimatedProfit(BigDecimal.valueOf(100))
                                .totalEstimatedProfitPercent(BigDecimal.valueOf(50))
                                .totalCost(90D)
                                .totalRevenue(180D)
                                .totalProfit(BigDecimal.valueOf(90))
                                .totalProfitPercent(BigDecimal.valueOf(50))
                                .totalPostedCost(BigDecimal.valueOf(85))
                                .totalPostedRevenue(BigDecimal.valueOf(170))
                                .totalPostedProfit(BigDecimal.valueOf(85))
                                .totalPostedProfitPercent(BigDecimal.valueOf(50))
                                .build()
                ));

        ShipmentBillingListRequest request = ShipmentBillingListRequest.builder()
                .guidsList(List.of(UUID.fromString("123e4567-e89b-12d3-a456-426614174000"))).build();

        // Act
        ShipmentBillingListResponse response = billingServiceAdapter.fetchShipmentBillingData(request);

        // Assert
        assertNotNull(response);
        assertEquals(1, response.getData().size());
        BillingData billingData = response.getData().get("123e4567-e89b-12d3-a456-426614174000");
        assertNotNull(billingData);
        assertEquals(BigDecimal.valueOf(100), billingData.getTotalEstimatedCost());
        assertEquals(BigDecimal.valueOf(200), billingData.getTotalEstimatedRevenue());
        assertEquals(BigDecimal.valueOf(100), billingData.getTotalEstimatedProfit());
        assertEquals(BigDecimal.valueOf(50), billingData.getTotalEstimatedProfitPercent());
        assertEquals(BigDecimal.valueOf(90.0), billingData.getTotalCost());
        assertEquals(BigDecimal.valueOf(180.0), billingData.getTotalRevenue());
        assertEquals(BigDecimal.valueOf(90), billingData.getTotalProfit());
        assertEquals(BigDecimal.valueOf(50), billingData.getTotalProfitPercent());
        assertEquals(BigDecimal.valueOf(85), billingData.getTotalPostedCost());
        assertEquals(BigDecimal.valueOf(170), billingData.getTotalPostedRevenue());
        assertEquals(BigDecimal.valueOf(85), billingData.getTotalPostedProfit());
        assertEquals(BigDecimal.valueOf(50), billingData.getTotalPostedProfitPercent());
        assertNull(billingData.getId());
    }

    @Test
    void createBillV2_failedTenantModel() {
        ShipmentDetailsResponse shipmentDetailsResponse =  new ShipmentDetailsResponse();
        shipmentDetailsResponse.setGuid(UUID.randomUUID());

        CustomerBooking customerBooking = new CustomerBooking();

        CreateBookingModuleInV1 createBookingModuleInV1 = new CreateBookingModuleInV1();

        V1RetrieveResponse v1RetrieveResponse = new V1RetrieveResponse();
        HttpHeaders httpHeaders = new HttpHeaders();
        when(v1Service.retrieveTenant(httpHeaders)).thenReturn(v1RetrieveResponse);
        when(v1Service.retrieveTenant(httpHeaders).getEntity()).thenThrow(new BillingException());

        when(v1ServiceUtil.createBookingRequestForV1(any(), anyBoolean(), anyBoolean(),
                eq(shipmentDetailsResponse.getGuid()))).thenReturn(createBookingModuleInV1);

        assertThrows(BillingException.class, () -> billingServiceAdapter.createBillV2(customerBooking, false, true, shipmentDetailsResponse, httpHeaders));
    }

    @Test
    void createBillV2_emptyClientCode() {
        ShipmentDetailsResponse shipmentDetailsResponse =  new ShipmentDetailsResponse();
        shipmentDetailsResponse.setGuid(UUID.randomUUID());

        CustomerBooking customerBooking = new CustomerBooking();
        TenantModel tenantModel = new TenantModel();
        BookingEntity bookingEntity = new BookingEntity();
        CreateBookingModuleInV1 createBookingModuleInV1 = new CreateBookingModuleInV1();
        createBookingModuleInV1.setEntity(bookingEntity);
        V1RetrieveResponse v1RetrieveResponse = new V1RetrieveResponse();
        v1RetrieveResponse.setEntity(tenantModel);
        HttpHeaders httpHeaders = new HttpHeaders();
        when(v1Service.retrieveTenant(httpHeaders)).thenReturn(v1RetrieveResponse);

        when(v1ServiceUtil.createBookingRequestForV1(any(), anyBoolean(), anyBoolean(),
                eq(shipmentDetailsResponse.getGuid()))).thenReturn(createBookingModuleInV1);

        assertThrows(BillingException.class, () -> billingServiceAdapter.createBillV2(customerBooking, false, true, shipmentDetailsResponse, httpHeaders));

    }

    @Test
    void createBillV2_emptyBillCharges() {
        ShipmentDetailsResponse shipmentDetailsResponse =  new ShipmentDetailsResponse();
        shipmentDetailsResponse.setGuid(UUID.randomUUID());

        CustomerBooking customerBooking = new CustomerBooking();
        TenantModel tenantModel = new TenantModel();

        EntityTransferOrganizations entityTransferOrganization = new EntityTransferOrganizations();
        entityTransferOrganization.setId(1L);
        entityTransferOrganization.setOrganizationCode("OrganizationCode");

        List<EntityTransferOrganizations> entityTransferOrganizations = List.of(entityTransferOrganization);

        V1DataResponse orgResponse = new V1DataResponse();
        orgResponse.setEntities(entityTransferOrganizations);

        EntityTransferAddress entityTransferAddress = new EntityTransferAddress();
        entityTransferAddress.setId(1L);
        entityTransferAddress.setAddressShortCode("AddressShortCode");

        List<EntityTransferAddress> entityTransferAddresses = List.of(entityTransferAddress);

        V1DataResponse addressResponse = new V1DataResponse();
        addressResponse.setEntities(entityTransferAddresses);

        BookingEntity entity = new BookingEntity();
        entity.setClientCode("OrganizationCode");
        entity.setClientAddressShortCode("AddressShortCode");
        entity.setBillCharges(List.of());

        CreateBookingModuleInV1 createBookingModuleInV1 = new CreateBookingModuleInV1();
        createBookingModuleInV1.setEntity(entity);
        V1RetrieveResponse v1RetrieveResponse = new V1RetrieveResponse();
        v1RetrieveResponse.setEntity(tenantModel);

        ResponseEntity<BillingEntityResponse> mockResponse = new ResponseEntity<>(billingEntityResponse, HttpStatus.OK);
        HttpHeaders httpHeaders = new HttpHeaders();
        when(v1Service.retrieveTenant(httpHeaders)).thenReturn(v1RetrieveResponse);
        when(v1Service.fetchOrganization(any(), any(HttpHeaders.class))).thenReturn(orgResponse);
        when(jsonHelper.convertValueToList(orgResponse.entities, EntityTransferOrganizations.class)).thenReturn(entityTransferOrganizations);
        when(v1Service.addressList(any(), any(HttpHeaders.class))).thenReturn(addressResponse);
        when(jsonHelper.convertValueToList(addressResponse.entities, EntityTransferAddress.class)).thenReturn(entityTransferAddresses);
        when(v1ServiceUtil.createBookingRequestForV1(any(), anyBoolean(), anyBoolean(),
                eq(shipmentDetailsResponse.getGuid()))).thenReturn(createBookingModuleInV1);
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getExternalCreateOrUpdate()).thenReturn(createOrUpdateEndpoint);
        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(BillingEntityResponse.class))).thenReturn(mockResponse);

        ResponseEntity<BillingEntityResponse> billV2 = billingServiceAdapter.createBillV2(customerBooking, false, true, shipmentDetailsResponse, httpHeaders);

        assertNotNull(billV2);

    }

    @Test
    void createBillV2() {
        ShipmentDetailsResponse shipmentDetailsResponse =  new ShipmentDetailsResponse();
        shipmentDetailsResponse.setGuid(UUID.randomUUID());

        CustomerBooking customerBooking = new CustomerBooking();

        TenantModel tenantModel = new TenantModel();
        tenantModel.setCurrencyCode(null);

        List<BillCharge> billCharges = new ArrayList<>();
        for (MeasurementBasis value : MeasurementBasis.values()) {
            BillCharge billCharge = BillCharge.builder()
                    .PerMeasurementBasis(value.toString())
                    .CreditorCode("OrganizationCode")
                    .ContainersGuid(List.of(UUID.randomUUID())).build();
            billCharges.add(billCharge);
        }

        EntityTransferOrganizations entityTransferOrganization = new EntityTransferOrganizations();
        entityTransferOrganization.setId(1L);
        entityTransferOrganization.setOrganizationCode("OrganizationCode");
        entityTransferOrganization.setPayables(Boolean.TRUE);

        List<EntityTransferOrganizations> entityTransferOrganizations = List.of(entityTransferOrganization);

        V1DataResponse orgResponse = new V1DataResponse();
        orgResponse.setEntities(entityTransferOrganizations);

        EntityTransferAddress entityTransferAddress = new EntityTransferAddress();
        entityTransferAddress.setId(1L);
        entityTransferAddress.setAddressShortCode("AddressShortCode");
        entityTransferAddress.setOrgId(1L);
        entityTransferAddress.setDefaultAddress(Boolean.TRUE);

        List<EntityTransferAddress> entityTransferAddresses = List.of(entityTransferAddress);

        V1DataResponse addressResponse = new V1DataResponse();
        addressResponse.setEntities(entityTransferAddresses);

        BookingEntity entity = new BookingEntity();
        entity.setClientCode("OrganizationCode");
        entity.setClientAddressShortCode("AddressShortCode");
        entity.setBillCharges(billCharges);

        CreateBookingModuleInV1 createBookingModuleInV1 = new CreateBookingModuleInV1();
        createBookingModuleInV1.setEntity(entity);
        V1RetrieveResponse v1RetrieveResponse = new V1RetrieveResponse();
        v1RetrieveResponse.setEntity(tenantModel);

        ResponseEntity<BillingEntityResponse> mockResponse = new ResponseEntity<>(billingEntityResponse, HttpStatus.OK);
        HttpHeaders httpHeaders = new HttpHeaders();

        when(v1Service.retrieveTenant(httpHeaders)).thenReturn(v1RetrieveResponse);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(v1Service.fetchOrganization(any(),any(HttpHeaders.class))).thenReturn(orgResponse);
        when(jsonHelper.convertValueToList(orgResponse.entities, EntityTransferOrganizations.class)).thenReturn(entityTransferOrganizations);
        when(v1Service.addressList(any(), any(HttpHeaders.class))).thenReturn(addressResponse);
        when(jsonHelper.convertValueToList(addressResponse.entities, EntityTransferAddress.class)).thenReturn(entityTransferAddresses);
        when(v1ServiceUtil.createBookingRequestForV1(any(), anyBoolean(), anyBoolean(),
                eq(shipmentDetailsResponse.getGuid()))).thenReturn(createBookingModuleInV1);
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getExternalCreateOrUpdate()).thenReturn(createOrUpdateEndpoint);
        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(BillingEntityResponse.class))).thenReturn(mockResponse);

        ResponseEntity<BillingEntityResponse> billV2 = billingServiceAdapter.createBillV2(customerBooking, false, true, shipmentDetailsResponse, httpHeaders);

        assertNotNull(billV2);

    }

    @Test
    void createBillV2_noDefaultAddress() {
        ShipmentDetailsResponse shipmentDetailsResponse =  new ShipmentDetailsResponse();
        shipmentDetailsResponse.setGuid(UUID.randomUUID());

        CustomerBooking customerBooking = new CustomerBooking();

        TenantModel tenantModel = new TenantModel();
        tenantModel.setCurrencyCode(null);

        List<BillCharge> billCharges = new ArrayList<>();
        for (MeasurementBasis value : MeasurementBasis.values()) {
            BillCharge billCharge = BillCharge.builder()
                    .PerMeasurementBasis(value.toString())
                    .CreditorCode("OrganizationCode")
                    .ContainersGuid(List.of(UUID.randomUUID())).build();
            billCharges.add(billCharge);
        }

        EntityTransferOrganizations entityTransferOrganization = new EntityTransferOrganizations();
        entityTransferOrganization.setId(1L);
        entityTransferOrganization.setOrganizationCode("OrganizationCode");
        entityTransferOrganization.setPayables(Boolean.FALSE);

        List<EntityTransferOrganizations> entityTransferOrganizations = List.of(entityTransferOrganization);

        V1DataResponse orgResponse = new V1DataResponse();
        orgResponse.setEntities(entityTransferOrganizations);

        EntityTransferAddress entityTransferAddress = new EntityTransferAddress();
        entityTransferAddress.setId(1L);
        entityTransferAddress.setAddressShortCode("AddressShortCode");
        entityTransferAddress.setOrgId(1L);
        entityTransferAddress.setDefaultAddress(Boolean.FALSE);

        List<EntityTransferAddress> entityTransferAddresses = List.of(entityTransferAddress);

        V1DataResponse addressResponse = new V1DataResponse();
        addressResponse.setEntities(entityTransferAddresses);

        BookingEntity entity = new BookingEntity();
        entity.setClientCode("OrganizationCode");
        entity.setClientAddressShortCode("AddressShortCode");
        entity.setBillCharges(billCharges);

        CreateBookingModuleInV1 createBookingModuleInV1 = new CreateBookingModuleInV1();
        createBookingModuleInV1.setEntity(entity);
        V1RetrieveResponse v1RetrieveResponse = new V1RetrieveResponse();
        v1RetrieveResponse.setEntity(tenantModel);

        ResponseEntity<BillingEntityResponse> mockResponse = new ResponseEntity<>(billingEntityResponse, HttpStatus.OK);
        HttpHeaders httpHeaders = new HttpHeaders();
        when(v1Service.retrieveTenant(httpHeaders)).thenReturn(v1RetrieveResponse);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(v1Service.fetchOrganization(any(),any(HttpHeaders.class))).thenReturn(orgResponse);
        when(jsonHelper.convertValueToList(orgResponse.entities, EntityTransferOrganizations.class)).thenReturn(entityTransferOrganizations);
        when(v1Service.addressList(any(), any(HttpHeaders.class))).thenReturn(addressResponse);
        when(jsonHelper.convertValueToList(addressResponse.entities, EntityTransferAddress.class)).thenReturn(entityTransferAddresses);
        when(v1ServiceUtil.createBookingRequestForV1(any(), anyBoolean(), anyBoolean(),
                eq(shipmentDetailsResponse.getGuid()))).thenReturn(createBookingModuleInV1);
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getExternalCreateOrUpdate()).thenReturn(createOrUpdateEndpoint);
        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(BillingEntityResponse.class))).thenReturn(mockResponse);

        ResponseEntity<BillingEntityResponse> billV2 = billingServiceAdapter.createBillV2(customerBooking, false, true, shipmentDetailsResponse, httpHeaders);

        assertNotNull(billV2);

    }

    @Test
    void createBillV2_noDefaultAddress_2() {
        ShipmentDetailsResponse shipmentDetailsResponse =  new ShipmentDetailsResponse();
        shipmentDetailsResponse.setGuid(UUID.randomUUID());

        CustomerBooking customerBooking = new CustomerBooking();

        TenantModel tenantModel = new TenantModel();
        tenantModel.setCurrencyCode(null);

        List<BillCharge> billCharges = new ArrayList<>();
        for (MeasurementBasis value : MeasurementBasis.values()) {
            BillCharge billCharge = BillCharge.builder()
                    .PerMeasurementBasis(value.toString())
                    .CreditorCode("OrganizationCode")
                    .ContainersGuid(List.of(UUID.randomUUID())).build();
            billCharges.add(billCharge);
        }

        EntityTransferOrganizations entityTransferOrganization = new EntityTransferOrganizations();
        entityTransferOrganization.setId(1L);
        entityTransferOrganization.setOrganizationCode("OrganizationCode");
        entityTransferOrganization.setPayables(Boolean.TRUE);

        List<EntityTransferOrganizations> entityTransferOrganizations = List.of(entityTransferOrganization);

        V1DataResponse orgResponse = new V1DataResponse();
        orgResponse.setEntities(entityTransferOrganizations);

        EntityTransferAddress entityTransferAddress = new EntityTransferAddress();
        entityTransferAddress.setId(1L);
        entityTransferAddress.setAddressShortCode("AddressShortCode1");
        entityTransferAddress.setOrgId(1L);
        entityTransferAddress.setDefaultAddress(null);

        List<EntityTransferAddress> entityTransferAddresses = List.of(entityTransferAddress);

        V1DataResponse addressResponse = new V1DataResponse();
        addressResponse.setEntities(entityTransferAddresses);

        BookingEntity entity = new BookingEntity();
        entity.setClientCode("OrganizationCode");
        entity.setClientAddressShortCode("AddressShortCode");
        entity.setBillCharges(billCharges);

        CreateBookingModuleInV1 createBookingModuleInV1 = new CreateBookingModuleInV1();
        createBookingModuleInV1.setEntity(entity);
        V1RetrieveResponse v1RetrieveResponse = new V1RetrieveResponse();
        v1RetrieveResponse.setEntity(tenantModel);

        ResponseEntity<BillingEntityResponse> mockResponse = new ResponseEntity<>(billingEntityResponse, HttpStatus.OK);
        HttpHeaders httpHeaders = new HttpHeaders();
        when(v1Service.retrieveTenant(httpHeaders)).thenReturn(v1RetrieveResponse);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(v1Service.fetchOrganization(any(), any(HttpHeaders.class))).thenReturn(orgResponse);
        when(jsonHelper.convertValueToList(orgResponse.entities, EntityTransferOrganizations.class)).thenReturn(entityTransferOrganizations);
        when(v1Service.addressList(any(), any(HttpHeaders.class))).thenReturn(addressResponse);
        when(jsonHelper.convertValueToList(addressResponse.entities, EntityTransferAddress.class)).thenReturn(entityTransferAddresses);
        when(v1ServiceUtil.createBookingRequestForV1(any(), anyBoolean(), anyBoolean(),
                eq(shipmentDetailsResponse.getGuid()))).thenReturn(createBookingModuleInV1);
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getExternalCreateOrUpdate()).thenReturn(createOrUpdateEndpoint);
        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(BillingEntityResponse.class))).thenReturn(mockResponse);


        ResponseEntity<BillingEntityResponse> billV2 = billingServiceAdapter.createBillV2(customerBooking, false, true, shipmentDetailsResponse, httpHeaders);

        assertNotNull(billV2);

    }

    @Test
    void fetchBill_Success() {
        try (MockedStatic<V1AuthHelper> v1AuthHelperMockedStatic = mockStatic(V1AuthHelper.class)) {
            v1AuthHelperMockedStatic.when(V1AuthHelper::getHeaders).thenReturn(new HttpHeaders());

            BillRetrieveRequest request = new BillRetrieveRequest();
            BillingEntityResponse mockResponse = new BillingEntityResponse();
            Map<String, Object> mockData = new HashMap<>();
            mockResponse.setData(mockData);
            ResponseEntity<BillingEntityResponse> mockResponseEntity = new ResponseEntity<>(mockResponse, HttpStatus.OK);
            ParameterizedTypeReference<BillingEntityResponse> responseType = new ParameterizedTypeReference<>() {
            };

            when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), eq(responseType)))
                    .thenReturn(mockResponseEntity);

            BillBaseResponse expectedResponse = new BillBaseResponse();
            when(modelMapper.map(mockData, BillBaseResponse.class)).thenReturn(expectedResponse);

            BillBaseResponse response = billingServiceAdapter.fetchBill(request);

            assertNotNull(response);
            assertEquals(expectedResponse, response);
        }
    }

    @Test
    void fetchBill_NullResponse() {
        try (MockedStatic<V1AuthHelper> v1AuthHelperMockedStatic = mockStatic(V1AuthHelper.class)) {
            v1AuthHelperMockedStatic.when(V1AuthHelper::getHeaders).thenReturn(new HttpHeaders());

            BillRetrieveRequest request = new BillRetrieveRequest();
            ResponseEntity<BillingEntityResponse> mockResponseEntity = new ResponseEntity<>(null, HttpStatus.OK);
            ParameterizedTypeReference<BillingEntityResponse> responseType = new ParameterizedTypeReference<>() {
            };

            when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), eq(responseType)))
                    .thenReturn(mockResponseEntity);

            assertThrows(BillingException.class, () -> billingServiceAdapter.fetchBill(request));
        }
    }

    @Test
    void fetchBill_NullData() {
        try (MockedStatic<V1AuthHelper> v1AuthHelperMockedStatic = mockStatic(V1AuthHelper.class)) {
            v1AuthHelperMockedStatic.when(V1AuthHelper::getHeaders).thenReturn(new HttpHeaders());

            BillRetrieveRequest request = new BillRetrieveRequest();
            BillingEntityResponse mockResponse = new BillingEntityResponse();
            mockResponse.setData(null);
            ResponseEntity<BillingEntityResponse> mockResponseEntity = new ResponseEntity<>(mockResponse, HttpStatus.OK);
            ParameterizedTypeReference<BillingEntityResponse> responseType = new ParameterizedTypeReference<>() {
            };

            when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), eq(responseType)))
                    .thenReturn(mockResponseEntity);

            assertThrows(BillingException.class, () -> billingServiceAdapter.fetchBill(request));
        }
    }

    @Test
    void fetchBill_ExceptionThrown() {
        try (MockedStatic<V1AuthHelper> v1AuthHelperMockedStatic = mockStatic(V1AuthHelper.class)) {
            v1AuthHelperMockedStatic.when(V1AuthHelper::getHeaders).thenReturn(new HttpHeaders());

            BillRetrieveRequest request = new BillRetrieveRequest();
            ParameterizedTypeReference<BillingEntityResponse> responseType = new ParameterizedTypeReference<>() {
            };

            when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), eq(responseType)))
                    .thenThrow(new RuntimeException("Runtime exception"));

            assertThrows(BillingException.class, () -> billingServiceAdapter.fetchBill(request));
        }
    }


    @Test
    void fetchBillCharges_Success() {
        try (MockedStatic<V1AuthHelper> v1AuthHelperMockedStatic = mockStatic(V1AuthHelper.class)) {
            v1AuthHelperMockedStatic.when(V1AuthHelper::getHeaders).thenReturn(new HttpHeaders());

            List<BillChargesBaseResponse> mockData = List.of(new BillChargesBaseResponse());
            billChargesBaseResponseBillingListResponse.setData(mockData);
            ResponseEntity<BillingListResponse<BillChargesBaseResponse>> mockResponseEntity = new ResponseEntity<>(billChargesBaseResponseBillingListResponse, HttpStatus.OK);
            ParameterizedTypeReference<BillingListResponse<BillChargesBaseResponse>> responseType = new ParameterizedTypeReference<>() {
            };

            when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), eq(responseType)))
                    .thenReturn(mockResponseEntity);

            when(modelMapper.map(billChargesBaseResponseBillingListResponse.getData(), new TypeToken<List<BillChargesBaseResponse>>() {
            }.getType()))
                    .thenReturn(mockData);

            List<BillChargesBaseResponse> response = billingServiceAdapter.fetchBillCharges(billChargesFilterRequest);

            assertNotNull(response);
            assertEquals(1, response.size());
            assertEquals(mockData.get(0), response.get(0));
        }
    }

    @Test
    void fetchBillCharges_NullResponse() {
        try (MockedStatic<V1AuthHelper> v1AuthHelperMockedStatic = mockStatic(V1AuthHelper.class)) {
            v1AuthHelperMockedStatic.when(V1AuthHelper::getHeaders).thenReturn(new HttpHeaders());

            ResponseEntity<BillingListResponse<BillChargesBaseResponse>> mockResponseEntity = new ResponseEntity<>(null, HttpStatus.OK);
            ParameterizedTypeReference<BillingListResponse<BillChargesBaseResponse>> responseType = new ParameterizedTypeReference<>() {
            };

            when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), eq(responseType)))
                    .thenReturn(mockResponseEntity);

            assertThrows(BillingException.class, () -> billingServiceAdapter.fetchBillCharges(billChargesFilterRequest));
        }
    }

    @Test
    void fetchBillCharges_NullData() {
        try (MockedStatic<V1AuthHelper> v1AuthHelperMockedStatic = mockStatic(V1AuthHelper.class)) {
            v1AuthHelperMockedStatic.when(V1AuthHelper::getHeaders).thenReturn(new HttpHeaders());

            billChargesBaseResponseBillingListResponse.setData(null);
            ResponseEntity<BillingListResponse<BillChargesBaseResponse>> mockResponseEntity = new ResponseEntity<>(billChargesBaseResponseBillingListResponse, HttpStatus.OK);
            ParameterizedTypeReference<BillingListResponse<BillChargesBaseResponse>> responseType = new ParameterizedTypeReference<>() {
            };

            when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), eq(responseType)))
                    .thenReturn(mockResponseEntity);

            assertThrows(BillingException.class, () -> billingServiceAdapter.fetchBillCharges(billChargesFilterRequest));
        }
    }

    @Test
    void fetchBillCharges_ExceptionThrown() {
        try (MockedStatic<V1AuthHelper> v1AuthHelperMockedStatic = mockStatic(V1AuthHelper.class)) {
            v1AuthHelperMockedStatic.when(V1AuthHelper::getHeaders).thenReturn(new HttpHeaders());

            ParameterizedTypeReference<BillingListResponse<BillChargesBaseResponse>> responseType = new ParameterizedTypeReference<>() {
            };

            when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), eq(responseType)))
                    .thenThrow(new RuntimeException("Runtime exception"));

            assertThrows(BillingException.class, () -> billingServiceAdapter.fetchBillCharges(billChargesFilterRequest));
        }
    }

    @Test
    void sendBillCreationRequest_Success() {

        createOrUpdateEndpoint = "/createOrUpdate";
        HttpHeaders httpHeaders = new HttpHeaders();

        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getExternalCreateOrUpdate()).thenReturn(createOrUpdateEndpoint);
        ResponseEntity<BillingEntityResponse> mockResponse = new ResponseEntity<>(billingEntityResponse, HttpStatus.OK);

        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(BillingEntityResponse.class)))
                .thenReturn(mockResponse);

        ResponseEntity<BillingEntityResponse> response = billingServiceAdapter.sendBillCreationRequest(externalBillPayloadRequest, httpHeaders);

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

        HttpHeaders httpHeaders = new HttpHeaders();
        assertThrows(BillingException.class, () -> billingServiceAdapter.sendBillCreationRequest(externalBillPayloadRequest, httpHeaders));
    }

    @Test
    void sendBillCreationRequest_Exception() {
        createOrUpdateEndpoint = "/createOrUpdate";

        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getExternalCreateOrUpdate()).thenReturn(createOrUpdateEndpoint);
        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(BillingEntityResponse.class)))
                .thenThrow(new RuntimeException("Error occurred"));

        HttpHeaders httpHeaders = new HttpHeaders();
        BillingException exception = assertThrows(BillingException.class, () -> {
            billingServiceAdapter.sendBillCreationRequest(externalBillPayloadRequest, httpHeaders);
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
