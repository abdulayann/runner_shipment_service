package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.dto.request.CreateBookingModuleInV1;
import com.dpw.runner.shipment.services.dto.request.CreateBookingModuleInV1.BookingEntity;
import com.dpw.runner.shipment.services.dto.request.CreateBookingModuleInV1.BookingEntity.BillCharge;
import com.dpw.runner.shipment.services.dto.request.billing.*;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingEntityResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingListResponse;
import com.dpw.runner.shipment.services.dto.v1.TenantModel;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.enums.MeasurementBasis;
import com.dpw.runner.shipment.services.masterDataObjects.dto.AddressData;
import com.dpw.runner.shipment.services.masterDataObjects.dto.OrganizationsMasterData;
import com.dpw.runner.shipment.services.exception.exceptions.billing.BillingException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import com.dpw.runner.shipment.services.utils.V2AuthHelper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

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
    void createBillV2_failedTenantModel() {
        ShipmentDetailsResponse shipmentDetailsResponse =  new ShipmentDetailsResponse();
        shipmentDetailsResponse.setGuid(UUID.randomUUID());

        CustomerBooking customerBooking = new CustomerBooking();

        CreateBookingModuleInV1 createBookingModuleInV1 = new CreateBookingModuleInV1();

        V1RetrieveResponse v1RetrieveResponse = new V1RetrieveResponse();

        when(v1Service.retrieveTenant()).thenReturn(v1RetrieveResponse);
        when(v1Service.retrieveTenant().getEntity()).thenThrow(new BillingException());

        when(v1ServiceUtil.createBookingRequestForV1(any(), anyBoolean(), anyBoolean(),
                eq(shipmentDetailsResponse.getGuid()))).thenReturn(createBookingModuleInV1);

        assertThrows(BillingException.class, () -> billingServiceAdapter.createBillV2(customerBooking, false, true, shipmentDetailsResponse));
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

        when(v1Service.retrieveTenant()).thenReturn(v1RetrieveResponse);

        when(v1ServiceUtil.createBookingRequestForV1(any(), anyBoolean(), anyBoolean(),
                eq(shipmentDetailsResponse.getGuid()))).thenReturn(createBookingModuleInV1);
        assertThrows(BillingException.class, () -> billingServiceAdapter.createBillV2(customerBooking, false, true, shipmentDetailsResponse));

    }

    @Test
    void createBillV2_emptyBillCharges() {
        ShipmentDetailsResponse shipmentDetailsResponse =  new ShipmentDetailsResponse();
        shipmentDetailsResponse.setGuid(UUID.randomUUID());

        CustomerBooking customerBooking = new CustomerBooking();
        TenantModel tenantModel = new TenantModel();

        OrganizationsMasterData entityTransferOrganization = new OrganizationsMasterData();
        entityTransferOrganization.setId(1L);
        entityTransferOrganization.setOrganizationCode("OrganizationCode");

        List<OrganizationsMasterData> entityTransferOrganizations = List.of(entityTransferOrganization);

        V1DataResponse orgResponse = new V1DataResponse();
        orgResponse.setEntities(entityTransferOrganizations);

        AddressData entityTransferAddress = new AddressData();
        entityTransferAddress.setId(1L);
        entityTransferAddress.setAddressShortCode("AddressShortCode");

        List<AddressData> entityTransferAddresses = List.of(entityTransferAddress);

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

        when(v1Service.retrieveTenant()).thenReturn(v1RetrieveResponse);
        when(v1Service.fetchOrganization(any())).thenReturn(orgResponse);
        when(jsonHelper.convertValueToList(orgResponse.entities, OrganizationsMasterData.class)).thenReturn(entityTransferOrganizations);
        when(v1Service.addressList(any())).thenReturn(addressResponse);
        when(jsonHelper.convertValueToList(addressResponse.entities, AddressData.class)).thenReturn(entityTransferAddresses);
        when(v1ServiceUtil.createBookingRequestForV1(any(), anyBoolean(), anyBoolean(),
                eq(shipmentDetailsResponse.getGuid()))).thenReturn(createBookingModuleInV1);
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getExternalCreateOrUpdate()).thenReturn(createOrUpdateEndpoint);
        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(BillingEntityResponse.class))).thenReturn(mockResponse);

        ResponseEntity<BillingEntityResponse> billV2 = billingServiceAdapter.createBillV2(customerBooking, false, true, shipmentDetailsResponse);

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

        OrganizationsMasterData entityTransferOrganization = new OrganizationsMasterData();
        entityTransferOrganization.setId(1L);
        entityTransferOrganization.setOrganizationCode("OrganizationCode");
        entityTransferOrganization.setPayables(Boolean.TRUE);

        List<OrganizationsMasterData> entityTransferOrganizations = List.of(entityTransferOrganization);

        V1DataResponse orgResponse = new V1DataResponse();
        orgResponse.setEntities(entityTransferOrganizations);

        AddressData entityTransferAddress = new AddressData();
        entityTransferAddress.setId(1L);
        entityTransferAddress.setAddressShortCode("AddressShortCode");
        entityTransferAddress.setOrgId(1L);
        entityTransferAddress.setDefaultAddress(Boolean.TRUE);

        List<AddressData> entityTransferAddresses = List.of(entityTransferAddress);

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

        when(v1Service.retrieveTenant()).thenReturn(v1RetrieveResponse);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(v1Service.fetchOrganization(any())).thenReturn(orgResponse);
        when(jsonHelper.convertValueToList(orgResponse.entities, OrganizationsMasterData.class)).thenReturn(entityTransferOrganizations);
        when(v1Service.addressList(any())).thenReturn(addressResponse);
        when(jsonHelper.convertValueToList(addressResponse.entities, AddressData.class)).thenReturn(entityTransferAddresses);
        when(v1ServiceUtil.createBookingRequestForV1(any(), anyBoolean(), anyBoolean(),
                eq(shipmentDetailsResponse.getGuid()))).thenReturn(createBookingModuleInV1);
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getExternalCreateOrUpdate()).thenReturn(createOrUpdateEndpoint);
        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(BillingEntityResponse.class))).thenReturn(mockResponse);

        ResponseEntity<BillingEntityResponse> billV2 = billingServiceAdapter.createBillV2(customerBooking, false, true, shipmentDetailsResponse);

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

        OrganizationsMasterData entityTransferOrganization = new OrganizationsMasterData();
        entityTransferOrganization.setId(1L);
        entityTransferOrganization.setOrganizationCode("OrganizationCode");
        entityTransferOrganization.setPayables(Boolean.FALSE);

        List<OrganizationsMasterData> entityTransferOrganizations = List.of(entityTransferOrganization);

        V1DataResponse orgResponse = new V1DataResponse();
        orgResponse.setEntities(entityTransferOrganizations);

        AddressData entityTransferAddress = new AddressData();
        entityTransferAddress.setId(1L);
        entityTransferAddress.setAddressShortCode("AddressShortCode");
        entityTransferAddress.setOrgId(1L);
        entityTransferAddress.setDefaultAddress(Boolean.FALSE);

        List<AddressData> entityTransferAddresses = List.of(entityTransferAddress);

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

        when(v1Service.retrieveTenant()).thenReturn(v1RetrieveResponse);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(v1Service.fetchOrganization(any())).thenReturn(orgResponse);
        when(jsonHelper.convertValueToList(orgResponse.entities, OrganizationsMasterData.class)).thenReturn(entityTransferOrganizations);
        when(v1Service.addressList(any())).thenReturn(addressResponse);
        when(jsonHelper.convertValueToList(addressResponse.entities, AddressData.class)).thenReturn(entityTransferAddresses);
        when(v1ServiceUtil.createBookingRequestForV1(any(), anyBoolean(), anyBoolean(),
                eq(shipmentDetailsResponse.getGuid()))).thenReturn(createBookingModuleInV1);
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getExternalCreateOrUpdate()).thenReturn(createOrUpdateEndpoint);
        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(BillingEntityResponse.class))).thenReturn(mockResponse);

        ResponseEntity<BillingEntityResponse> billV2 = billingServiceAdapter.createBillV2(customerBooking, false, true, shipmentDetailsResponse);

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

        OrganizationsMasterData entityTransferOrganization = new OrganizationsMasterData();
        entityTransferOrganization.setId(1L);
        entityTransferOrganization.setOrganizationCode("OrganizationCode");
        entityTransferOrganization.setPayables(Boolean.TRUE);

        List<OrganizationsMasterData> entityTransferOrganizations = List.of(entityTransferOrganization);

        V1DataResponse orgResponse = new V1DataResponse();
        orgResponse.setEntities(entityTransferOrganizations);

        AddressData entityTransferAddress = new AddressData();
        entityTransferAddress.setId(1L);
        entityTransferAddress.setAddressShortCode("AddressShortCode1");
        entityTransferAddress.setOrgId(1L);
        entityTransferAddress.setDefaultAddress(null);

        List<AddressData> entityTransferAddresses = List.of(entityTransferAddress);

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

        when(v1Service.retrieveTenant()).thenReturn(v1RetrieveResponse);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(v1Service.fetchOrganization(any())).thenReturn(orgResponse);
        when(jsonHelper.convertValueToList(orgResponse.entities, OrganizationsMasterData.class)).thenReturn(entityTransferOrganizations);
        when(v1Service.addressList(any())).thenReturn(addressResponse);
        when(jsonHelper.convertValueToList(addressResponse.entities, AddressData.class)).thenReturn(entityTransferAddresses);
        when(v1ServiceUtil.createBookingRequestForV1(any(), anyBoolean(), anyBoolean(),
                eq(shipmentDetailsResponse.getGuid()))).thenReturn(createBookingModuleInV1);
        when(billingServiceUrlConfig.getBaseUrl()).thenReturn(baseUrl);
        when(billingServiceUrlConfig.getExternalCreateOrUpdate()).thenReturn(createOrUpdateEndpoint);
        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(BillingEntityResponse.class))).thenReturn(mockResponse);

        ResponseEntity<BillingEntityResponse> billV2 = billingServiceAdapter.createBillV2(customerBooking, false, true, shipmentDetailsResponse);

        assertNotNull(billV2);

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

}
