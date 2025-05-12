package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.*;

@ContextConfiguration(classes = {MasterDataHelper.class})
@ExtendWith(SpringExtension.class)
class MasterDataHelperTest {
    @MockBean
    private IShipmentDao iShipmentDao;

    @MockBean
    private IV1Service iV1Service;

    @Autowired
    private MasterDataHelper masterDataHelper;

    @MockBean
    private MasterDataKeyUtils masterDataKeyUtils;

    @MockBean
    private MasterDataUtils masterDataUtils;

    @MockBean
    private CommonUtils commonUtils;

    @Test
    void testAddAllMasterDataInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setCarrierDetails(CarrierDetailResponse.builder().build());
        shipmentDetailsResponse.setAdditionalDetails(AdditionalDetailResponse.builder().build());
        shipmentDetailsResponse.setRoutingsList(List.of(RoutingsResponse.builder().build()));
        shipmentDetailsResponse.setReferenceNumbersList(List.of(new ReferenceNumbersResponse()));
        shipmentDetailsResponse.setContainersList(Set.of(new ContainerResponse()));
        shipmentDetailsResponse.setPackingList(List.of(new PackingResponse()));
        shipmentDetailsResponse.setBookingCarriagesList(List.of(new BookingCarriageResponse()));
        shipmentDetailsResponse.setServicesList(List.of(new ServiceDetailsResponse()));
        shipmentDetailsResponse.setEventsList(List.of(new EventsResponse()));
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllMasterDataInSingleCallResult = masterDataHelper
                .addAllMasterDataInSingleCall(shipmentDetailsResponse, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInBulkMasterList(isA(MasterListRequestV2.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllMasterDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllMasterDataInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());

        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder().build();

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllMasterDataInSingleCallResult = masterDataHelper
                .addAllMasterDataInSingleCall(shipmentDetailsResponse, new HashMap<>());

        // Assert
        verify(masterDataUtils, atLeast(1)).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(),
                Mockito.<Class<Object>>any(), isA(Map.class), Mockito.<String>any(), isA(Map.class));
        verify(masterDataUtils).fetchInBulkMasterList(isA(MasterListRequestV2.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllMasterDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllMasterDataInSingleCall3() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder().build();
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllMasterDataInSingleCallResult = masterDataHelper
                .addAllMasterDataInSingleCall(shipmentDetailsResponse, new HashMap<>());

        // Assert
        verify(masterDataUtils, atLeast(1)).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(),
                Mockito.<Class<Object>>any(), isA(Map.class), Mockito.<String>any(), isA(Map.class));
        verify(masterDataUtils).fetchInBulkMasterList(isA(MasterListRequestV2.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllMasterDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllMasterDataInSingleCall4() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());

        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder().build();
        shipmentDetailsResponse.setAdditionalDetails(AdditionalDetailResponse.builder().build());
        shipmentDetailsResponse.setCarrierDetails(CarrierDetailResponse.builder().build());
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllMasterDataInSingleCallResult = masterDataHelper
                .addAllMasterDataInSingleCall(shipmentDetailsResponse, null);

        // Assert
        verify(masterDataUtils, atLeast(1)).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(),
                Mockito.<Class<Object>>any(), isA(Map.class), Mockito.<String>any(), isA(Map.class));
        verify(masterDataUtils).fetchInBulkMasterList(isA(MasterListRequestV2.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllMasterDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllUnlocationDataInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkUnlocations(Mockito.<Set<String>>any(), Mockito.<String>any()))
                .thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllUnlocationDataInSingleCallResult = masterDataHelper
                .addAllUnlocationDataInSingleCall(shipmentDetailsResponse, new HashMap<>());

        // Assert
        ResponseEntity<IRunnerResponse> getResult = actualAddAllUnlocationDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllUnlocationDataInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkUnlocations(Mockito.<Set<String>>any(), Mockito.<String>any()))
                .thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setCarrierDetails(CarrierDetailResponse.builder().build());
        shipmentDetailsResponse.setAdditionalDetails(AdditionalDetailResponse.builder().build());
        shipmentDetailsResponse.setRoutingsList(List.of(RoutingsResponse.builder().build()));
        shipmentDetailsResponse.setBookingCarriagesList(List.of(new BookingCarriageResponse()));
        shipmentDetailsResponse.setContainersList(Set.of(new ContainerResponse()));
        shipmentDetailsResponse.setServicesList(List.of(new ServiceDetailsResponse()));
        shipmentDetailsResponse.setPackingList(List.of(new PackingResponse()));
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllUnlocationDataInSingleCallResult = masterDataHelper
                .addAllUnlocationDataInSingleCall(shipmentDetailsResponse, new HashMap<>());

        // Assert
        ResponseEntity<IRunnerResponse> getResult = actualAddAllUnlocationDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllUnlocationDataInSingleCall3() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkUnlocations(Mockito.<Set<String>>any(), Mockito.<String>any()))
                .thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setCarrierDetails(CarrierDetailResponse.builder().build());
        shipmentDetailsResponse.setAdditionalDetails(AdditionalDetailResponse.builder().build());
        shipmentDetailsResponse.setRoutingsList(List.of(RoutingsResponse.builder().build()));
        shipmentDetailsResponse.setBookingCarriagesList(List.of(new BookingCarriageResponse()));
        shipmentDetailsResponse.setContainersList(Set.of(new ContainerResponse()));
        shipmentDetailsResponse.setServicesList(List.of(new ServiceDetailsResponse()));
        shipmentDetailsResponse.setPackingList(List.of(new PackingResponse()));
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllUnlocationDataInSingleCallResult = masterDataHelper
                .addAllUnlocationDataInSingleCall(shipmentDetailsResponse, null);

        // Assert
        ResponseEntity<IRunnerResponse> getResult = actualAddAllUnlocationDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllOrganizationDataInSingleCall_Success () throws ExecutionException, InterruptedException {
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        when(masterDataUtils.createInBulkOrganizationRequest(any(), any(), anyMap(), anyString(), anyMap())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInOrganizations(anySet(), anyString())).thenReturn(new HashMap<>());
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllOrganizationDataInSingleCallResult =  masterDataHelper.addAllOrganizationDataInSingleCall(shipmentDetailsResponse, null);
        ResponseEntity<IRunnerResponse> getResult = actualAddAllOrganizationDataInSingleCallResult.get();

        assertEquals(HttpStatus.OK, getResult.getStatusCode());
    }

    @Test
    void testAddAllOrganizationDataInSingleCall_Error () throws ExecutionException, InterruptedException {
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        when(masterDataUtils.createInBulkOrganizationRequest(any(), any(), anyMap(), anyString(), anyMap())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInOrganizations(anySet(), anyString())).thenThrow(new RuntimeException());
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllOrganizationDataInSingleCallResult =  masterDataHelper.addAllOrganizationDataInSingleCall(shipmentDetailsResponse, null);
        ResponseEntity<IRunnerResponse> getResult = actualAddAllOrganizationDataInSingleCallResult.get();

        assertNull(getResult);
    }

    @Test
    void testAddAllTenantDataInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkTenantsRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInTenantsList(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllTenantDataInSingleCallResult = masterDataHelper
                .addAllTenantDataInSingleCall(shipmentDetailsResponse, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInTenantsList(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllTenantDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllTenantDataInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkTenantsRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInTenantsList(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder().build();

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllTenantDataInSingleCallResult = masterDataHelper
                .addAllTenantDataInSingleCall(shipmentDetailsResponse, new HashMap<>());

        // Assert
        verify(masterDataUtils, atLeast(1)).createInBulkTenantsRequest(Mockito.<IRunnerResponse>any(),
                Mockito.<Class<Object>>any(), isA(Map.class), Mockito.<String>any(), isA(Map.class));
        verify(masterDataUtils).fetchInTenantsList(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllTenantDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllTenantDataInSingleCall3() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkTenantsRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInTenantsList(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setAdditionalDetails(AdditionalDetailResponse.builder().build());

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllTenantDataInSingleCallResult = masterDataHelper
                .addAllTenantDataInSingleCall(shipmentDetailsResponse, null);

        // Assert
        verify(masterDataUtils).fetchInTenantsList(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllTenantDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllCurrencyDataInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkCurrencyRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInCurrencyList(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllCurrencyDataInSingleCallResult = masterDataHelper
                .addAllCurrencyDataInSingleCall(shipmentDetailsResponse, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInCurrencyList(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllCurrencyDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllCurrencyDataInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkCurrencyRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInCurrencyList(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllCurrencyDataInSingleCallResult = masterDataHelper
                .addAllCurrencyDataInSingleCall(shipmentDetailsResponse, null);

        // Assert
        verify(masterDataUtils).fetchInCurrencyList(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllCurrencyDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllCarrierDataInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkCarriers(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setCarrierDetails(CarrierDetailResponse.builder().build());
        shipmentDetailsResponse.setRoutingsList(List.of(RoutingsResponse.builder().build()));

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllCarrierDataInSingleCallResult = masterDataHelper
                .addAllCarrierDataInSingleCall(shipmentDetailsResponse, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInBulkCarriers(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllCarrierDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllCarrierDataInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkCarriers(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setCarrierDetails(CarrierDetailResponse.builder().build());
        shipmentDetailsResponse.setRoutingsList(List.of(RoutingsResponse.builder().build()));

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllCarrierDataInSingleCallResult = masterDataHelper
                .addAllCarrierDataInSingleCall(shipmentDetailsResponse, null);

        // Assert
        verify(masterDataUtils).fetchInBulkCarriers(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllCarrierDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllCommodityTypesInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkCommodityTypes(Mockito.<List<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setPackingList(List.of(new PackingResponse()));
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllCommodityTypesInSingleCallResult = masterDataHelper
                .addAllCommodityTypesInSingleCall(shipmentDetailsResponse, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInBulkCommodityTypes(isA(List.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllCommodityTypesInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllCommodityTypesInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkCommodityTypes(Mockito.<List<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());

        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder().build();
        shipmentDetailsResponse.setContainersList(Set.of(new ContainerResponse()));

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllCommodityTypesInSingleCallResult = masterDataHelper
                .addAllCommodityTypesInSingleCall(shipmentDetailsResponse, null);

        // Assert
        verify(masterDataUtils).fetchInBulkCommodityTypes(isA(List.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllCommodityTypesInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllWarehouseDataInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInWareHousesList(Mockito.<List<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setAdditionalDetails(AdditionalDetailResponse.builder().build());
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllWarehouseDataInSingleCallResult = masterDataHelper
                .addAllWarehouseDataInSingleCall(shipmentDetailsResponse, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInWareHousesList(isA(List.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllWarehouseDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllWarehouseDataInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInWareHousesList(Mockito.<List<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setAdditionalDetails(AdditionalDetailResponse.builder().build());
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllWarehouseDataInSingleCallResult = masterDataHelper
                .addAllWarehouseDataInSingleCall(shipmentDetailsResponse, null);

        // Assert
        verify(masterDataUtils).fetchInWareHousesList(isA(List.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllWarehouseDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllActivityDataInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInActivityMasterList(Mockito.<List<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setAdditionalDetails(AdditionalDetailResponse.builder().build());
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllActivityDataInSingleCallResult = masterDataHelper
                .addAllActivityDataInSingleCall(shipmentDetailsResponse, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInActivityMasterList(isA(List.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllActivityDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllActivityDataInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInActivityMasterList(Mockito.<List<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setAdditionalDetails(AdditionalDetailResponse.builder().build());
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllActivityDataInSingleCallResult = masterDataHelper
                .addAllActivityDataInSingleCall(shipmentDetailsResponse, null);

        // Assert
        verify(masterDataUtils).fetchInActivityMasterList(isA(List.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllActivityDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllSalesAgentInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInSalesAgentList(Mockito.<List<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setAdditionalDetails(AdditionalDetailResponse.builder().build());
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllSalesAgentInSingleCallResult = masterDataHelper
                .addAllSalesAgentInSingleCall(shipmentDetailsResponse, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInSalesAgentList(isA(List.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllSalesAgentInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllSalesAgentInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInSalesAgentList(Mockito.<List<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setAdditionalDetails(AdditionalDetailResponse.builder().build());
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllSalesAgentInSingleCallResult = masterDataHelper
                .addAllSalesAgentInSingleCall(shipmentDetailsResponse, null);

        // Assert
        verify(masterDataUtils).fetchInSalesAgentList(isA(List.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllSalesAgentInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllContainerTypesInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkContainerTypes(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setAdditionalDetails(AdditionalDetailResponse.builder().build());
        shipmentDetailsResponse.setContainersList(Set.of(new ContainerResponse()));
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllContainerTypesInSingleCallResult = masterDataHelper
                .addAllContainerTypesInSingleCall(shipmentDetailsResponse, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInBulkContainerTypes(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllContainerTypesInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllContainerTypesInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkContainerTypes(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setAdditionalDetails(AdditionalDetailResponse.builder().build());
        shipmentDetailsResponse.setContainersList(Set.of(new ContainerResponse()));
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllContainerTypesInSingleCallResult = masterDataHelper
                .addAllContainerTypesInSingleCall(shipmentDetailsResponse, null);

        // Assert
        verify(masterDataUtils).fetchInBulkContainerTypes(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllContainerTypesInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllVesselDataInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkVessels(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setAdditionalDetails(AdditionalDetailResponse.builder().build());
        shipmentDetailsResponse.setContainersList(Set.of(new ContainerResponse()));
        shipmentDetailsResponse.setRoutingsList(List.of(new RoutingsResponse()));
        shipmentDetailsResponse.setBookingCarriagesList(List.of(new BookingCarriageResponse()));
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllVesselDataInSingleCallResult = masterDataHelper
                .addAllVesselDataInSingleCall(shipmentDetailsResponse, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInBulkVessels(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllVesselDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllVesselDataInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkVessels(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setAdditionalDetails(AdditionalDetailResponse.builder().build());
        shipmentDetailsResponse.setContainersList(Set.of(new ContainerResponse()));
        shipmentDetailsResponse.setRoutingsList(List.of(new RoutingsResponse()));
        shipmentDetailsResponse.setBookingCarriagesList(List.of(new BookingCarriageResponse()));
        shipmentDetailsResponse.setCarrierDetails(CarrierDetailResponse.builder().build());
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllVesselDataInSingleCallResult = masterDataHelper
                .addAllVesselDataInSingleCall(shipmentDetailsResponse, null);

        // Assert
        verify(masterDataUtils).fetchInBulkVessels(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllVesselDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllDGSubstanceDataInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInDGSubstanceList(Mockito.<List<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setAdditionalDetails(AdditionalDetailResponse.builder().build());
        shipmentDetailsResponse.setPackingList(List.of(new PackingResponse()));
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllDGSubstanceDataInSingleCallResult = masterDataHelper
                .addAllDGSubstanceDataInSingleCall(shipmentDetailsResponse, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInDGSubstanceList(isA(List.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllDGSubstanceDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testSetTruckDriverDetailsData() {
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setTruckDriverDetails(List.of(
                TruckDriverDetailsResponse.builder().containerId(1L).build(),
                TruckDriverDetailsResponse.builder().build(),
                TruckDriverDetailsResponse.builder().containerId(2L).build()
                ));
        Map<Long, ContainerResponse> map = new HashMap<>();
        map.put(1L, new ContainerResponse());
        masterDataHelper.setTruckDriverDetailsData(shipmentDetailsResponse, map);
        assertNotNull(map);
    }

    @Test
    void testSetTruckDriverDetailsData2() {
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        Map<Long, ContainerResponse> map = new HashMap<>();
        map.put(1L, new ContainerResponse());
        masterDataHelper.setTruckDriverDetailsData(shipmentDetailsResponse, map);
        assertNotNull(map);
    }

    @Test
    void testSetTruckDriverDetailsData3() {
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setTruckDriverDetails(new ArrayList<>());
        Map<Long, ContainerResponse> map = new HashMap<>();
        map.put(1L, new ContainerResponse());
        masterDataHelper.setTruckDriverDetailsData(shipmentDetailsResponse, map);
        assertNotNull(map);
    }

    @Test
    void testSetContainersPacksAutoUpdateData() {
        // Arrange
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();

        // Act
        masterDataHelper.setContainersPacksAutoUpdateData(shipmentDetailsResponse, new HashMap<>());

        assertFalse(shipmentDetailsResponse.getCargoFinanceBooking());
        assertFalse(shipmentDetailsResponse.getIntraBranch());
        assertFalse(shipmentDetailsResponse.getIsShipmentReadOnly());
    }

    @Test
    void testSetContainersPacksAutoUpdateData2() {
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setContainerAutoWeightVolumeUpdate(true);
        List<PackingResponse> packingResponse = new ArrayList<>();
        packingResponse.add(new PackingResponse());
        PackingResponse packResponse = new PackingResponse();
        packResponse.setContainerId(1L);
        packResponse.setHandlingInfo("abc");
        packResponse.setGoodsDescription("def");
        packingResponse.add(packResponse);
        PackingResponse packResponse2 = new PackingResponse();
        packResponse2.setContainerId(1L);
        packResponse2.setHandlingInfo("abc");
        packResponse2.setGoodsDescription("def");
        packingResponse.add(packResponse2);
        shipmentDetailsResponse.setPackingList(packingResponse);
        Set<ContainerResponse> containerResponses = new HashSet<>();
        ContainerResponse containerResponse = new ContainerResponse();
        containerResponse.setId(1L);
        containerResponses.add(containerResponse);
        containerResponses.add(new ContainerResponse());
        shipmentDetailsResponse.setContainersList(containerResponses);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).build());
        Map<Long, ContainerResponse> map = new HashMap<>();
        map.put(1L, containerResponse);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        masterDataHelper.setContainersPacksAutoUpdateData(shipmentDetailsResponse, map);
        assertNotNull(map);
    }

    @Test
    void testSetContainersPacksAutoUpdateData3() {
        // Arrange
        ShipmentDetailsResponse shipmentDetailsResponse = mock(ShipmentDetailsResponse.class);
        when(shipmentDetailsResponse.getContainerAutoWeightVolumeUpdate()).thenReturn(false);
        when(shipmentDetailsResponse.getContainersList()).thenReturn(new HashSet<>());
        when(shipmentDetailsResponse.getPackingList()).thenReturn(new ArrayList<>());

        // Act
        masterDataHelper.setContainersPacksAutoUpdateData(shipmentDetailsResponse, new HashMap<>());

        verify(shipmentDetailsResponse, atLeast(1)).getContainerAutoWeightVolumeUpdate();
        verify(shipmentDetailsResponse).getContainersList();
        verify(shipmentDetailsResponse).getPackingList();
    }

    @Test
    void testSetContainersPacksAutoUpdateData4() {
        // Arrange
        ContainerResponse containerResponse = new ContainerResponse();
        containerResponse.setAchievedVolume(new BigDecimal("2.3"));
        containerResponse.setAchievedVolumeUnit("42");
        containerResponse.setAchievedWeight(new BigDecimal("2.3"));
        containerResponse.setAchievedWeightUnit("42");
        containerResponse.setAllocatedVolume(new BigDecimal("2.3"));
        containerResponse.setAllocatedVolumeUnit("42");
        containerResponse.setAllocatedWeight(new BigDecimal("2.3"));
        containerResponse.setAllocatedWeightUnit("42");
        containerResponse.setAllocationDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        containerResponse.setBookingId(3L);
        containerResponse.setCarrierSealNumber("com.dpw.runner.shipment.services.dto.response.ContainerResponse");
        containerResponse.setChargeable(new BigDecimal("2.3"));
        containerResponse.setChargeableUnit("42");
        containerResponse.setCommodityCode("42");
        containerResponse.setCommodityGroup("42");
        containerResponse.setCommodityTypeData(new HashMap<>());
        containerResponse.setConsolidationId(3L);
        containerResponse.setContainerCode("42");
        containerResponse.setContainerCodeData(new HashMap<>());
        containerResponse.setContainerComments("42");
        containerResponse.setContainerCount(0L);
        containerResponse.setContainerNumber("com.dpw.runner.shipment.services.dto.response.ContainerResponse");
        containerResponse.setContainerStuffingLocation("42");
        containerResponse.setContractEnforcedQuantityLimit(-1L);
        containerResponse.setCustomsReleaseCode("com.dpw.runner.shipment.services.dto.response.ContainerResponse");
        containerResponse.setCustomsSealNumber("com.dpw.runner.shipment.services.dto.response.ContainerResponse");
        PartiesResponse.PartiesResponseBuilder entityTypeResult = PartiesResponse.builder()
                .entityId(1L)
                .entityType("Entity Type");
        PartiesResponse.PartiesResponseBuilder orgCodeResult = entityTypeResult.guid(UUID.randomUUID())
                .id(1L)
                .orgCode("Org Code");
        PartiesResponse deliveryAddress = orgCodeResult.orgData(new HashMap<>()).tenantId(1).type("Type").build();
        containerResponse.setDeliveryAddress(deliveryAddress);
        containerResponse.setDescriptionOfGoods("42");
        containerResponse.setDgClass("42");
        containerResponse.setEventsList(new ArrayList<>());
        containerResponse.setExtraParams("42");
        containerResponse.setGrossVolume(new BigDecimal("2.3"));
        containerResponse.setGrossVolumeUnit("42");
        containerResponse.setGrossWeight(new BigDecimal("2.3"));
        containerResponse.setGrossWeightUnit("42");
        containerResponse.setGuid(UUID.randomUUID());
        containerResponse.setHandlingInfo("42");
        containerResponse.setHazardous(false);
        containerResponse.setHazardousUn("42");
        containerResponse.setHblDeliveryMode("42");
        containerResponse.setHblNumber("com.dpw.runner.shipment.services.dto.response.ContainerResponse");
        containerResponse.setHsCode("42");
        containerResponse.setId(3L);
        containerResponse.setInnerPackageMeasurementUnit("com.dpw.runner.shipment.services.dto.response.ContainerResponse");
        containerResponse.setInnerPackageNumber("Inner Package Number");
        containerResponse.setInnerPackageType("com.dpw.runner.shipment.services.dto.response.ContainerResponse");
        containerResponse.setInvoiceCurrency("Invoice Currency");
        containerResponse.setInvoiceNumber("com.dpw.runner.shipment.services.dto.response.ContainerResponse");
        containerResponse.setInvoiceValue(new BigDecimal("2.3"));
        containerResponse.setIsAttached(false);
        containerResponse.setIsContractEnforced(false);
        containerResponse.setIsEmpty(false);
        containerResponse.setIsOwnContainer(false);
        containerResponse.setIsPart(false);
        containerResponse.setIsReefer(false);
        containerResponse.setIsShipperOwned(false);
        containerResponse.setIsTemperatureMaintained(false);
        containerResponse.setLoggingId(3L);
        containerResponse.setMarksNums("42");
        containerResponse.setMasterData(new HashMap<>());
        containerResponse.setMaxTemp(new BigDecimal("2.3"));
        containerResponse.setMaxTempUnit("42");
        containerResponse.setMeasurement(new BigDecimal("2.3"));
        containerResponse.setMeasurementUnit("42");
        containerResponse.setMinTemp(new BigDecimal("2.3"));
        containerResponse.setMinTempUnit("42");
        containerResponse.setNetWeight(new BigDecimal("2.3"));
        containerResponse.setNetWeightUnit("42");
        containerResponse.setOwnType("42");
        containerResponse.setPackageBreadth(new BigDecimal("2.3"));
        containerResponse.setPackageHeight(new BigDecimal("2.3"));
        containerResponse.setPackageLength(new BigDecimal("2.3"));
        containerResponse.setPacks("42");
        containerResponse.setPacksType("42");
        containerResponse.setPacrNumber("com.dpw.runner.shipment.services.dto.response.ContainerResponse");
        PartiesResponse.PartiesResponseBuilder entityTypeResult2 = PartiesResponse.builder()
                .entityId(1L)
                .entityType("Entity Type");
        PartiesResponse.PartiesResponseBuilder orgCodeResult2 = entityTypeResult2.guid(UUID.randomUUID())
                .id(1L)
                .orgCode("Org Code");
        PartiesResponse pickupAddress = orgCodeResult2.orgData(new HashMap<>()).tenantId(1).type("Type").build();
        containerResponse.setPickupAddress(pickupAddress);
        containerResponse.setRemarks("42");
        containerResponse.setSealNumber("com.dpw.runner.shipment.services.dto.response.ContainerResponse");
        containerResponse.setSerialNumber("com.dpw.runner.shipment.services.dto.response.ContainerResponse");
        containerResponse.setShipperSealNumber("com.dpw.runner.shipment.services.dto.response.ContainerResponse");
        containerResponse.setStatus(ContainerStatus.DISCHARGED_FROM_BARGE);
        containerResponse.setTareWeight(new BigDecimal("2.3"));
        containerResponse.setTareWeightUnit("42");
        containerResponse.setTenantId(3);
        containerResponse.setTerminalOperatorSealNumber("com.dpw.runner.shipment.services.dto.response.ContainerResponse");
        containerResponse.setTextFieldData(new HashMap<>());
        containerResponse.setTransportMode("42");
        containerResponse.setUnlocationData(new HashMap<>());
        containerResponse.setVeterinarySealNumber("com.dpw.runner.shipment.services.dto.response.ContainerResponse");
        containerResponse.setVolumeUtilization("42");
        containerResponse.setWeightUtilization("42");

        Set<ContainerResponse> containerResponseList = new HashSet<>();
        containerResponseList.add(containerResponse);
        ShipmentDetailsResponse shipmentDetailsResponse = mock(ShipmentDetailsResponse.class);
        when(shipmentDetailsResponse.getContainerAutoWeightVolumeUpdate()).thenReturn(false);
        when(shipmentDetailsResponse.getContainersList()).thenReturn(containerResponseList);
        when(shipmentDetailsResponse.getPackingList()).thenReturn(new ArrayList<>());

        // Act
        masterDataHelper.setContainersPacksAutoUpdateData(shipmentDetailsResponse, new HashMap<>());

        // Assert
        verify(shipmentDetailsResponse, atLeast(1)).getContainerAutoWeightVolumeUpdate();
        verify(shipmentDetailsResponse).getContainersList();
        verify(shipmentDetailsResponse).getPackingList();
    }

    @Test
    void testSetContainersPacksAutoUpdateData5() {
        // Arrange
        PackingResponse packingResponse = new PackingResponse();
        packingResponse.setBookingId(3L);
        packingResponse.setChargeable(new BigDecimal("2.3"));
        packingResponse.setChargeableUnit("42");
        packingResponse.setCommodity("42");
        packingResponse.setCommodityGroup("42");
        packingResponse.setCommodityTypeData(new HashMap<>());
        packingResponse.setConsolidationId(3L);
        packingResponse.setContainerDesc("42");
        packingResponse.setContainerId(3L);
        packingResponse.setContainerNumber("com.dpw.runner.shipment.services.dto.response.PackingResponse");
        packingResponse.setContractEnforcedQuantityLimit(-1L);
        packingResponse.setCountryCode("US");
        packingResponse.setCustomsReleaseCode("com.dpw.runner.shipment.services.dto.response.PackingResponse");
        packingResponse.setDGClass("42");
        packingResponse.setDGGoodsId(3);
        packingResponse.setDGSubstanceId(3);
        packingResponse.setDgClassAir("42");
        packingResponse.setDgClassAirDescription("42");
        packingResponse.setFlashPoint("42");
        packingResponse.setGoodsDescription("42");
        packingResponse.setGuid(UUID.randomUUID());
        packingResponse.setHSCode("42");
        packingResponse.setHandlingInfo("42");
        packingResponse.setHazardous(false);
        packingResponse.setHeight(new BigDecimal("2.3"));
        packingResponse.setHeightUnit("42");
        packingResponse.setId(3L);
        packingResponse.setInnerPackageNumber("Inner Package Number");
        packingResponse.setInnerPackageType("com.dpw.runner.shipment.services.dto.response.PackingResponse");
        packingResponse.setInnerPacksCount(0L);
        packingResponse.setInnerPacksId(3L);
        packingResponse.setInspections("42");
        packingResponse.setIsContractEnforced(false);
        packingResponse.setIsDimension(false);
        packingResponse.setIsTemperatureControlled(false);
        packingResponse.setLength(new BigDecimal("2.3"));
        packingResponse.setLengthUnit("42");
        packingResponse.setMarksnNums("42");
        packingResponse.setMasterData(new HashMap<>());
        packingResponse.setMaxTemp(new BigDecimal("2.3"));
        packingResponse.setMaxTempUnit("42");
        packingResponse.setMinTemp(new BigDecimal("2.3"));
        packingResponse.setMinTempUnit("42");
        packingResponse.setNetWeight(new BigDecimal("2.3"));
        packingResponse.setNetWeightUnit("42");
        packingResponse.setOrigin("42");
        packingResponse.setPackingOrder("42");
        packingResponse.setPacks("42");
        packingResponse.setPacksType("42");
        packingResponse.setReferenceNumber("com.dpw.runner.shipment.services.dto.response.PackingResponse");
        packingResponse.setShipmentId(3L);
        packingResponse.setShipmentNumber("com.dpw.runner.shipment.services.dto.response.PackingResponse");
        packingResponse.setTransportMode("42");
        packingResponse.setUNDGContact("42");
        packingResponse.setUnNumberAir("com.dpw.runner.shipment.services.dto.response.PackingResponse");
        packingResponse.setUnlocationData(new HashMap<>());
        packingResponse.setVinNumber("com.dpw.runner.shipment.services.dto.response.PackingResponse");
        packingResponse.setVolume(new BigDecimal("2.3"));
        packingResponse.setVolumeUnit("42");
        packingResponse.setVolumeWeight(new BigDecimal("2.3"));
        packingResponse.setVolumeWeightUnit("42");
        packingResponse.setWeight(new BigDecimal("2.3"));
        packingResponse.setWeightUnit("42");
        packingResponse.setWidth(new BigDecimal("2.3"));
        packingResponse.setWidthUnit("42");

        ArrayList<PackingResponse> packingResponseList = new ArrayList<>();
        packingResponseList.add(packingResponse);
        ShipmentDetailsResponse shipmentDetailsResponse = mock(ShipmentDetailsResponse.class);
        when(shipmentDetailsResponse.getContainerAutoWeightVolumeUpdate()).thenReturn(false);
        when(shipmentDetailsResponse.getContainersList()).thenReturn(new HashSet<>());
        when(shipmentDetailsResponse.getPackingList()).thenReturn(packingResponseList);

        // Act
        masterDataHelper.setContainersPacksAutoUpdateData(shipmentDetailsResponse, new HashMap<>());

        // Assert that nothing has changed
        verify(shipmentDetailsResponse, atLeast(1)).getContainerAutoWeightVolumeUpdate();
        verify(shipmentDetailsResponse).getContainersList();
        verify(shipmentDetailsResponse).getPackingList();
    }
}
