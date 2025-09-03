package com.dpw.runner.shipment.services.utils.v3;


import com.dpw.runner.shipment.services.dto.response.ServiceDetailsResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class ServiceDetailsV3UtilTest {

    @Mock
    private CommonUtils commonUtils;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private MasterDataKeyUtils masterDataKeyUtils;

    @InjectMocks
    private ServiceDetailsV3Util serviceDetailsV3Util;

    private ServiceDetailsResponse sampleServiceDetailsResponse;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        sampleServiceDetailsResponse = new ServiceDetailsResponse();
        sampleServiceDetailsResponse.setId(1L);
        sampleServiceDetailsResponse.setServiceType("CLN");
    }

    @Test
    void testAddAllMasterDataInSingleCallList_success() {
        List<ServiceDetailsResponse> serviceDetailsResponses = List.of(sampleServiceDetailsResponse);
        List<MasterListRequest> mockRequests = List.of(new MasterListRequest());

        when(masterDataUtils.createInBulkMasterListRequest(any(), any(), any(), any(), any()))
                .thenReturn(mockRequests);

        when(masterDataUtils.fetchInBulkMasterList(any()))
                .thenReturn(Map.of("key", new EntityTransferMasterLists()));

        doNothing().when(masterDataUtils).pushToCache(any(), any(), any(), any(), any());

        doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), anyString(), any(), any());

        assertDoesNotThrow(() -> serviceDetailsV3Util.addAllMasterDataInSingleCallList(serviceDetailsResponses, new HashMap<>()));
    }

    @Test
    void testAddAllUnlocationInSingleCallList_success() {
        List<ServiceDetailsResponse> serviceDetailsResponses = List.of(sampleServiceDetailsResponse);

        when(masterDataUtils.createInBulkUnLocationsRequest(any(), any(), any(), any(), any()))
                .thenReturn(List.of("loc1"));

        when(masterDataUtils.fetchInBulkUnlocations(anySet(), anyString()))
                .thenReturn(Map.of("loc1", new EntityTransferUnLocations()));

        doNothing().when(masterDataUtils).pushToCache(any(), any(), any(), any(), any());

        doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), anyString(), any(), any());

        assertDoesNotThrow(() -> serviceDetailsV3Util.addAllUnlocationInSingleCallList(serviceDetailsResponses, new HashMap<>()));
    }

    @Test
    void testAddAllMasterDataInSingleCallList_shouldHandleException() {
        List<ServiceDetailsResponse> serviceDetailsResponses = List.of(sampleServiceDetailsResponse);
        List<MasterListRequest> mockRequests = List.of(new MasterListRequest());

        when(masterDataUtils.createInBulkMasterListRequest(any(), any(), any(), any(), any()))
                .thenReturn(mockRequests);

        when(masterDataUtils.fetchInBulkMasterList(any()))
                .thenReturn(Map.of("key", new EntityTransferMasterLists()));

        doNothing().when(masterDataUtils).pushToCache(any(), any(), any(), any(), any());

        when(masterDataUtils.setMasterData(any(), any(), any())).thenThrow(new RuntimeException("Simulated Exception"));

        assertDoesNotThrow(() -> serviceDetailsV3Util.addAllMasterDataInSingleCallList(serviceDetailsResponses, null));

        verify(masterDataUtils, times(1)).createInBulkMasterListRequest(any(), any(), any(), any(), any());
    }

    @Test
    void testAddAllUnlocationInSingleCallList_shouldHandleException() {
        List<ServiceDetailsResponse> serviceDetailsResponses = List.of(sampleServiceDetailsResponse);

        when(masterDataUtils.createInBulkUnLocationsRequest(any(), any(), any(), any(), any()))
                .thenReturn(List.of("loc1"));

        when(masterDataUtils.fetchInBulkUnlocations(anySet(), anyString()))
                .thenReturn(Map.of("loc1", new EntityTransferUnLocations()));

        doNothing().when(masterDataUtils).pushToCache(any(), any(), any(), any(), any());

        when(masterDataUtils.setMasterData(any(), any(), any())).thenThrow(new RuntimeException("Simulated Exception"));

        assertDoesNotThrow(() -> serviceDetailsV3Util.addAllUnlocationInSingleCallList(serviceDetailsResponses, null));

        verify(masterDataUtils, times(1)).createInBulkUnLocationsRequest(any(), any(), any(), any(), any());
    }

    @Test
    void testAddAllMasterDataInSingleCall_Success() {
        Map<String, Object> masterDataResponse = new HashMap<>();
        List<MasterListRequest> dummyRequests = List.of(new MasterListRequest());

        when(masterDataUtils.createInBulkMasterListRequest(any(), any(), any(), any(), any()))
                .thenReturn(dummyRequests);

        when(masterDataUtils.fetchInBulkMasterList(any()))
                .thenReturn(Map.of("key", new EntityTransferMasterLists()));

        doNothing().when(masterDataUtils).pushToCache(any(), any(), any(), any(), any());

        doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), anyString(), any(), any());

        CompletableFuture<Map<String, EntityTransferMasterLists>> result = serviceDetailsV3Util.addAllMasterDataInSingleCall(sampleServiceDetailsResponse, masterDataResponse);
        assertNotNull(result);
    }

    @Test
    void testAddAllMasterDataInSingleCall_Exception() {
        List<MasterListRequest> dummyRequests = List.of(new MasterListRequest());

        when(masterDataUtils.createInBulkMasterListRequest(any(), any(), any(), any(), any()))
                .thenReturn(dummyRequests);

        when(masterDataUtils.fetchInBulkMasterList(any()))
                .thenReturn(Map.of("key", new EntityTransferMasterLists()));

        doNothing().when(masterDataUtils).pushToCache(any(), any(), any(), any(), any());

        when(masterDataUtils.setMasterData(any(), any(), any())).thenThrow(new RuntimeException("Simulated Exception"));

        CompletableFuture<Map<String, EntityTransferMasterLists>> result = serviceDetailsV3Util.addAllMasterDataInSingleCall(sampleServiceDetailsResponse, null);
        assertNull(result.join());
    }

    @Test
    void testAddAllUnlocationDataInSingleCall_Success() {
        Map<String, Object> masterDataResponse = new HashMap<>();
        List<String> locationCodes = List.of("LOC123");
        Map<String, EntityTransferUnLocations> responseMap = Map.of("LOC123", new EntityTransferUnLocations());

        when(masterDataUtils.createInBulkUnLocationsRequest(any(), any(), any(), any(), any())).thenReturn(locationCodes);
        when(masterDataUtils.fetchInBulkUnlocations(any(), anyString())).thenReturn(responseMap);
        doNothing().when(masterDataUtils).pushToCache(any(), anyString(), any(), any(), any());
        doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), anyString(), any(), any());

        CompletableFuture<Map<String, EntityTransferUnLocations>> result = serviceDetailsV3Util.addAllUnlocationDataInSingleCall(sampleServiceDetailsResponse, masterDataResponse);
        assertNotNull(result);
    }

    @Test
    void testAddAllUnlocationDataInSingleCall_Exception() {
        List<String> locationCodes = List.of("LOC123");
        Map<String, EntityTransferUnLocations> responseMap = Map.of("LOC123", new EntityTransferUnLocations());

        when(masterDataUtils.createInBulkUnLocationsRequest(any(), any(), any(), any(), any())).thenReturn(locationCodes);
        when(masterDataUtils.fetchInBulkUnlocations(any(), anyString())).thenReturn(responseMap);
        doNothing().when(masterDataUtils).pushToCache(any(), anyString(), any(), any(), any());
        when(masterDataUtils.setMasterData(any(), any(), any())).thenThrow(new RuntimeException("Simulated Exception"));

        CompletableFuture<Map<String, EntityTransferUnLocations>> result = serviceDetailsV3Util.addAllUnlocationDataInSingleCall(sampleServiceDetailsResponse, null);
        assertNotNull(result);
    }
}
