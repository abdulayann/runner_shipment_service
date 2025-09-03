package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.anyMap;
import static org.mockito.Mockito.anySet;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.isNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class RoutingV3UtilTest {
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private CommonUtils commonUtils;
    @Mock
    private MasterDataKeyUtils masterDataKeyUtils;
    @InjectMocks
    private RoutingV3Util routingV3Util;

    @Test
    void addAllMasterDataInSingleCallList() {
        List<RoutingsResponse> routingListResponse = new ArrayList<>();
        routingListResponse.add(RoutingsResponse.builder().id(1L).build());
        Map<String, Object> masterDataMap = new HashMap<>();
        List<MasterListRequest> masterListRequestList = new ArrayList<>();
        masterListRequestList.add(MasterListRequest.builder().build());
        Map<String, String> masterData = new HashMap<>();
        when(masterDataUtils.createInBulkMasterListRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(masterListRequestList);
        routingV3Util.addAllMasterDataInSingleCallList(routingListResponse, masterDataMap);
        verify(masterDataKeyUtils).setMasterDataValue(anyMap(), anyString(), anyMap(), anyMap());
    }

    @Test
    void addAllMasterDataInSingleCallList1() {
        List<RoutingsResponse> routingListResponse = new ArrayList<>();
        routingListResponse.add(RoutingsResponse.builder().id(1L).build());
        List<MasterListRequest> masterListRequestList = new ArrayList<>();
        masterListRequestList.add(MasterListRequest.builder().build());
        Map<String, String> masterData = new HashMap<>();
        when(masterDataUtils.createInBulkMasterListRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(masterListRequestList);
        when(masterDataUtils.setMasterData(isNull(), anyString(), anyMap())).thenReturn(masterData);
        routingV3Util.addAllMasterDataInSingleCallList(routingListResponse, null);
        verify(masterDataUtils).setMasterData(isNull(), anyString(), anyMap());
    }

    @Test
    void addAllMasterDataInSingleCallList2() {
        List<RoutingsResponse> routingListResponse = new ArrayList<>();
        routingListResponse.add(RoutingsResponse.builder().id(1L).build());
        List<MasterListRequest> masterListRequestList = new ArrayList<>();
        masterListRequestList.add(MasterListRequest.builder().build());
        when(masterDataUtils.createInBulkMasterListRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(masterListRequestList);
        when(masterDataUtils.setMasterData(isNull(), anyString(), anyMap())).thenThrow(RuntimeException.class);
        routingV3Util.addAllMasterDataInSingleCallList(routingListResponse, null);
        verify(masterDataUtils).pushToCache(anyMap(), anyString(), anySet(), any(), anyMap());
    }


    @Test
    void addAllUnlocationInSingleCallList() {
        List<RoutingsResponse> routingListResponse = new ArrayList<>();
        routingListResponse.add(RoutingsResponse.builder().id(1L).build());
        Map<String, Object> masterDataMap = new HashMap<>();
        when(masterDataUtils.createInBulkUnLocationsRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(List.of(""));
        routingV3Util.addAllUnlocationInSingleCallList(routingListResponse, masterDataMap);
        verify(masterDataKeyUtils).setMasterDataValue(anyMap(), anyString(), anyMap(), anyMap());
    }

    @Test
    void addAllUnlocationInSingleCallList1() {
        List<RoutingsResponse> routingListResponse = new ArrayList<>();
        routingListResponse.add(RoutingsResponse.builder().id(1L).build());
        Map<String, String> masterData = new HashMap<>();
        when(masterDataUtils.createInBulkUnLocationsRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(List.of(""));
        when(masterDataUtils.setMasterData(isNull(), anyString(), anyMap())).thenReturn(masterData);
        routingV3Util.addAllUnlocationInSingleCallList(routingListResponse, null);
        verify(masterDataUtils).setMasterData(isNull(), anyString(), anyMap());
    }

    @Test
    void addAllUnlocationInSingleCallList2() {
        List<RoutingsResponse> routingListResponse = new ArrayList<>();
        routingListResponse.add(RoutingsResponse.builder().id(1L).build());
        when(masterDataUtils.createInBulkUnLocationsRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(List.of(""));
        when(masterDataUtils.setMasterData(isNull(), anyString(), anyMap())).thenThrow(RuntimeException.class);
        routingV3Util.addAllUnlocationInSingleCallList(routingListResponse, null);
        verify(masterDataUtils).pushToCache(anyMap(), anyString(), anySet(), any(), anyMap());
    }

    @Test
    void addAllVesselInSingleCallList() {
        List<RoutingsResponse> routingListResponse = new ArrayList<>();
        routingListResponse.add(RoutingsResponse.builder().id(1L).build());
        Map<String, Object> masterDataMap = new HashMap<>();
        when(masterDataUtils.createInBulkVesselsRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(List.of(""));
        routingV3Util.addAllVesselInSingleCallList(routingListResponse, masterDataMap);
        verify(masterDataKeyUtils).setMasterDataValue(anyMap(), anyString(), anyMap(), anyMap());
    }

    @Test
    void addAllVesselInSingleCallList1() {
        List<RoutingsResponse> routingListResponse = new ArrayList<>();
        routingListResponse.add(RoutingsResponse.builder().id(1L).build());
        Map<String, String> masterData = new HashMap<>();
        when(masterDataUtils.createInBulkVesselsRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(List.of(""));
        when(masterDataUtils.setMasterData(isNull(), anyString(), anyMap())).thenReturn(masterData);
        routingV3Util.addAllVesselInSingleCallList(routingListResponse, null);
        verify(masterDataUtils).setMasterData(isNull(), anyString(), anyMap());
    }

    @Test
    void addAllVesselInSingleCallList2() {
        List<RoutingsResponse> routingListResponse = new ArrayList<>();
        routingListResponse.add(RoutingsResponse.builder().id(1L).build());
        when(masterDataUtils.createInBulkVesselsRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(List.of(""));
        when(masterDataUtils.setMasterData(isNull(), anyString(), anyMap())).thenThrow(RuntimeException.class);
        routingV3Util.addAllVesselInSingleCallList(routingListResponse, null);
        verify(masterDataUtils).pushToCache(anyMap(), anyString(), anySet(), any(), anyMap());
    }


    @Test
    void addAllMasterDataInSingleCall() {
        RoutingsResponse routingResponse = RoutingsResponse.builder().build();
        Map<String, Object> masterDataMap = new HashMap<>();
        List<MasterListRequest> masterListRequestList = new ArrayList<>();
        masterListRequestList.add(MasterListRequest.builder().build());
        when(masterDataUtils.createInBulkMasterListRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(masterListRequestList);
        routingV3Util.addAllMasterDataInSingleCall(routingResponse, masterDataMap);
        verify(masterDataKeyUtils).setMasterDataValue(anyMap(), anyString(), anyMap(), anyMap());
    }

    @Test
    void addAllMasterDataInSingleCall1() {
        RoutingsResponse routingResponse = RoutingsResponse.builder().build();
        List<MasterListRequest> masterListRequestList = new ArrayList<>();
        masterListRequestList.add(MasterListRequest.builder().build());
        Map<String, String> masterData = new HashMap<>();
        when(masterDataUtils.createInBulkMasterListRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(masterListRequestList);
        when(masterDataUtils.setMasterData(isNull(), anyString(), anyBoolean(), anyMap())).thenReturn(masterData);
        routingV3Util.addAllMasterDataInSingleCall(routingResponse, null);
        verify(masterDataUtils).setMasterData(isNull(), anyString(), anyBoolean(), anyMap());
    }

    @Test
    void addAllMasterDataInSingleCall2() {
        RoutingsResponse routingResponse = RoutingsResponse.builder().build();
        List<MasterListRequest> masterListRequestList = new ArrayList<>();
        masterListRequestList.add(MasterListRequest.builder().build());
        when(masterDataUtils.createInBulkMasterListRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(masterListRequestList);
        when(masterDataUtils.setMasterData(isNull(), anyString(), anyBoolean(), anyMap())).thenThrow(RuntimeException.class);
        routingV3Util.addAllMasterDataInSingleCall(routingResponse, null);
        verify(masterDataUtils).pushToCache(anyMap(), anyString(), anySet(), any(), anyMap());
    }

    @Test
    void addAllUnlocationInSingleCall() {
        RoutingsResponse routingResponse = RoutingsResponse.builder().id(1L).build();
        Map<String, Object> masterDataMap = new HashMap<>();
        when(masterDataUtils.createInBulkUnLocationsRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(List.of(""));
        routingV3Util.addAllUnlocationInSingleCall(routingResponse, masterDataMap);
        verify(masterDataKeyUtils).setMasterDataValue(anyMap(), anyString(), anyMap(), anyMap());
    }

    @Test
    void addAllUnlocationInSingleCall1() {
        RoutingsResponse routingResponse = RoutingsResponse.builder().id(1L).build();
        Map<String, String> masterData = new HashMap<>();
        when(masterDataUtils.createInBulkUnLocationsRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(List.of(""));
        when(masterDataUtils.setMasterData(isNull(), anyString(), anyMap())).thenReturn(masterData);
        routingV3Util.addAllUnlocationInSingleCall(routingResponse, null);
        verify(masterDataUtils).setMasterData(isNull(), anyString(), anyMap());
    }

    @Test
    void addAllUnlocationInSingleCall2() {
        RoutingsResponse routingResponse = RoutingsResponse.builder().id(1L).build();
        when(masterDataUtils.createInBulkUnLocationsRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(List.of(""));
        when(masterDataUtils.setMasterData(isNull(), anyString(), anyMap())).thenThrow(RuntimeException.class);
        routingV3Util.addAllUnlocationInSingleCall(routingResponse, null);
        verify(masterDataUtils).pushToCache(anyMap(), anyString(), anySet(), any(), anyMap());
    }

    @Test
    void addAllVesselInSingleCall() {
        RoutingsResponse routingResponse = RoutingsResponse.builder().id(1L).build();
        Map<String, Object> masterDataMap = new HashMap<>();
        when(masterDataUtils.createInBulkVesselsRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(List.of(""));
        routingV3Util.addAllVesselInSingleCall(routingResponse, masterDataMap);
        verify(masterDataKeyUtils).setMasterDataValue(anyMap(), anyString(), anyMap(), anyMap());
    }

    @Test
    void addAllVesselInSingleCall1() {
        RoutingsResponse routingResponse = RoutingsResponse.builder().id(1L).build();
        Map<String, String> masterData = new HashMap<>();
        when(masterDataUtils.createInBulkVesselsRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(List.of(""));
        when(masterDataUtils.setMasterData(isNull(), anyString(), anyMap())).thenReturn(masterData);
        routingV3Util.addAllVesselInSingleCall(routingResponse, null);
        verify(masterDataUtils).setMasterData(isNull(), anyString(), anyMap());
    }

    @Test
    void addAllVesselInSingleCall2() {
        RoutingsResponse routingResponse = RoutingsResponse.builder().id(1L).build();
        when(masterDataUtils.createInBulkVesselsRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(List.of(""));
        when(masterDataUtils.setMasterData(isNull(), anyString(), anyMap())).thenThrow(RuntimeException.class);
        routingV3Util.addAllVesselInSingleCall(routingResponse, null);
        verify(masterDataUtils).pushToCache(anyMap(), anyString(), anySet(), any(), anyMap());
    }

    @Test
    void addAllCarrierInSingleCallList() {
        List<RoutingsResponse> routingListResponse = new ArrayList<>();
        routingListResponse.add(RoutingsResponse.builder().id(1L).build());
        Map<String, Object> masterDataMap = new HashMap<>();
        when(masterDataUtils.createInBulkCarriersRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(List.of(""));
        routingV3Util.addAllCarrierInSingleCallList(routingListResponse, masterDataMap);
        verify(masterDataKeyUtils).setMasterDataValue(anyMap(), anyString(), anyMap(), anyMap());
    }

    @Test
    void addAllCarrierInSingleCallList1() {
        List<RoutingsResponse> routingListResponse = new ArrayList<>();
        routingListResponse.add(RoutingsResponse.builder().id(1L).build());
        Map<String, String> masterData = new HashMap<>();
        when(masterDataUtils.createInBulkCarriersRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(List.of(""));
        when(masterDataUtils.setMasterData(isNull(), anyString(), anyMap())).thenReturn(masterData);
        routingV3Util.addAllCarrierInSingleCallList(routingListResponse, null);
        verify(masterDataUtils).setMasterData(isNull(), anyString(), anyMap());
    }

    @Test
    void addAllCarrierInSingleCallList2() {
        List<RoutingsResponse> routingListResponse = new ArrayList<>();
        routingListResponse.add(RoutingsResponse.builder().id(1L).build());
        when(masterDataUtils.createInBulkCarriersRequest(any(), eq(Routings.class), anyMap(), anyString(), anyMap())).thenReturn(List.of(""));
        when(masterDataUtils.setMasterData(isNull(), anyString(), anyMap())).thenThrow(RuntimeException.class);
        routingV3Util.addAllCarrierInSingleCallList(routingListResponse, null);
        verify(masterDataUtils).pushToCache(anyMap(), anyString(), anySet(), any(), anyMap());
    }
}