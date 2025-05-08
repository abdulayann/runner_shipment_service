package com.dpw.runner.shipment.services.utils;


import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dto.request.PackingExcelModel;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.utils.v3.PackingV3Util;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import javax.servlet.http.HttpServletResponse;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class PackingV3UtilTest {

    @Mock
    private IPackingDao packingDao;

    @Mock
    private CommonUtils commonUtils;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private HttpServletResponse response;

    @Mock
    private MasterDataKeyUtils masterDataKeyUtils;

    @InjectMocks
    private PackingV3Util packingV3Util;

    private Packing samplePacking;
    private PackingResponse sampleResponse;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        samplePacking = new Packing();
        samplePacking.setId(1L);
        samplePacking.setOrigin("origin");

        sampleResponse = new PackingResponse();
        sampleResponse.setId(1L);
    }

    @Test
    void testDownloadPacking_withShipmentId() throws Exception {
        BulkDownloadRequest request = new BulkDownloadRequest();
        request.setShipmentId("1");

        PackingExcelModel packingExcelModel = new PackingExcelModel();

        Page<Packing> mockPage = new PageImpl<>(List.of(samplePacking));
        when(packingDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(mockPage);
        when(commonUtils.convertToList(any(), eq(PackingExcelModel.class)))
                .thenReturn(List.of(packingExcelModel));

        MockHttpServletResponse mockResponse = new MockHttpServletResponse();

        packingV3Util.downloadPacking(mockResponse, request);

        assertEquals("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", mockResponse.getContentType());
        assertTrue(mockResponse.getHeader("Content-Disposition").startsWith("attachment; filename=CargoDetails_"));
        assertTrue(mockResponse.getContentAsByteArray().length > 0);
    }

    @Test
    void testDownloadPacking_withShipmentId_and_ConsolidationId() throws Exception {
        BulkDownloadRequest request = new BulkDownloadRequest();
        request.setShipmentId("123");
        request.setConsolidationId("456");

        PackingExcelModel packingExcelModel = new PackingExcelModel();
        packingExcelModel.setOrigin("origin");

        Page<Packing> shipmentPage = mock(Page.class);
        Page<Packing> consPage = mock(Page.class);
        when(shipmentPage.getContent()).thenReturn(List.of(samplePacking));
        when(consPage.getContent()).thenReturn(List.of(samplePacking)); // Overlapping case

        when(packingDao.findAll(any(), any()))
                .thenReturn(shipmentPage)
                .thenReturn(consPage);

        when(commonUtils.convertToList(anyList(), eq(PackingExcelModel.class)))
                .thenReturn(Collections.singletonList(packingExcelModel));

        when(masterDataUtils.fetchInBulkUnlocations(anySet(), anyString()))
                .thenReturn(Map.of("origin", new EntityTransferUnLocations()));

        MockHttpServletResponse mockResponse = new MockHttpServletResponse();
        packingV3Util.downloadPacking(mockResponse, request);

        assertEquals(Constants.CONTENT_TYPE_FOR_EXCEL, mockResponse.getContentType());
    }

    @Test
    void testDownloadPacking_ConsolidationId() throws Exception {
        BulkDownloadRequest request = new BulkDownloadRequest();
        request.setConsolidationId("456");

        PackingExcelModel packingExcelModel = new PackingExcelModel();
        packingExcelModel.setOrigin("origin");

        Page<Packing> consPage = mock(Page.class);
        when(consPage.getContent()).thenReturn(List.of(samplePacking)); // Overlapping case

        when(packingDao.findAll(any(), any()))
                .thenReturn(consPage);

        when(commonUtils.convertToList(anyList(), eq(PackingExcelModel.class)))
                .thenReturn(Collections.singletonList(packingExcelModel));

        when(masterDataUtils.fetchInBulkUnlocations(anySet(), anyString()))
                .thenReturn(Map.of("origin2", new EntityTransferUnLocations()));

        MockHttpServletResponse mockResponse = new MockHttpServletResponse();
        packingV3Util.downloadPacking(mockResponse, request);

        assertEquals(Constants.CONTENT_TYPE_FOR_EXCEL, mockResponse.getContentType());
    }

    @Test
    void testAddAllMasterDataInSingleCallList_success() {
        List<PackingResponse> packingResponses = List.of(sampleResponse);
        List<MasterListRequest> mockRequests = List.of(new MasterListRequest());

        when(masterDataUtils.createInBulkMasterListRequest(any(), any(), any(), any(), any()))
                .thenReturn(mockRequests);

        when(masterDataUtils.fetchInBulkMasterList(any()))
                .thenReturn(Map.of("key", new EntityTransferMasterLists()));

        doNothing().when(masterDataUtils).pushToCache(any(), any(), any(), any(), any());

        doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), anyString(), any(), any());

        assertDoesNotThrow(() -> packingV3Util.addAllMasterDataInSingleCallList(packingResponses, new HashMap<>()));
    }

    @Test
    void testAddAllUnlocationInSingleCallList_success() {
        List<PackingResponse> packingResponses = List.of(sampleResponse);

        when(masterDataUtils.createInBulkUnLocationsRequest(any(), any(), any(), any(), any()))
                .thenReturn(List.of("loc1"));

        when(masterDataUtils.fetchInBulkUnlocations(anySet(), anyString()))
                .thenReturn(Map.of("loc1", new EntityTransferUnLocations()));

        doNothing().when(masterDataUtils).pushToCache(any(), any(), any(), any(), any());

        doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), anyString(), any(), any());

        assertDoesNotThrow(() -> packingV3Util.addAllUnlocationInSingleCallList(packingResponses, new HashMap<>()));
    }

    @Test
    void testAddAllCommodityTypesInSingleCallList_success() {
        List<PackingResponse> packingResponses = List.of(sampleResponse);

        when(masterDataUtils.createInBulkCommodityTypeRequest(any(), any(), any(), any(), any()))
                .thenReturn(List.of("commodity1"));

        when(masterDataUtils.fetchInBulkCommodityTypes(anyList()))
                .thenReturn(Map.of("commodity1", new EntityTransferCommodityType()));

        doNothing().when(masterDataUtils).pushToCache(any(), any(), any(), any(), any());

        doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), anyString(), any(), any());

        assertDoesNotThrow(() -> packingV3Util.addAllCommodityTypesInSingleCallList(packingResponses, new HashMap<>()));
    }

    @Test
    void testAddAllMasterDataInSingleCallList_shouldHandleException() {
        List<PackingResponse> packingResponses = List.of(sampleResponse);
        List<MasterListRequest> mockRequests = List.of(new MasterListRequest());

        when(masterDataUtils.createInBulkMasterListRequest(any(), any(), any(), any(), any()))
                .thenReturn(mockRequests);

        when(masterDataUtils.fetchInBulkMasterList(any()))
                .thenReturn(Map.of("key", new EntityTransferMasterLists()));

        doNothing().when(masterDataUtils).pushToCache(any(), any(), any(), any(), any());

        when(masterDataUtils.setMasterData(any(), any(), any())).thenThrow(new RuntimeException("Simulated Exception"));

        assertDoesNotThrow(() -> packingV3Util.addAllMasterDataInSingleCallList(packingResponses, null));

        verify(masterDataUtils, times(1)).createInBulkMasterListRequest(any(), any(), any(), any(), any());
    }

    @Test
    void testAddAllUnlocationInSingleCallList_shouldHandleException() {
        List<PackingResponse> packingResponses = List.of(sampleResponse);

        when(masterDataUtils.createInBulkUnLocationsRequest(any(), any(), any(), any(), any()))
                .thenReturn(List.of("loc1"));

        when(masterDataUtils.fetchInBulkUnlocations(anySet(), anyString()))
                .thenReturn(Map.of("loc1", new EntityTransferUnLocations()));

        doNothing().when(masterDataUtils).pushToCache(any(), any(), any(), any(), any());

        when(masterDataUtils.setMasterData(any(), any(), any())).thenThrow(new RuntimeException("Simulated Exception"));

        assertDoesNotThrow(() -> packingV3Util.addAllUnlocationInSingleCallList(packingResponses, null));

        verify(masterDataUtils, times(1)).createInBulkUnLocationsRequest(any(), any(), any(), any(), any());
    }

    @Test
    void testAddAllCommodityTypesInSingleCallList_shouldHandleException() {
        List<PackingResponse> packingResponses = List.of(sampleResponse);

        when(masterDataUtils.createInBulkCommodityTypeRequest(any(), any(), any(), any(), any()))
                .thenReturn(List.of("commodity1"));

        when(masterDataUtils.fetchInBulkCommodityTypes(anyList()))
                .thenReturn(Map.of("commodity1", new EntityTransferCommodityType()));

        doNothing().when(masterDataUtils).pushToCache(any(), any(), any(), any(), any());

        when(masterDataUtils.setMasterData(any(), any(), any())).thenThrow(new RuntimeException("Simulated Exception"));

        assertDoesNotThrow(() -> packingV3Util.addAllCommodityTypesInSingleCallList(packingResponses, null));

        verify(masterDataUtils, times(1)).createInBulkCommodityTypeRequest(any(), any(), any(), any(), any());
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

        CompletableFuture<Map<String, EntityTransferMasterLists>> result = packingV3Util.addAllMasterDataInSingleCall(sampleResponse, masterDataResponse);
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

        CompletableFuture<Map<String, EntityTransferMasterLists>> result = packingV3Util.addAllMasterDataInSingleCall(sampleResponse, null);
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

        CompletableFuture<Map<String, EntityTransferUnLocations>> result = packingV3Util.addAllUnlocationDataInSingleCall(sampleResponse, masterDataResponse);
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

        CompletableFuture<Map<String, EntityTransferUnLocations>> result = packingV3Util.addAllUnlocationDataInSingleCall(sampleResponse, null);
        assertNotNull(result);
    }

    @Test
    void testAddAllCommodityTypesInSingleCall_Success() {
        Map<String, Object> masterDataResponse = new HashMap<>();
        List<String> commodityTypes = List.of("COM1");
        Map<String, EntityTransferCommodityType> responseMap = Map.of("COM1", new EntityTransferCommodityType());

        when(masterDataUtils.createInBulkCommodityTypeRequest(any(), any(), any(), any(), any())).thenReturn(commodityTypes);
        when(masterDataUtils.fetchInBulkCommodityTypes(any())).thenReturn(responseMap);
        doNothing().when(masterDataUtils).pushToCache(any(), anyString(), any(), any(), any());
        doNothing().when(masterDataKeyUtils).setMasterDataValue(any(), anyString(), any(), any());

        CompletableFuture<Map<String, EntityTransferCommodityType>> result = packingV3Util.addAllCommodityTypesInSingleCall(sampleResponse, masterDataResponse);
        assertNotNull(result);
        assertEquals(responseMap, result.join());
    }

    @Test
    void testAddAllCommodityTypesInSingleCall_Exception() {
        List<String> commodityTypes = List.of("COM1");
        Map<String, EntityTransferCommodityType> responseMap = Map.of("COM1", new EntityTransferCommodityType());

        when(masterDataUtils.createInBulkCommodityTypeRequest(any(), any(), any(), any(), any())).thenReturn(commodityTypes);
        when(masterDataUtils.fetchInBulkCommodityTypes(any())).thenReturn(responseMap);
        doNothing().when(masterDataUtils).pushToCache(any(), anyString(), any(), any(), any());
        when(masterDataUtils.setMasterData(any(), any(), any())).thenThrow(new RuntimeException("Simulated Exception"));

        CompletableFuture<Map<String, EntityTransferCommodityType>> result = packingV3Util.addAllCommodityTypesInSingleCall(sampleResponse, null);
        assertNotNull(result);
    }


}
