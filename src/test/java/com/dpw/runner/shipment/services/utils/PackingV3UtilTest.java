package com.dpw.runner.shipment.services.utils;


import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackUtilizationV3Request;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.dto.request.PackingExcelModel;
import com.dpw.runner.shipment.services.dto.request.ShipmentOrderAttachDetachRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentOrderV3Request;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.OrderLineV3Response;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.entity.Allocations;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
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

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class PackingV3UtilTest extends CommonMocks {

    @Mock
    private IPackingDao packingDao;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private HttpServletResponse response;

    @Mock
    private MasterDataKeyUtils masterDataKeyUtils;

    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Mock
    private IConsolidationV3Service consolidationV3Service;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IPackingService packingService;

    @InjectMocks
    private PackingV3Util packingV3Util;

    private Packing samplePacking;
    private PackingResponse sampleResponse;
    private ShipmentDetails shipmentDetails;
    private List<Packing> packings;
    private ConsoleShipmentMapping consoleShipmentMapping;
    private ConsolidationDetails consolidationDetails;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        samplePacking = new Packing();
        samplePacking.setId(1L);
        samplePacking.setOrigin("origin");

        sampleResponse = new PackingResponse();
        sampleResponse.setId(1L);

        // Setup ShipmentDetails
        shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setAtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(carrierDetails);

        // Setup ConsoleShipmentMapping
        consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setConsolidationId(100L);
        consoleShipmentMapping.setShipmentId(1L);

        // Setup packings
        packings = new ArrayList<>();
        packings.add(samplePacking);

        // Setup ConsolidationDetails
        consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(100L);
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).IsMAWBColoadingEnabled(true).build());
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

    @Test
    void testUpdateConsolidationIdInPackings_withAirTransportMode() {
        // Given
        when(consoleShipmentMappingDao.findByShipmentId(1L)).thenReturn(Collections.singletonList(consoleShipmentMapping));

        // When
        Long result = packingV3Util.updateConsolidationIdInPackings(shipmentDetails, packings);

        // Then
        assertEquals(100L, result);
        assertEquals(100L, packings.get(0).getConsolidationId());
        verify(consoleShipmentMappingDao).findByShipmentId(1L);
    }

    @Test
    void testUpdateConsolidationIdInPackings_withNonAirTransportMode() {
        // Given
        shipmentDetails.setTransportMode("SEA");

        // When
        Long result = packingV3Util.updateConsolidationIdInPackings(shipmentDetails, packings);

        // Then
        assertNull(result);
        assertNull(packings.get(0).getConsolidationId());
        verify(consoleShipmentMappingDao, never()).findByShipmentId(anyLong());
    }

    @Test
    void testGetConsolidationId_withExistingMapping() {
        // Given
        when(consoleShipmentMappingDao.findByShipmentId(1L)).thenReturn(Collections.singletonList(consoleShipmentMapping));

        // When
        Long result = packingV3Util.getConsolidationId(1L);

        // Then
        assertEquals(100L, result);
        verify(consoleShipmentMappingDao).findByShipmentId(1L);
    }

    @Test
    void testGetConsolidationId_withNoMapping() {
        // Given
        when(consoleShipmentMappingDao.findByShipmentId(1L)).thenReturn(Collections.emptyList());

        // When
        Long result = packingV3Util.getConsolidationId(1L);

        // Then
        assertNull(result);
        verify(consoleShipmentMappingDao).findByShipmentId(1L);
    }

    @Test
    void testProcessPackingRequests_allFieldsSet() {
        // Given
        LocalDateTime now = LocalDateTime.now();
        Packing packing1 = new Packing();
        packing1.setCargoGateInDate(now);
        packing1.setDateType(DateBehaviorType.ACTUAL);
        packing1.setContainerId(201L);

        Packing packing2 = new Packing();
        packing2.setCargoGateInDate(now.plusDays(1));
        packing2.setDateType(DateBehaviorType.ACTUAL);
        packing2.setContainerId(202L);

        List<Packing> packingList = Arrays.asList(packing1, packing2);

        // When
        packingV3Util.processPackingRequests(packingList, shipmentDetails);

        // Then
        assertEquals(ShipmentPackStatus.CARGO_GATED_IN, shipmentDetails.getShipmentPackStatus());
        assertEquals(now.plusDays(1), shipmentDetails.getShipmentGateInDate());
        assertEquals(DateBehaviorType.ACTUAL, shipmentDetails.getDateType());
    }

    @Test
    void testProcessPackingRequests_partialGatedPartialAssigned() {
        // Given
        LocalDateTime now = LocalDateTime.now();
        Packing packing1 = new Packing();
        packing1.setCargoGateInDate(now);
        packing1.setDateType(DateBehaviorType.ACTUAL);
        packing1.setContainerId(201L);

        Packing packing2 = new Packing();
        packing2.setCargoGateInDate(null);
        packing2.setContainerId(null);

        List<Packing> packingList = Arrays.asList(packing1, packing2);

        // When
        packingV3Util.processPackingRequests(packingList, shipmentDetails);

        // Then
        assertEquals(ShipmentPackStatus.PARTIAL_CARGO_GATE_IN, shipmentDetails.getShipmentPackStatus());
        assertEquals(now, shipmentDetails.getShipmentGateInDate());
        assertEquals(DateBehaviorType.ACTUAL, shipmentDetails.getDateType());
    }

    @Test
    void testProcessPackingRequests_noneGatedFullAssigned() {
        // Given
        Packing packing1 = new Packing();
        packing1.setCargoGateInDate(null);
        packing1.setContainerId(201L);

        Packing packing2 = new Packing();
        packing2.setCargoGateInDate(null);
        packing2.setContainerId(202L);

        List<Packing> packingList = Arrays.asList(packing1, packing2);

        // When
        packingV3Util.processPackingRequests(packingList, shipmentDetails);

        // Then
        assertEquals(ShipmentPackStatus.ASSIGNED, shipmentDetails.getShipmentPackStatus());
        assertNull(shipmentDetails.getShipmentGateInDate());
    }

    @Test
    void testSetShipmentPackStatusSailed_withAtdSet() {
        // When
        packingV3Util.setShipmentPackStatusSailed(shipmentDetails);

        // Then
        assertEquals(ShipmentPackStatus.SAILED, shipmentDetails.getShipmentPackStatus());
    }

    @Test
    void testSetShipmentPackStatusSailed_withNoAtd() {
        // Given
        shipmentDetails.getCarrierDetails().setAtd(null);

        // When
        packingV3Util.setShipmentPackStatusSailed(shipmentDetails);

        // Then
        assertNull(shipmentDetails.getShipmentPackStatus());
    }

    @Test
    void testSavePackUtilisationCalculationInConsole_success() {
        // Given
        CalculatePackUtilizationV3Request request = new CalculatePackUtilizationV3Request();
        request.setConsolidationId(100L);

        when(consolidationV3Service.findById(100L)).thenReturn(Optional.of(consolidationDetails));

        // When
        packingV3Util.savePackUtilisationCalculationInConsole(request);

        // Then
        verify(consolidationV3Service, never()).save(any(), anyBoolean());
    }

    @Test
    void testSavePackUtilisationCalculationInConsole_nonAirTransportMode() {
        // Given
        CalculatePackUtilizationV3Request request = new CalculatePackUtilizationV3Request();
        request.setConsolidationId(100L);

        consolidationDetails.setTransportMode("SEA");
        when(consolidationV3Service.findById(100L)).thenReturn(Optional.of(consolidationDetails));

        // When
        packingV3Util.savePackUtilisationCalculationInConsole(request);

        // Then
        verify(consolidationV3Service, never()).save(any(), anyBoolean());
    }

    @Test
    void testSavePackUtilisationCalculationInConsole_consolidationNotFound() {
        // Given
        CalculatePackUtilizationV3Request request = new CalculatePackUtilizationV3Request();
        request.setConsolidationId(100L);

        when(consolidationV3Service.findById(100L)).thenReturn(Optional.empty());

        // When
        packingV3Util.savePackUtilisationCalculationInConsole(request);

        // Then
        verify(consolidationV3Service, never()).save(any(), anyBoolean());
    }

    @Test
    void testSavePackUtilisationCalculationInConsole_exceptionHandling() {
        // Given
        CalculatePackUtilizationV3Request request = new CalculatePackUtilizationV3Request();
        request.setConsolidationId(100L);

        when(consolidationV3Service.findById(100L)).thenThrow(new RuntimeException("Test exception"));

        // When & Then
        assertDoesNotThrow(() -> packingV3Util.savePackUtilisationCalculationInConsole(request));
    }

    @Test
    void testCalculatePacksUtilisationForConsolidation_withShipmentPackingList() throws RunnerException {
        // Given
        CalculatePackUtilizationV3Request request = new CalculatePackUtilizationV3Request();
        request.setConsolidationId(100L);

        List<Packing> shipmentPackingList = new ArrayList<>();
        Packing packing = new Packing();
        packing.setId(101L);
        packing.setShipmentId(1L);
        shipmentPackingList.add(packing);
        request.setShipmentPackingList(shipmentPackingList);

        List<Packing> consolPackingList = new ArrayList<>();
        Packing consolPacking = new Packing();
        consolPacking.setId(102L);
        consolPacking.setShipmentId(2L);
        consolPackingList.add(consolPacking);

        when(consolidationV3Service.findById(100L)).thenReturn(Optional.of(consolidationDetails));
        when(packingDao.findByConsolidationId(100L)).thenReturn(consolPackingList);

        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setAchievedWeight(BigDecimal.valueOf(1000.0));
        packSummaryResponse.setAchievedVolume(BigDecimal.valueOf(10.0));
        packSummaryResponse.setWeightUnit(Constants.WEIGHT_UNIT_KG);
        packSummaryResponse.setVolumeUnit(Constants.VOLUME_UNIT_M3);

        when(packingService.calculatePackSummary(anyList(), eq("AIR"), isNull(), any(ShipmentMeasurementDetailsDto.class)))
                .thenReturn(packSummaryResponse);

        Allocations allocations = new Allocations();
        allocations.setWeight(BigDecimal.valueOf(2000));
        allocations.setVolume(BigDecimal.valueOf(20));
        consolidationDetails.setAllocations(allocations);

        when(commonUtils.calculateConsolUtilization(any(ConsolidationDetails.class))).thenReturn(consolidationDetails);
        mockTenantSettings();

        // When
        PackSummaryResponse result = packingV3Util.calculatePacksUtilisationForConsolidation(request);

        // Then
        assertNotNull(result);
        assertEquals(packSummaryResponse, result);
    }

    @Test
    void testCalculatePacksUtilisationForConsolidation_withAttachingShipments() throws RunnerException {
        // Given
        CalculatePackUtilizationV3Request request = new CalculatePackUtilizationV3Request();
        request.setConsolidationId(100L);
        request.setShipmentIdList(Arrays.asList(1L, 2L));

        List<Packing> consolPackingList = new ArrayList<>();
        Packing consolPacking = new Packing();
        consolPacking.setId(102L);
        consolPacking.setShipmentId(3L);
        consolPackingList.add(consolPacking);

        List<Packing> shipmentPacks = new ArrayList<>();
        Packing shipmentPack1 = new Packing();
        shipmentPack1.setId(103L);
        shipmentPack1.setShipmentId(1L);
        shipmentPacks.add(shipmentPack1);

        Packing shipmentPack2 = new Packing();
        shipmentPack2.setId(104L);
        shipmentPack2.setShipmentId(2L);
        shipmentPacks.add(shipmentPack2);

        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setAchievedWeight(BigDecimal.valueOf(1000.0));
        packSummaryResponse.setAchievedVolume(BigDecimal.valueOf(10.0));
        packSummaryResponse.setWeightUnit(Constants.WEIGHT_UNIT_KG);
        packSummaryResponse.setVolumeUnit(Constants.VOLUME_UNIT_M3);

        Allocations allocations = new Allocations();
        allocations.setWeight(BigDecimal.valueOf(2000));
        allocations.setVolume(BigDecimal.valueOf(20));
        consolidationDetails.setAllocations(allocations);

        when(consolidationV3Service.findById(100L)).thenReturn(Optional.of(consolidationDetails));
        when(packingDao.findByConsolidationId(100L)).thenReturn(consolPackingList);
        when(packingDao.findByShipmentIdIn(Arrays.asList(1L, 2L))).thenReturn(shipmentPacks);

        when(packingService.calculatePackSummary(anyList(), eq("AIR"), isNull(), any(ShipmentMeasurementDetailsDto.class)))
                .thenReturn(packSummaryResponse);
        mockTenantSettings();

        when(commonUtils.calculateConsolUtilization(any(ConsolidationDetails.class))).thenReturn(consolidationDetails);

        // When
        PackSummaryResponse result = packingV3Util.calculatePacksUtilisationForConsolidation(request);

        // Then
        assertNotNull(result);
        assertEquals(packSummaryResponse, result);
    }

    @Test
    void testCalculatePacksUtilisationForConsolidation_withIgnoreConsolidationPacks() throws RunnerException {
        // Given
        CalculatePackUtilizationV3Request request = new CalculatePackUtilizationV3Request();
        request.setConsolidationId(100L);
        request.setShipmentIdList(Arrays.asList(1L, 2L));
        request.setIgnoreConsolidationPacks(true);

        List<Packing> shipmentPacks = new ArrayList<>();
        Packing shipmentPack1 = new Packing();
        shipmentPack1.setId(103L);
        shipmentPack1.setShipmentId(1L);
        shipmentPacks.add(shipmentPack1);

        Packing shipmentPack2 = new Packing();
        shipmentPack2.setId(104L);
        shipmentPack2.setShipmentId(2L);
        shipmentPacks.add(shipmentPack2);

        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setAchievedWeight(BigDecimal.valueOf(1000.0));
        packSummaryResponse.setAchievedVolume(BigDecimal.valueOf(10.0));
        packSummaryResponse.setWeightUnit(Constants.WEIGHT_UNIT_KG);
        packSummaryResponse.setVolumeUnit(Constants.VOLUME_UNIT_M3);

        Allocations allocations = new Allocations();
        allocations.setWeight(BigDecimal.valueOf(2000));
        allocations.setVolume(BigDecimal.valueOf(20));
        consolidationDetails.setAllocations(allocations);

        when(consolidationV3Service.findById(100L)).thenReturn(Optional.of(consolidationDetails));
        when(packingDao.findByShipmentIdIn(Arrays.asList(1L, 2L))).thenReturn(shipmentPacks);

        when(packingService.calculatePackSummary(anyList(), eq("AIR"), isNull(), any(ShipmentMeasurementDetailsDto.class)))
                .thenReturn(packSummaryResponse);

        when(commonUtils.calculateConsolUtilization(any(ConsolidationDetails.class))).thenReturn(consolidationDetails);
        mockTenantSettings();

        // When
        PackSummaryResponse result = packingV3Util.calculatePacksUtilisationForConsolidation(request);

        // Then
        assertNotNull(result);
        assertEquals(packSummaryResponse, result);
        verify(packingDao, times(1)).findByConsolidationId(100L);
    }

    @Test
    void testCalculatePacksUtilisationForConsolidation_withUpdatedConsolPacks() throws RunnerException {
        // Given
        CalculatePackUtilizationV3Request request = new CalculatePackUtilizationV3Request();
        request.setConsolidationId(100L);

        List<Packing> updatedConsolPacks = new ArrayList<>();
        Packing updatedPack = new Packing();
        updatedPack.setId(105L);
        updatedPack.setShipmentId(3L);
        updatedConsolPacks.add(updatedPack);

        PackingV3Request packingV3Request = new PackingV3Request();
        packingV3Request.setId(105L);
        packingV3Request.setShipmentId(3L);
        request.setPackingList(List.of(packingV3Request));

        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setAchievedWeight(BigDecimal.valueOf(1000.0));
        packSummaryResponse.setAchievedVolume(BigDecimal.valueOf(10.0));
        packSummaryResponse.setWeightUnit(Constants.WEIGHT_UNIT_KG);
        packSummaryResponse.setVolumeUnit(Constants.VOLUME_UNIT_M3);

        Allocations allocations = new Allocations();
        allocations.setWeight(BigDecimal.valueOf(2000));
        allocations.setVolume(BigDecimal.valueOf(20));
        consolidationDetails.setAllocations(allocations);

        when(jsonHelper.convertValueToList(any(), eq(Packing.class))).thenReturn(updatedConsolPacks);
        when(consolidationV3Service.findById(100L)).thenReturn(Optional.of(consolidationDetails));

        when(packingService.calculatePackSummary(anyList(), eq("AIR"), isNull(), any(ShipmentMeasurementDetailsDto.class)))
                .thenReturn(packSummaryResponse);

        when(commonUtils.calculateConsolUtilization(any(ConsolidationDetails.class))).thenReturn(consolidationDetails);
        mockTenantSettings();

        // When
        PackSummaryResponse result = packingV3Util.calculatePacksUtilisationForConsolidation(request);

        // Then
        assertNotNull(result);
        assertEquals(packSummaryResponse, result);
    }

    @Test
    void testCalculatePacksUtilisationForConsolidation_consolidationNotFound() throws RunnerException {
        // Given
        CalculatePackUtilizationV3Request request = new CalculatePackUtilizationV3Request();
        request.setConsolidationId(100L);

        when(consolidationV3Service.findById(100L)).thenReturn(Optional.empty());

        // When
        PackSummaryResponse result = packingV3Util.calculatePacksUtilisationForConsolidation(request);

        // Then
        assertNull(result);
        verify(packingService, never()).calculatePackSummary(anyList(), anyString(), any(), any());
    }

    @Test
    void testMapToOrderDetailsList_returnEmptyList() {
        List<ShipmentOrderV3Request> list = new ArrayList<>();
        List<ShipmentOrderAttachDetachRequest.OrderDetails> orderDetailsResponse = packingV3Util.mapToOrderDetailsList(list);
        assertTrue(orderDetailsResponse.isEmpty());
    }


    @Test
    void testMapToOrderDetailsList() {
        ShipmentOrderV3Request element = new ShipmentOrderV3Request();
        element.setId(1L);
        element.setOrderGuid(UUID.randomUUID());
        element.setOrderNumber("ORD_00001");
        element.setShipmentId(2L);

        OrderLineV3Response packingReq = OrderLineV3Response.builder()
                .packs("test packs")
                .packsType("test type")
                .build();
        element.setOrderPackings(List.of(packingReq));

        List<ShipmentOrderV3Request> list = new ArrayList<>();
        list.add(element);

        List<ShipmentOrderAttachDetachRequest.OrderDetails> orderDetailsResponse = packingV3Util.mapToOrderDetailsList(list);
        assertFalse(orderDetailsResponse.isEmpty());
    }

    @Test
    void testMapToOrderDetail_returnNull() {
        ShipmentOrderAttachDetachRequest.OrderDetails orderDetailsResponse = packingV3Util.mapToOrderDetails(null);
        assertNull(orderDetailsResponse);
    }

    @Test
    void testMapToOrderDetail_orderPackingIsNull() {
        ShipmentOrderV3Request element = new ShipmentOrderV3Request();
        element.setId(1L);
        element.setOrderGuid(UUID.randomUUID());
        element.setOrderNumber("ORD_00001");
        element.setShipmentId(2L);

        ShipmentOrderAttachDetachRequest.OrderDetails orderDetailsResponse = packingV3Util.mapToOrderDetails(element);
        assertNotNull(orderDetailsResponse);
    }

    @Test
    void mapOrderLineList_null_returnsEmptyList() {
        List<PackingV3Request> result = packingV3Util.mapOrderLineListToPackingV3RequestList(null);
        assertNotNull(result, "Should never return null for null input");
        assertTrue(result.isEmpty(), "Null input should produce an empty list");
    }

    @Test
    void mapOrderLineList_empty_returnsEmptyList() {
        List<PackingV3Request> result = packingV3Util.mapOrderLineListToPackingV3RequestList(Collections.emptyList());
        assertNotNull(result);
        assertTrue(result.isEmpty(), "Empty input list should return empty list");
    }

    @Test
    void mapOrderLineList_filtersNullAndMapsValues() {
        OrderLineV3Response orderLine = new OrderLineV3Response();
        orderLine.setId(123L);
        orderLine.setGuid(UUID.randomUUID());
        orderLine.setCommodityGroup("COM-GRP");
        orderLine.setContainerId(999L);
        orderLine.setGoodsDescription("Good desc");
        orderLine.setHSCode("HS-001");
        orderLine.setLength(new BigDecimal("12.5"));
        orderLine.setLengthUnit("cm");
        orderLine.setWidth(new BigDecimal("1.5"));
        orderLine.setWidthUnit("cm");
        orderLine.setHeight(new BigDecimal("2.0"));
        orderLine.setHeightUnit("cm");
        orderLine.setWeight(new BigDecimal("5.5"));
        orderLine.setWeightUnit("kg");
        orderLine.setVolume(new BigDecimal("0.25"));
        orderLine.setVolumeUnit("m3");
        orderLine.setNetWeight(new BigDecimal("5.0"));
        orderLine.setNetWeightUnit("kg");
        orderLine.setPacks("10");
        orderLine.setPacksType("BOX");
        orderLine.setLineNo(new BigDecimal("1"));
        orderLine.setSubLineNo(new BigDecimal("0"));
        orderLine.setProductCode("PROD-1");
        orderLine.setShipmentOrderId(555L);

        List<OrderLineV3Response> input = Arrays.asList(orderLine, null);
        List<PackingV3Request> result = packingV3Util.mapOrderLineListToPackingV3RequestList(input);

        assertNotNull(result);
        assertEquals(1, result.size(), "Null elements should be filtered out");
        PackingV3Request mapped = result.get(0);

        assertEquals(orderLine.getCommodityGroup(), mapped.getCommodityGroup());
        assertEquals(orderLine.getContainerId(), mapped.getContainerId());
        assertEquals(orderLine.getGoodsDescription(), mapped.getGoodsDescription());
        assertEquals(orderLine.getHSCode(), mapped.getHSCode());
        assertEquals(orderLine.getPacks(), mapped.getPacks());
        assertEquals(orderLine.getPacksType(), mapped.getPacksType());
        assertEquals(orderLine.getProductCode(), mapped.getProductCode());
        assertEquals(orderLine.getShipmentOrderId(), mapped.getShipmentOrderId());

        assertEquals(0, mapped.getLength().compareTo(orderLine.getLength()));
        assertEquals(0, mapped.getWidth().compareTo(orderLine.getWidth()));
        assertEquals(0, mapped.getHeight().compareTo(orderLine.getHeight()));
        assertEquals(0, mapped.getWeight().compareTo(orderLine.getWeight()));
        assertEquals(0, mapped.getVolume().compareTo(orderLine.getVolume()));
        assertEquals(0, mapped.getNetWeight().compareTo(orderLine.getNetWeight()));
    }

    @Test
    void mapOrderLine_null_returnsNull() {
        PackingV3Request result = packingV3Util.mapOrderLineToPackingV3Request(null);
        assertNull(result, "Mapping a null OrderLine should return null");
    }

    @Test
    void mapOrderLine_valid_returnsMappedObject() {
        OrderLineV3Response orderLineRes = new OrderLineV3Response();
        orderLineRes.setId(7L);
        orderLineRes.setGuid(UUID.randomUUID());
        orderLineRes.setCommodityGroup("grp");
        orderLineRes.setContainerId(11L);
        orderLineRes.setGoodsDescription("desc");
        orderLineRes.setHSCode("hs-code");
        orderLineRes.setLength(BigDecimal.valueOf(2));
        orderLineRes.setLengthUnit("cm");
        orderLineRes.setPacks("3");
        orderLineRes.setPacksType("CTN");
        orderLineRes.setLineNo(BigDecimal.ONE);
        orderLineRes.setSubLineNo(BigDecimal.ZERO);
        orderLineRes.setProductCode("P-01");
        orderLineRes.setShipmentOrderId(100L);

        PackingV3Request packingReq = packingV3Util.mapOrderLineToPackingV3Request(orderLineRes);
        assertNotNull(packingReq);

        assertEquals(orderLineRes.getCommodityGroup(), packingReq.getCommodityGroup());
        assertEquals(orderLineRes.getContainerId(), packingReq.getContainerId());
        assertEquals(orderLineRes.getGoodsDescription(), packingReq.getGoodsDescription());
        assertEquals(orderLineRes.getHSCode(), packingReq.getHSCode());
        assertEquals(orderLineRes.getPacks(), packingReq.getPacks());
        assertEquals(orderLineRes.getPacksType(), packingReq.getPacksType());
        assertEquals(orderLineRes.getProductCode(), packingReq.getProductCode());
        assertEquals(orderLineRes.getShipmentOrderId(), packingReq.getShipmentOrderId());
        assertEquals(0, packingReq.getLength().compareTo(orderLineRes.getLength()));
    }


}
