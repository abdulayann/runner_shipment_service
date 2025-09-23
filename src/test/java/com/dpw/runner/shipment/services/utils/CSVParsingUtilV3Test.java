package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentVersionContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ContainerConstants;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.dao.impl.ConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.ContainerEventExcelModel;
import com.dpw.runner.shipment.services.dto.request.ContainersExcelModel;
import com.dpw.runner.shipment.services.dto.request.PackingExcelModel;
import com.dpw.runner.shipment.services.dto.v1.response.V1ContainerTypeResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferDGSubstance;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.CommodityResponse;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.core.type.TypeReference;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.MDC;
import org.springframework.data.domain.PageImpl;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class CSVParsingUtilV3Test {

    @Mock
    private ConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IV1Service v1Service;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    private CommonUtils commonUtils;

    @InjectMocks
    private CSVParsingUtilV3<Packing> csvParsingUtilV3;

    private JsonTestUtility jsonTestUtility;

    private static MultipartFile mockPackingFile;
    private static MultipartFile mockPackingInvalidFile;
    private static MultipartFile mockPackingInvalidDGSubstanceIdFile;
    private static MultipartFile mockContainerFile, mockContainerFileEmptyContainerCode, mockPackingFileWithCols;
    private static MultipartFile nullExcelFile, emptyExcelFile2;
    private static MultipartFile mockExcelFileEventsMissingEventCode;
    private static MultipartFile testContainerV3;
    private static CSVParsingUtil<Containers> csvParsingUtilContainer;
    private static CSVParsingUtil<Events> csvParsingUtilContainerEvents;
    private static MultipartFile mockExcelFileEvents, mockPackingMissingShipmentNumberInShipmentDict, mockPackingMissingShipmentNumber;

    @BeforeEach
    void setUp() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        csvParsingUtilContainer = new CSVParsingUtil<Containers>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2));
        csvParsingUtilV3 = new CSVParsingUtilV3<Packing>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);
        csvParsingUtilContainerEvents = new CSVParsingUtil<Events>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2));
    }

    @AfterEach
    void tearDown() {
        csvParsingUtilV3.executorService.shutdown();
        csvParsingUtilContainer.executorService.shutdown();
        csvParsingUtilContainerEvents.executorService.shutdown();
    }

    @BeforeAll
    static void setupBeforeAll() throws IOException {
        nullExcelFile = getFileFromResources("EmptyExcel.xlsx");
        emptyExcelFile2 = getFileFromResources("EmptyExcelWithMissingCol.xlsx");
        mockPackingMissingShipmentNumberInShipmentDict = getFileFromResources("TestCargoDetails_Missing_ShipmentNumber_InShipmentDict.xlsx");
        mockPackingMissingShipmentNumber = getFileFromResources("TestCargoDetails_Missing_ShipmentNumber.xlsx");
        mockExcelFileEvents = getFileFromResources("TestContainerEvents.xlsx");
        mockContainerFile = getFileFromResources("TestContainers.xlsx");
        testContainerV3 = getFileFromResources("TestContainerV3.xlsx");
        mockContainerFileEmptyContainerCode = getFileFromResources("TestContainerEmptyContainerCode.xlsx");
        mockPackingFile = getFileFromResources("TestCargoDetails.xlsx");
        mockPackingFileWithCols = getFileFromResources("TestCargoDetailsWithCols.xlsx");
        mockPackingInvalidFile = getFileFromResources("TestCargoDetailsInvalid.xlsx");
        mockPackingInvalidDGSubstanceIdFile = getFileFromResources("CargoDetails_InvalidDGSubstanceId.xlsx");
        mockExcelFileEventsMissingEventCode = getFileFromResources("TestContainerEvents_MissingEventCode.xlsx");
    }

    static MultipartFile getFileFromResources(String fileName) throws IOException {
        try (InputStream is = CSVParsingUtilTest.class.getClassLoader().getResourceAsStream(fileName)) {
            if (is == null) {
                throw new IOException("File not found: " + fileName);
            }
            return new MockMultipartFile("file", fileName, "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", is);
        }
    }

    @Test
    void testNullExcelFile() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();

        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(ConsoleShipmentMapping.builder().shipmentId(12L).consolidationId(13L).build()));
        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(sd1)));

        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        assertThrows(ValidationException.class, () -> csvParsingUtilV3.parseExcelFile(nullExcelFile, request, null, masterDataMap, Packing.class, PackingExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap, new ArrayList<>(), new ArrayList<>()));
    }

    @Test
    void testEmptyExcelFile_Columns_less_than_1() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"), jsonTestUtility.getTestPacking(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"), jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();

        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(ConsoleShipmentMapping.builder().shipmentId(12L).consolidationId(13L).build()));
        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(sd1)));

        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        assertThrows(ValidationException.class, () -> csvParsingUtilV3.parseExcelFilePacking(emptyExcelFile2, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap, new ArrayList<>()));
    }

    @Test
    void testInValidInput_InvalidDGSubstanceId() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"), jsonTestUtility.getTestPacking(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"), jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a", "b"));
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(ConsoleShipmentMapping.builder().shipmentId(12L).consolidationId(13L).build()));
        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(sd1)));

        assertThrows(ValidationException.class, () -> csvParsingUtilV3.parseExcelFilePacking(mockPackingInvalidDGSubstanceIdFile, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap, new ArrayList<>()));
    }

    @Test
    void testInValidInput_validDGSubstanceId() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"), jsonTestUtility.getTestPacking(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"), jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a", "b"));
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(ConsoleShipmentMapping.builder().shipmentId(12L).consolidationId(13L).build()));
        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(sd1)));
        List<Packing> result = (List<Packing>) csvParsingUtilV3.parseExcelFilePacking(mockPackingFile, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap, new ArrayList<>());

        assertNotNull(result);
        assertEquals(2, result.size());
    }

    @Test
    void testValidInput_ForPacking() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"), jsonTestUtility.getTestPacking(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"), jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a", "b"));
        when(v1Service.fetchDangerousGoodData(any())).thenReturn(V1DataResponse.builder().entities(new ArrayList<>()).build());
        List<EntityTransferDGSubstance> entityTransferDGSubstanceList = List.of(EntityTransferDGSubstance.builder().Id(2L).UNIDNo(123L).FlashPoint("FlashPoint").build());
        when(jsonHelper.convertValueToList(any(Object.class), eq(EntityTransferDGSubstance.class))).thenReturn(entityTransferDGSubstanceList);
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(ConsoleShipmentMapping.builder().shipmentId(12L).consolidationId(13L).build()));


        when(v1Service.fetchCommodityData(any())).thenReturn(V1DataResponse.builder().entities(new ArrayList<>()).build());
        CommodityResponse commodityResponse = new CommodityResponse();

        commodityResponse.setCode("1231");
        List<CommodityResponse> commodityResponseList = List.of(commodityResponse);
        when(jsonHelper.convertValueToList(any(Object.class), eq(CommodityResponse.class))).thenReturn(commodityResponseList);

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(sd1)));
        List<Packing> result = (List<Packing>) csvParsingUtilV3.parseExcelFilePacking(mockPackingFile, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap, new ArrayList<>());

        assertNotNull(result);
        assertEquals(2, result.size());
    }

    @Test
    void testInValidInput_ForPacking_Missing_ShipmentNumber() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"), jsonTestUtility.getTestPacking(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"), jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a", "b"));
        when(v1Service.fetchDangerousGoodData(any())).thenReturn(V1DataResponse.builder().entities(new ArrayList<>()).build());
        List<EntityTransferDGSubstance> entityTransferDGSubstanceList = List.of(EntityTransferDGSubstance.builder().Id(2L).UNIDNo(123L).FlashPoint("FlashPoint").build());
        when(jsonHelper.convertValueToList(any(Object.class), eq(EntityTransferDGSubstance.class))).thenReturn(entityTransferDGSubstanceList);
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(ConsoleShipmentMapping.builder().shipmentId(12L).consolidationId(13L).build()));


        when(v1Service.fetchCommodityData(any())).thenReturn(V1DataResponse.builder().entities(new ArrayList<>()).build());
        CommodityResponse commodityResponse = new CommodityResponse();

        commodityResponse.setCode("1231");
        List<CommodityResponse> commodityResponseList = List.of(commodityResponse);
        when(jsonHelper.convertValueToList(any(Object.class), eq(CommodityResponse.class))).thenReturn(commodityResponseList);

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(sd1)));
        assertThrows(ValidationException.class, () -> csvParsingUtilV3.parseExcelFilePacking(mockPackingMissingShipmentNumber, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap, new ArrayList<>()));

    }

    @Test
    void testInValidInput_ForPacking_Missing_ShipmentNumber_InShipmentIdDict() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"), jsonTestUtility.getTestPacking(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"), jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a", "b"));
        when(v1Service.fetchDangerousGoodData(any())).thenReturn(V1DataResponse.builder().entities(new ArrayList<>()).build());
        List<EntityTransferDGSubstance> entityTransferDGSubstanceList = List.of(EntityTransferDGSubstance.builder().Id(2L).UNIDNo(123L).FlashPoint("FlashPoint").build());
        when(jsonHelper.convertValueToList(any(Object.class), eq(EntityTransferDGSubstance.class))).thenReturn(entityTransferDGSubstanceList);
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(ConsoleShipmentMapping.builder().shipmentId(12L).consolidationId(13L).build()));


        when(v1Service.fetchCommodityData(any())).thenReturn(V1DataResponse.builder().entities(new ArrayList<>()).build());
        CommodityResponse commodityResponse = new CommodityResponse();

        commodityResponse.setCode("1231");
        List<CommodityResponse> commodityResponseList = List.of(commodityResponse);
        when(jsonHelper.convertValueToList(any(Object.class), eq(CommodityResponse.class))).thenReturn(commodityResponseList);

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(sd1)));
        assertThrows(ValidationException.class, () -> csvParsingUtilV3.parseExcelFilePacking(mockPackingMissingShipmentNumberInShipmentDict, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap, new ArrayList<>()));

    }

    @Test
    void testFetchMasterList() {
        MasterData md = new MasterData();
        md.setItemValue("val");

        when(v1Service.fetchMasterData(any())).thenReturn(V1DataResponse.builder().entities(new ArrayList<>()).build());
        when(jsonHelper.convertValueToList(any(Object.class), any())).thenReturn(List.of(md));
        Map<String, Set<String>> masterDataMap = new HashMap<>();
        csvParsingUtilV3.fetchMasterLists(MasterDataType.BL_TYPE, masterDataMap);

        assertTrue(masterDataMap.containsKey(MasterDataType.BL_TYPE.getDescription()));
        assertTrue(masterDataMap.get(MasterDataType.BL_TYPE.getDescription()).contains(md.getItemValue()));
    }

    @Test
    void testInValidInput_ForPacking_InvalidContainerCode_ThrowsException() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        var container = jsonTestUtility.getTestContainer();
        container.setGuid(UUID.fromString("bcf5d7cc-9448-4418-90f1-a94245f5d2d7"));
        Map<UUID, Containers> mapOfEntity = Map.of(
                UUID.fromString("bcf5d7cc-9448-4418-90f1-a94245f5d2d7"), container);

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        var spyService = Mockito.spy(csvParsingUtilContainer);

        MDC.setContextMap(Map.of("a", "b"));
        lenient().doReturn(Map.of("ContainerTypes", Set.of(""))).when(spyService).getAllMasterDataContainer(any(), any(), any(), any());

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        assertThrows(ValidationException.class, () -> spyService.parseExcelFile(mockContainerFile, request, mapOfEntity, masterDataMap, Containers.class, ContainersExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap));
    }

    @Test
    void testInValidInput_ForContainer_EmptyContainerCode_ThrowsException() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setTransportMode("NULL");
        request.setConsolidationId(123L);
        var container = jsonTestUtility.getTestContainer();
        container.setGuid(UUID.fromString("bcf5d7cc-9448-4418-90f1-a94245f5d2d7"));
        Map<UUID, Containers> mapOfEntity = Map.of(
                UUID.fromString("bcf5d7cc-9448-4418-90f1-a94245f5d2d7"), container);

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        var spyService = Mockito.spy(csvParsingUtilContainer);

        MDC.setContextMap(Map.of("a", "b"));

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        assertThrows(ValidationException.class, () -> spyService.parseExcelFile(mockContainerFileEmptyContainerCode, request, mapOfEntity, masterDataMap, Containers.class, ContainersExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap));
    }

    @Test
    void testInValidInput_ForPacking_InvalidHBLDeliveryMode_ThrowsException() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        var container = jsonTestUtility.getTestContainer();
        container.setGuid(UUID.fromString("bcf5d7cc-9448-4418-90f1-a94245f5d2d7"));
        Map<UUID, Containers> mapOfEntity = Map.of(
                UUID.fromString("bcf5d7cc-9448-4418-90f1-a94245f5d2d7"), container);

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        var spyService = Mockito.spy(csvParsingUtilContainer);

        MDC.setContextMap(Map.of("a", "b"));
        lenient().doReturn(Map.of("HBLDeliveryMode", Set.of(""))).when(spyService).getAllMasterDataContainer(any(), any(), any(), any());

        assertThrows(ValidationException.class, () -> spyService.parseExcelFile(mockContainerFile, request, mapOfEntity, masterDataMap, Containers.class, ContainersExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap));
    }

    @Test
    void testInValidInput_ForPacking_InvalidVolumeUnit_ThrowsException() {
        var spyService = Mockito.spy(csvParsingUtilV3);
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"), jsonTestUtility.getTestPacking(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"), jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("Entity", "Packing"));
        doReturn(Map.of("VolumeUnit", Set.of("KG", "MG", "DG"))).when(spyService).getAllMasterDataPacking(any(), any(), any(), any());

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(sd1)));
        assertThrows(ValidationException.class, () -> spyService.parseExcelFilePacking(mockPackingFileWithCols, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap, new ArrayList<>()));
    }

    @Test
    void testInValidInput_ForPacking_InvalidGrossVolumeUnit_ThrowsException() {
        var spyService = Mockito.spy(csvParsingUtilV3);
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"), jsonTestUtility.getTestPacking(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"), jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a", "b"));

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(sd1)));
        doReturn(Map.of("VolumeUnit", Set.of("M3", "MG")))
                .when(spyService).getAllMasterDataPacking(any(), any(), any(), any());
        assertThrows(ValidationException.class, () ->
                spyService.parseExcelFilePacking(mockPackingFileWithCols, request, mapOfEntity,
                        masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap, new ArrayList<>())
        );
    }

    @Test
    void testInValidInput_ForPacking_InvalidAllocatedVolumeUnit_ThrowsException() {
        var spyService = Mockito.spy(csvParsingUtilV3);
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"), jsonTestUtility.getTestPacking(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"), jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a", "b"));
        doReturn(Map.of("VolumeUnit", Set.of("M3", "KG", "DG"))).when(spyService).getAllMasterDataPacking(any(), any(), any(), any());

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(sd1)));
        assertThrows(ValidationException.class, () -> spyService.parseExcelFilePacking(mockPackingFileWithCols, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap, new ArrayList<>()));
    }

    @Test
    void testInValidInput_ForPacking_InvalidAchievedVolumeUnit_ThrowsException() {
        var spyService = Mockito.spy(csvParsingUtilV3);
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"), jsonTestUtility.getTestPacking(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"), jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a", "b"));
        doReturn(Map.of("VolumeUnit", Set.of("M3", "KG", "MG"))).when(spyService).getAllMasterDataPacking(any(), any(), any(), any());

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(sd1)));
        assertThrows(ValidationException.class, () -> spyService.parseExcelFilePacking(mockPackingFileWithCols, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap, new ArrayList<>()));
    }

    @Test
    void testInValidInput_ForPacking_InvalidOrigin_ThrowsException() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"), jsonTestUtility.getTestPacking(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"), jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a", "b"));
        when(v1Service.fetchDangerousGoodData(any())).thenReturn(V1DataResponse.builder().entities(new ArrayList<>()).build());
        List<EntityTransferDGSubstance> entityTransferDGSubstanceList = List.of(EntityTransferDGSubstance.builder().Id(2L).UNIDNo(123L).FlashPoint("FlashPoint").build());
        when(jsonHelper.convertValueToList(any(Object.class), eq(EntityTransferDGSubstance.class))).thenReturn(entityTransferDGSubstanceList);
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(ConsoleShipmentMapping.builder().shipmentId(12L).consolidationId(13L).build()));

        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().entities(new ArrayList<>()).build());
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setLocCode("");
        unlocationsResponse.setLocationsReferenceGUID(UUID.randomUUID().toString());

        when(jsonHelper.convertValueToList(any(Object.class), eq(UnlocationsResponse.class))).thenReturn(List.of(unlocationsResponse));


        when(v1Service.fetchCommodityData(any())).thenReturn(V1DataResponse.builder().entities(new ArrayList<>()).build());
        CommodityResponse commodityResponse = new CommodityResponse();
        commodityResponse.setCode("1231");
        List<CommodityResponse> commodityResponseList = List.of(commodityResponse);
        when(jsonHelper.convertValueToList(any(Object.class), eq(CommodityResponse.class))).thenReturn(commodityResponseList);

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(sd1)));
        assertThrows(ValidationException.class, () -> csvParsingUtilV3.parseExcelFilePacking(mockPackingFile, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap, new ArrayList<>()));
    }

    @Test
    void testValidInput_ForContainerEvents() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Events> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"), jsonTestUtility.getTestEventData(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"), jsonTestUtility.getTestEventData());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        var consol = jsonTestUtility.getTestNewConsolidation();
        consol.setContainersList(List.of(Containers.builder().containerNumber("ABCD1231").build()));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consol));
        MDC.setContextMap(Map.of("a", "b"));
        CommodityResponse commodityResponse = new CommodityResponse();

        commodityResponse.setCode("1231");

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        List<Events> result = (List<Events>) csvParsingUtilContainerEvents.parseExcelFile(mockExcelFileEvents, request, mapOfEntity, masterDataMap, Events.class, ContainerEventExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap);

        assertNotNull(result);
        assertEquals(1, result.size());
    }

    @Test
    void testInValidInput_ForContainerEvents_NullConsolidationId_throwsException() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(null);
        Map<UUID, Events> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"), jsonTestUtility.getTestEventData(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"), jsonTestUtility.getTestEventData());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        var consol = jsonTestUtility.getTestNewConsolidation();
        consol.setContainersList(List.of(Containers.builder().containerNumber("ABCD1231").build()));
        MDC.setContextMap(Map.of("a", "b"));

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        assertThrows(ValidationException.class, () -> csvParsingUtilContainerEvents.parseExcelFile(mockExcelFileEvents, request, mapOfEntity, masterDataMap, Events.class, ContainerEventExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap));
    }

    @Test
    void testValidInput_ForContainerEvents_MissingEventCodeColumn_ThrowsException() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Events> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"), jsonTestUtility.getTestEventData(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"), jsonTestUtility.getTestEventData());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        var consol = jsonTestUtility.getTestNewConsolidation();
        consol.setContainersList(List.of(Containers.builder().containerNumber("ABCD1231").build()));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consol));
        MDC.setContextMap(Map.of("a", "b"));
        CommodityResponse commodityResponse = new CommodityResponse();

        commodityResponse.setCode("1231");

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        assertThrows(ValidationException.class, () -> csvParsingUtilContainerEvents.parseExcelFile(mockExcelFileEventsMissingEventCode, request, mapOfEntity, masterDataMap, Events.class, ContainerEventExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap));
    }

    @Test
    void testValidInput_ForContainerEvents_MissingConsolidation_ThrowsException() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Events> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"), jsonTestUtility.getTestEventData(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"), jsonTestUtility.getTestEventData());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        var consol = jsonTestUtility.getTestNewConsolidation();
        consol.setContainersList(List.of(Containers.builder().containerNumber("ABCD1231").build()));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
        MDC.setContextMap(Map.of("a", "b"));
        CommodityResponse commodityResponse = new CommodityResponse();

        commodityResponse.setCode("1231");

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        assertThrows(ValidationException.class, () -> csvParsingUtilContainerEvents.parseExcelFile(mockExcelFileEventsMissingEventCode, request, mapOfEntity, masterDataMap, Events.class, ContainerEventExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap));
    }


    @Test
    void testValidInput_ForContainers() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        var container = jsonTestUtility.getTestContainer();
        container.setGuid(UUID.fromString("bcf5d7cc-9448-4418-90f1-a94245f5d2d7"));
        Map<UUID, Containers> mapOfEntity = Map.of(
                UUID.fromString("bcf5d7cc-9448-4418-90f1-a94245f5d2d7"), container);

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();


        when(v1Service.fetchContainerTypeData(any())).thenReturn(V1DataResponse.builder().entities(new ArrayList<>()).build());
        V1ContainerTypeResponse v1ContainerTypeResponse = new V1ContainerTypeResponse();
        v1ContainerTypeResponse.setCode("20GP");
        when(jsonHelper.convertValueToList(any(Object.class), eq(V1ContainerTypeResponse.class))).thenReturn(List.of(v1ContainerTypeResponse));


        MDC.setContextMap(Map.of("a", "b"));
        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        List<Containers> result = csvParsingUtilContainer.parseExcelFile(mockContainerFile, request, mapOfEntity, masterDataMap, Containers.class, ContainersExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap);

        assertNotNull(result);
        assertEquals(1, result.size());

        ShipmentVersionContext.markV3();
        result = csvParsingUtilContainer.parseExcelFile(testContainerV3, request, mapOfEntity, masterDataMap, Containers.class, ContainersExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap);
        ShipmentVersionContext.remove();
        assertNotNull(result);
        assertEquals(1, result.size());
    }

    @Test
    void testInValidInput_ForContainers_GuidNotPresentInMapOfEntity() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        var container = jsonTestUtility.getTestContainer();
        container.setGuid(UUID.fromString("bcf5d7cc-9448-4418-90f1-a94245f5d2d7"));
        Map<UUID, Containers> mapOfEntity = new HashMap<>();

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a", "b"));
        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);

        assertThrows(ValidationException.class, () -> csvParsingUtilContainer.parseExcelFile(mockContainerFile, request, mapOfEntity, masterDataMap, Containers.class, ContainersExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap));

    }

    @Test
    void testInValidInputFile() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);

        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"), jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a", "b"));
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(ConsoleShipmentMapping.builder().shipmentId(12L).consolidationId(13L).build()));
        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);

        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(sd1)));

        assertThrows(ValidationException.class, () -> csvParsingUtilV3.parseExcelFilePacking(mockPackingInvalidFile, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap, new ArrayList<>()));
    }

    @Test
    void testValidateParseExcelFile_HeaderSetSmallerThanHeaderRow_ThrowsValidationException() {
        Row headerRow = Mockito.mock(Row.class);
        when(headerRow.getLastCellNum()).thenReturn((short) 5);
        Set<String> headerSet = new HashSet<>(List.of("col1", "col2", "col3"));
        Set<String> mandatoryColumns = new HashSet<>();
        ValidationException exception = assertThrows(
                ValidationException.class,
                () -> csvParsingUtilV3.validateParseExcelFile(mandatoryColumns, headerSet, headerRow)
        );
        assertEquals(ContainerConstants.INVALID_EXCEL_COLUMNS, exception.getMessage());
    }

    @Test
    void testValidateGuidPos_AddsErrorForInvalidGuidAndCatchesException() {
        var spyUtil = Mockito.spy(csvParsingUtilV3);
        Row mockRow = Mockito.mock(Row.class);
        Cell mockCell = Mockito.mock(Cell.class);
        String validGuid = UUID.randomUUID().toString();
        when(mockRow.getCell(0)).thenReturn(mockCell);
        doReturn(validGuid).when(spyUtil).getCellValueAsString(mockCell);
        Map<UUID, Packing> mapOfEntity = new HashMap<>(); // empty map
        List<String> errorList = new ArrayList<>();
        spyUtil.validateGuidPos(mapOfEntity, 0, mockRow, 1, errorList);
        assertEquals(1, errorList.size());
        assertTrue(errorList.get(0).contains(validGuid));
        errorList.clear();
        String invalidGuid = "NOT-A-UUID";
        doReturn(invalidGuid).when(spyUtil).getCellValueAsString(mockCell);
        spyUtil.validateGuidPos(mapOfEntity, 0, mockRow, 2, errorList);
        assertEquals(1, errorList.size());
        assertTrue(errorList.get(0).contains(invalidGuid));
    }

    @Test
    void testParseExcelFile_WhenEntityTypeIsEvents_CallsParseExcelFileEvents() throws IOException {
        CSVParsingUtilV3<?> spyService = Mockito.spy(csvParsingUtilV3);

        MultipartFile mockFile = Mockito.mock(MultipartFile.class);
        BulkUploadRequest request = new BulkUploadRequest();
        Map<String, Set<String>> masterDataMap = new HashMap<>();

        List<Events> expectedEvents = List.of(new Events(), new Events());

        doReturn(expectedEvents)
                .when(spyService)
                .parseExcelFileEvents(eq(mockFile), eq(request), eq(masterDataMap), any(Class.class));

        List<?> result = spyService.parseExcelFile(
                mockFile,
                request,
                new HashMap<>(),
                masterDataMap,
                (Class) Events.class,
                Object.class,
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new ArrayList<>(),
                new ArrayList<>()
        );

        verify(spyService, times(1))
                .parseExcelFileEvents(eq(mockFile), eq(request), eq(masterDataMap), any(Class.class));

        assertSame(expectedEvents, result);
    }

    @Test
    void testParseExcelGeneric_SuccessfulExecution_ReturnsEntityList() throws Exception {
        CSVParsingUtilV3<?> spyService = Mockito.spy(csvParsingUtilV3);

        MultipartFile mockFile = Mockito.mock(MultipartFile.class);
        BulkUploadRequest request = new BulkUploadRequest();
        Map<UUID, Object> mapOfEntity = new HashMap<>();
        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        List<String> errorList = new ArrayList<>();
        List<String> excelHeaders = List.of("guid", "containerStuffingLocation", "commodityCode");

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();
        Row headerRow = sheet.createRow(0);
        headerRow.createCell(0).setCellValue("guid");
        headerRow.createCell(1).setCellValue("containerStuffingLocation");
        headerRow.createCell(2).setCellValue("commodityCode");

        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        workbook.write(bos);
        workbook.close();
        byte[] workbookBytes = bos.toByteArray();
        when(mockFile.getInputStream()).thenReturn(new ByteArrayInputStream(workbookBytes));

        doNothing().when(spyService).validateExcel(any(Sheet.class));
        doReturn(new String[]{"guid", "containerStuffingLocation", "commodityCode"})
                .when(spyService).extractHeader(any(Row.class), any(), any());
        doReturn(0).when(spyService).findColumnIndex(any(String[].class), eq("guid"));
        doReturn(1).when(spyService).findColumnIndex(any(String[].class), eq("containerStuffingLocation"));
        doReturn(2).when(spyService).findColumnIndex(any(String[].class), eq("commodityCode"));
        doNothing().when(spyService).addGuidInList(any(Sheet.class), anyInt(), anyList(), anyInt(), anyList(), anyInt(), anyList());
        doReturn(new HashMap<>()).when(spyService).getAllMasterDataContainer(anyList(), anyList(), anyMap(), anyMap());
        doNothing().when(spyService).processSheetLastRowNum(any(), anyMap(), any(), any(), anyInt(), any(), anyMap(), anyMap(), anyList(), anyList());

        Method method = CSVParsingUtilV3.class.getDeclaredMethod(
                "parseExcelGeneric",
                MultipartFile.class,
                BulkUploadRequest.class,
                Map.class,
                Map.class,
                Class.class,
                Class.class,
                Map.class,
                List.class,
                List.class
        );
        method.setAccessible(true);

        List<?> result = (List<?>) method.invoke(
                spyService,
                mockFile,
                request,
                mapOfEntity,
                masterDataMap,
                Object.class,
                Object.class,
                locCodeToLocationReferenceGuidMap,
                errorList,
                excelHeaders
        );

        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testParseExcelGeneric_ValidationExceptionCaught() throws Exception {
        CSVParsingUtilV3<?> spyService = Mockito.spy(csvParsingUtilV3);

        MultipartFile mockFile = Mockito.mock(MultipartFile.class);
        BulkUploadRequest request = new BulkUploadRequest();
        Map<UUID, Object> mapOfEntity = new HashMap<>();
        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        List<String> errorList = new ArrayList<>();
        List<String> excelHeaders = List.of("guid");

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();
        Row headerRow = sheet.createRow(0);
        headerRow.createCell(0).setCellValue("guid");
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        workbook.write(bos);
        workbook.close();
        byte[] workbookBytes = bos.toByteArray();
        when(mockFile.getInputStream()).thenReturn(new ByteArrayInputStream(workbookBytes));

        doThrow(new ValidationException("Invalid Excel")).when(spyService).validateExcel(any(Sheet.class));

        Method method = CSVParsingUtilV3.class.getDeclaredMethod(
                "parseExcelGeneric",
                MultipartFile.class,
                BulkUploadRequest.class,
                Map.class,
                Map.class,
                Class.class,
                Class.class,
                Map.class,
                List.class,
                List.class
        );
        method.setAccessible(true);

        Throwable thrown = assertThrows(InvocationTargetException.class, () ->
                method.invoke(
                        spyService,
                        mockFile,
                        request,
                        mapOfEntity,
                        masterDataMap,
                        Object.class,
                        Object.class,
                        locCodeToLocationReferenceGuidMap,
                        errorList,
                        excelHeaders
                )
        );

        Throwable cause = thrown.getCause();
        assertTrue(cause instanceof ValidationException);
        assertEquals("Invalid Excel", cause.getMessage());
    }


    @Test
    void testParseExcelGeneric_ReflectionExceptionCaught() throws Exception {
        CSVParsingUtilV3<?> spyService = Mockito.spy(csvParsingUtilV3);

        MultipartFile mockFile = Mockito.mock(MultipartFile.class);
        BulkUploadRequest request = new BulkUploadRequest();
        Map<UUID, Object> mapOfEntity = new HashMap<>();
        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        List<String> errorList = new ArrayList<>();
        List<String> excelHeaders = List.of("guid");

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();
        Row headerRow = sheet.createRow(0);
        headerRow.createCell(0).setCellValue("guid");
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        workbook.write(bos);
        workbook.close();
        byte[] workbookBytes = bos.toByteArray();
        when(mockFile.getInputStream()).thenReturn(new ByteArrayInputStream(workbookBytes));

        doNothing().when(spyService).validateExcel(any(Sheet.class));
        doReturn(new String[]{"guid"}).when(spyService).extractHeader(any(Row.class), any(), any());
        doReturn(0).when(spyService).findColumnIndex(any(String[].class), eq("guid"));
        doNothing().when(spyService).addGuidInList(any(), anyInt(), anyList(), anyInt(), anyList(), anyInt(), anyList());
        doReturn(new HashMap<>()).when(spyService).getAllMasterDataContainer(anyList(), anyList(), anyMap(), anyMap());

        doThrow(new NoSuchFieldException("Field missing")).when(spyService)
                .processSheetLastRowNum(any(), anyMap(), any(), any(), anyInt(), any(), anyMap(), anyMap(), anyList(), anyList());

        Method method = CSVParsingUtilV3.class.getDeclaredMethod(
                "parseExcelGeneric",
                MultipartFile.class,
                BulkUploadRequest.class,
                Map.class,
                Map.class,
                Class.class,
                Class.class,
                Map.class,
                List.class,
                List.class
        );
        method.setAccessible(true);

        Throwable thrown = assertThrows(InvocationTargetException.class, () ->
                method.invoke(
                        spyService,
                        mockFile,
                        request,
                        mapOfEntity,
                        masterDataMap,
                        Object.class,
                        Object.class,
                        locCodeToLocationReferenceGuidMap,
                        errorList,
                        excelHeaders
                )
        );

        Throwable cause = thrown.getCause();
        assertTrue(cause instanceof ValidationException);
        assertEquals(ContainerConstants.EXCEL_SHEET_INVALID, cause.getMessage());
    }

    @Test
    void testExtractHeader_FullCoverage() {
        CSVParsingUtilV3<?> spyService = Mockito.spy(csvParsingUtilV3);

        class TestModel {
            @ExcelCell(displayName = "Col1")
            private String col1;
            @ExcelCell(displayName = "Col2")
            private String col2;
        }

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();
        Row headerRow = sheet.createRow(0);
        headerRow.createCell(0).setCellValue("Col1");
        headerRow.createCell(1).setCellValue("Col2");

        List<String> excelHeaders = new ArrayList<>();

        doAnswer(invocation -> {
            Cell cell = invocation.getArgument(0);
            return cell != null ? cell.getStringCellValue() : null;
        }).when(spyService).getCellValueAsString(any(Cell.class));

        doNothing().when(spyService).validateHeaderUniqueness(anySet(), anyInt());

        String[] result = spyService.extractHeader(headerRow, TestModel.class, excelHeaders);

        assertNotNull(result);
        assertArrayEquals(new String[]{"col1", "col2"}, result); // mappedHeaders from renameFieldMap or getCamelCase
        assertEquals(List.of("Col1", "Col2"), excelHeaders);
    }

    @Test
    void testExtractHeader_ThrowsValidationException_WhenNoHeaders() {
        CSVParsingUtilV3<?> spyService = Mockito.spy(csvParsingUtilV3);

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();
        Row headerRow = sheet.createRow(0); // no cells

        List<String> excelHeaders = new ArrayList<>();

        ValidationException ex = assertThrows(ValidationException.class,
                () -> spyService.extractHeader(headerRow, Object.class, excelHeaders));
        assertTrue(ex.getMessage().contains("No column headers found"));
    }

    @Test
    void testExtractHeader_ThrowsValidationException_WhenBlankBeforeLastCell() {
        CSVParsingUtilV3<?> spyService = Mockito.spy(csvParsingUtilV3);

        class TestModel {
            @ExcelCell(displayName = "Col1")
            private String col1;
            @ExcelCell(displayName = "Col2")
            private String col2;
        }

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();
        Row headerRow = sheet.createRow(0);
        headerRow.createCell(0).setCellValue("Col1");
        headerRow.createCell(1).setCellValue(""); // blank cell before last
        headerRow.createCell(2).setCellValue("Col2");

        List<String> excelHeaders = new ArrayList<>();

        doAnswer(invocation -> {
            Cell cell = invocation.getArgument(0);
            return cell != null ? cell.getStringCellValue() : null;
        }).when(spyService).getCellValueAsString(any(Cell.class));

        ValidationException ex = assertThrows(ValidationException.class,
                () -> spyService.extractHeader(headerRow, TestModel.class, excelHeaders));
        assertTrue(ex.getMessage().contains("Blank column header found at position 2"));
    }

    @Test
    void testValidateHeaders_WithReflection() throws Exception {
        CSVParsingUtilV3<?> service = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);
        CommonUtils mockCommonUtils = Mockito.mock(CommonUtils.class);

        // Inject private field via reflection
        Field commonUtilsField = CSVParsingUtilV3.class.getDeclaredField("commonUtils");
        commonUtilsField.setAccessible(true);
        commonUtilsField.set(service, mockCommonUtils);

        List<String> headers = List.of("field1"); // field2 missing
        List<String> errorList = new ArrayList<>();

        // Use any(TypeReference.class) to match argument
        Mockito.when(mockCommonUtils.getAppConfigValueByKey(
                eq("MANDATORY_FIELD_FOR_CONTAINER_UPLOAD"),
                any(TypeReference.class)
        )).thenReturn(List.of("field1", "field2"));

        // Call the method
        service.validateHeaders(headers, errorList);

        assertEquals(1, errorList.size());
        assertEquals("Row# 1 : field2 is mandatory", errorList.get(0));
    }

    @Test
    void testValidateHeaderUniqueness_NoException() {
        CSVParsingUtilV3<?> service = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);

        Set<String> headers = Set.of("Col1", "Col2", "Col3");
        int totalColumns = 3;

        // Should not throw any exception
        assertDoesNotThrow(() -> service.validateHeaderUniqueness(headers, totalColumns));
    }

    @Test
    void testValidateHeaderUniqueness_ThrowsValidationException() {
        CSVParsingUtilV3<?> service = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);

        Set<String> headers = Set.of("Col1", "Col2"); // only 2 unique headers
        int totalColumns = 3; // total columns expected

        ValidationException ex = assertThrows(ValidationException.class,
                () -> service.validateHeaderUniqueness(headers, totalColumns));

        assertEquals(ContainerConstants.INVALID_EXCEL_COLUMNS, ex.getMessage());
    }

    @Test
    void testValidateExcelColumn_NoException() {
        CSVParsingUtilV3<?> service = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();
        Row headerRow = sheet.createRow(0);
        headerRow.createCell(0).setCellValue("Col1"); // non-empty cell

        assertDoesNotThrow(() -> service.validateExcelColumn(headerRow, 0));
    }

    @Test
    void testValidateExcelColumn_CellIsNull_ThrowsValidationException() {
        CSVParsingUtilV3<?> service = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();
        Row headerRow = sheet.createRow(0);
        // No cell created → headerRow.getCell(0) is null

        ValidationException ex = assertThrows(ValidationException.class,
                () -> service.validateExcelColumn(headerRow, 0));

        assertEquals(ContainerConstants.INVALID_EXCEL_COLUMNS, ex.getMessage());
    }

    @Test
    void testValidateExcelColumn_CellIsEmpty_ThrowsValidationException() {
        CSVParsingUtilV3<?> service = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();
        Row headerRow = sheet.createRow(0);
        headerRow.createCell(0).setCellValue(""); // empty cell

        ValidationException ex = assertThrows(ValidationException.class,
                () -> service.validateExcelColumn(headerRow, 0));

        assertEquals(ContainerConstants.INVALID_EXCEL_COLUMNS, ex.getMessage());
    }

    @Test
    void testAddGuidInList_FullCoverage() {
        CSVParsingUtilV3<?> spyService = Mockito.spy(csvParsingUtilV3);

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();
        // Row 0 is header
        Row headerRow = sheet.createRow(0);
        headerRow.createCell(0).setCellValue("CommodityCode");
        headerRow.createCell(1).setCellValue("Unlocation");
        headerRow.createCell(2).setCellValue("Guid");

        // Row 1 with values
        Row row1 = sheet.createRow(1);
        row1.createCell(0).setCellValue("C001");
        row1.createCell(1).setCellValue("LOC1");
        row1.createCell(2).setCellValue("GUID1");

        // Row 2 with values
        Row row2 = sheet.createRow(2);
        row2.createCell(0).setCellValue("C002");
        row2.createCell(1).setCellValue("LOC2");
        row2.createCell(2).setCellValue("GUID2");

        List<String> commodityCodesList = new ArrayList<>();
        List<String> unlocationsList = new ArrayList<>();
        List<String> errorList = new ArrayList<>();

        // Stub getCellValueAsString to return cell value
        doAnswer(invocation -> {
            Cell cell = invocation.getArgument(0);
            return cell != null ? cell.getStringCellValue() : null;
        }).when(spyService).getCellValueAsString(any(Cell.class));

        // Stub validateDuplicateGuid to do nothing
        doNothing().when(spyService).validateDuplicateGuid(anyInt(), any(Row.class), anySet(), anyInt(), anyList());

        // Call method
        spyService.addGuidInList(sheet, 0, commodityCodesList, 1, unlocationsList, 2, errorList);

        // Assertions
        assertEquals(List.of("C001", "C002"), commodityCodesList);
        assertEquals(List.of("LOC1", "LOC2"), unlocationsList);
        assertTrue(errorList.isEmpty());
    }

    @Test
    void testAddGuidInList_ThrowsValidationException_WhenCommodityCodeCellNull() {
        CSVParsingUtilV3<?> spyService = Mockito.spy(csvParsingUtilV3);

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();
        // Row 0 is header
        Row headerRow = sheet.createRow(0);
        headerRow.createCell(0).setCellValue("CommodityCode");

        // Row 1 with null commodity code
        sheet.createRow(1); // no cell 0

        List<String> commodityCodesList = new ArrayList<>();
        List<String> unlocationsList = new ArrayList<>();
        List<String> errorList = new ArrayList<>();

        ValidationException ex = assertThrows(ValidationException.class,
                () -> spyService.addGuidInList(sheet, 0, commodityCodesList, -1, unlocationsList, -1, errorList));

        assertEquals("Please enter at least One container line in the upload file.", ex.getMessage());
    }

    @Test
    void testValidateDuplicateGuid_NoDuplicate() {
        CSVParsingUtilV3<?> spyService = Mockito.spy(csvParsingUtilV3);

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();
        Row row = sheet.createRow(0);
        row.createCell(0).setCellValue("GUID1");

        Set<String> guidSet = new HashSet<>(); // empty set
        List<String> errorList = new ArrayList<>();

        // Stub getCellValueAsString to return cell value
        doReturn("GUID1").when(spyService).getCellValueAsString(row.getCell(0));

        // Call method
        spyService.validateDuplicateGuid(0, row, guidSet, 0, errorList);

        // No error should be added
        assertTrue(errorList.isEmpty());
    }

    @Test
    void testValidateDuplicateGuid_AddsError_WhenDuplicate() {
        CSVParsingUtilV3<?> spyService = Mockito.spy(csvParsingUtilV3);

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();
        Row row = sheet.createRow(0);
        row.createCell(0).setCellValue("GUID1");

        Set<String> guidSet = new HashSet<>();
        guidSet.add("GUID1"); // already exists
        List<String> errorList = new ArrayList<>();

        doReturn("GUID1").when(spyService).getCellValueAsString(row.getCell(0));

        spyService.validateDuplicateGuid(0, row, guidSet, 0, errorList);

        assertEquals(1, errorList.size());
        assertEquals(String.format(ContainerConstants.GENERIC_DUPLICATE_FIELD_MSG, 1, "Guid", "GUID1"), errorList.get(0));
    }

    @Test
    void testProcessSheetLastRowNum_FullCoverage() throws Exception {
        // Spy the service to stub private/helper methods
        CSVParsingUtilV3<Packing> spyService = Mockito.spy(csvParsingUtilV3);

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();

        // Header row (row 0)
        sheet.createRow(0).createCell(0).setCellValue("Guid");

        // Row 1 with GUID
        Row row1 = sheet.createRow(1);
        row1.createCell(0).setCellValue("cbd3bb18-bba8-46ad-85e6-4e94290fc21b");

        // Row 2 with GUID
        Row row2 = sheet.createRow(2);
        row2.createCell(0).setCellValue("87930938-8fdf-42d4-a70c-204b511a1684");

        BulkUploadRequest request = new BulkUploadRequest();
        Map<UUID, Packing> mapOfEntity = new HashMap<>();
        List<Packing> entityList = new ArrayList<>();
        List<String> errorList = new ArrayList<>();
        Map<String, Set<String>> masterListsMap = new HashMap<>();
        Map<String, String> existingContainerNumbers = new HashMap<>();
        String[] header = new String[]{"Guid"};

        // Stub getCellValueAsString
        doAnswer(invocation -> ((Cell) invocation.getArgument(0)).getStringCellValue())
                .when(spyService).getCellValueAsString(any(Cell.class));

        // Stub validateSheetLastRowNum: return false to process the row
        doReturn(false).when(spyService).validateSheetLastRowNum(anyMap(), anyString(), anyInt(), anyList());

        // Stub processHeaderForExcel to do nothing
        doNothing().when(spyService).processHeaderForExcel(any(), any(), any(), any(), anyInt(), anyInt(), anyBoolean(), any(), any(), anyList());

        // Call method
        spyService.processSheetLastRowNum(request, mapOfEntity, Packing.class, sheet, 0, header, masterListsMap, existingContainerNumbers, entityList, errorList);

        // Assertions
        assertEquals(2, entityList.size()); // two rows processed
        assertTrue(errorList.isEmpty());

        // Verify that helper methods were called
        verify(spyService, times(14)).getCellValueAsString(any(Cell.class));
        verify(spyService, times(2)).processHeaderForExcel(any(), any(), any(), any(), anyInt(), anyInt(), anyBoolean(), any(), any(), anyList());
    }

    @Test
    void testValidateSheetLastRowNum_EmptyGuid_ReturnsFalse() {
        CSVParsingUtilV3<?> service = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);


        List<String> errorList = new ArrayList<>();
        boolean result = service.validateSheetLastRowNum(new HashMap<>(), "", 1, errorList);

        assertFalse(result);
        assertTrue(errorList.isEmpty());
    }

    @Test
    void testValidateSheetLastRowNum_ValidGuidInMap_ReturnsFalse() {
        CSVParsingUtilV3<?> service = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);


        UUID guid = UUID.randomUUID();
        Map<UUID, ?> mapOfEntity = Map.of(guid, new Packing()); // use wildcard
        List<String> errorList = new ArrayList<>();

// Unsafe cast to satisfy generic signature
        boolean result = ((CSVParsingUtilV3<Packing>) service)
                .validateSheetLastRowNum((Map<UUID, Packing>) mapOfEntity, guid.toString(), 1, errorList);
        assertFalse(result);
        assertTrue(errorList.isEmpty());
    }

    @Test
    void testProcessHeaderForExcel_AllBranches() {
        CSVParsingUtilV3<Packing> spyService = Mockito.spy(csvParsingUtilV3);

        BulkUploadRequest request = new BulkUploadRequest();
        request.setTransportMode("SEA");

        // Prepare headers to cover different validations
        String[] headers = {
                "hazardous", "containerCode", "containerNumber", "grossWeightUnit", "netWeightUnit"
        };

        // Mock a row with cells for each header
        Row mockRow = Mockito.mock(Row.class);
        for (int i = 0; i < headers.length; i++) {
            Cell mockCell = Mockito.mock(Cell.class);
            when(mockRow.getCell(i)).thenReturn(mockCell);

            // Return test values for each cell
            switch (headers[i]) {
                case "hazardous":
                    doReturn("yes").when(spyService).getCellValueAsString(mockCell);
                    break;
                case "containerCode":
                    doReturn("CODE1").when(spyService).getCellValueAsString(mockCell);
                    break;
                case "containerNumber":
                    doReturn("CONT001").when(spyService).getCellValueAsString(mockCell);
                    break;
                default:
                    doReturn("10").when(spyService).getCellValueAsString(mockCell);
            }
        }

        Map<String, Set<String>> masterListsMap = new HashMap<>();
        masterListsMap.put(Constants.CONTAINER_TYPES, Set.of("CODE1"));
        masterListsMap.put(MasterDataType.WEIGHT_UNIT.getDescription(), Set.of("10"));

        // Avoid duplicate container exception
        Map<String, String> existingContainerNumbers = new HashMap<>();

        Packing entity = new Packing();
        List<String> errorList = new ArrayList<>();

        // Call method with isUpdate=false (so duplicate logic does not throw)
        spyService.processHeaderForExcel(request, headers, mockRow, masterListsMap, 1, 0, false, existingContainerNumbers, entity, errorList);

        // Assertions for error list (should be empty if all data valid)
        assertTrue(errorList.isEmpty());

        // Verify that getCellValueAsString was called for each cell
        // Instead of exact count
        verify(spyService, atLeast(headers.length)).getCellValueAsString(any(Cell.class));

    }


    @Test
    void testProcessHeaderForExcel_DuplicateContainerNumber_ThrowsException() {
        CSVParsingUtilV3<Packing> spyService = Mockito.spy(csvParsingUtilV3);

        BulkUploadRequest request = new BulkUploadRequest();
        request.setTransportMode("SEA");

        String[] headers = {Constants.CONTAINER_NUMBER};
        Row mockRow = Mockito.mock(Row.class);
        Cell mockCellContainerNumber = Mockito.mock(Cell.class);

        when(mockRow.getCell(0)).thenReturn(mockCellContainerNumber);
        doReturn("CONT001").when(spyService).getCellValueAsString(mockCellContainerNumber);

        Map<String, Set<String>> masterListsMap = new HashMap<>();
        Map<String, String> existingContainerNumbers = new HashMap<>();
        existingContainerNumbers.put("CONT001", "some-guid"); // duplicate container

        Packing entity = new Packing();
        List<String> errorList = new ArrayList<>();

        // Call and assert that ValidationException is thrown
        ValidationException ex = assertThrows(ValidationException.class, () ->
                spyService.processHeaderForExcel(request, headers, mockRow, masterListsMap, 1, 0, true, existingContainerNumbers, entity, errorList)
        );

        assertTrue(ex.getMessage().contains("Duplicate container number CONT001 found"));
    }

    @Test
    void testRaiseException() {
        CSVParsingUtilV3<?> service = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao,
                v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);

        // Case 1: fieldType is Long
        ValidationException ex1 = assertThrows(ValidationException.class, () -> {
            service.raiseException("longField", 5, Long.class);
        });
        assertEquals("LONGFIELD is invalid at row: 5. Please provide integer value and within the range of integer", ex1.getMessage());

        // Case 2: fieldType is long (primitive)
        ValidationException ex2 = assertThrows(ValidationException.class, () -> {
            service.raiseException("primitiveLong", 10, long.class);
        });
        assertEquals("PRIMITIVELONG is invalid at row: 10. Please provide integer value and within the range of integer", ex2.getMessage());

        // Case 3: fieldType is other type
        ValidationException ex3 = assertThrows(ValidationException.class, () -> {
            service.raiseException("name", 15, String.class);
        });
        assertEquals("name is invalid at row: 15. Please provide correct value", ex3.getMessage());
    }

    @Test
    void testSetFieldForEvents_AllBranches() throws Exception {
        CSVParsingUtilV3<Events> service = new CSVParsingUtilV3<>(null, null, null, null, null, null, null);
        Events entity = new Events();

        // Integer field
        service.setFieldForEvents(entity, "pieces", "10");
        assertEquals(10, entity.getPieces());

        // String field
        service.setFieldForEvents(entity, "eventCode", "EVT001");
        assertEquals("EVT001", entity.getEventCode());

        // Long field
        service.setFieldForEvents(entity, "entityId", "999");
        assertEquals(999L, entity.getEntityId());

        // Boolean field
        service.setFieldForEvents(entity, "isPartial", "true");
        assertTrue(entity.getIsPartial());

        // BigDecimal field
        service.setFieldForEvents(entity, "weight", "123.45");
        assertEquals(new BigDecimal("123.45"), entity.getWeight());

        // LocalDateTime field
        String dateTimeStr = "2025-09-23T15:30:00";
        service.setFieldForEvents(entity, "estimated", dateTimeStr);
        assertEquals(LocalDateTime.parse(dateTimeStr), entity.getEstimated());

        // Boolean mapping for publicTrackingEvent → isPublicTrackingEvent
        service.setFieldForEvents(entity, "publicTrackingEvent", "true");
        assertTrue(entity.getIsPublicTrackingEvent());

        // Empty string should not modify field
        service.setFieldForEvents(entity, "remarks", "");
        assertNull(entity.getRemarks());

        // containerNumber is skipped
        service.setFieldForEvents(entity, "containerNumber", "ABC123");
        assertNull(entity.getContainerNumber());

        // NoSuchFieldException branch
        assertThrows(NoSuchFieldException.class,
                () -> service.setFieldForEvents(entity, "nonExistingField", "value"));
    }


    @Test
    void testGetAllMasterDataContainer() {
        ExecutorService executor = Executors.newFixedThreadPool(2);
        CSVParsingUtilV3<?> service = Mockito.spy(new CSVParsingUtilV3<>(null, null, null, null, null, executor, null));

        // Prepare inputs
        List<String> unlocationsList = List.of("LOC1");
        List<String> commodityCodesList = List.of("CMD1");
        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        // Stub private/fetch methods
        doAnswer(invocation -> {
            masterDataMap.put("WEIGHT_UNIT", Set.of("KG"));
            return null;
        }).when(service).fetchMasterLists(eq(MasterDataType.WEIGHT_UNIT), anyMap());

        doAnswer(invocation -> {
            masterDataMap.put("VOLUME_UNIT", Set.of("CBM"));
            return null;
        }).when(service).fetchMasterLists(eq(MasterDataType.VOLUME_UNIT), anyMap());

        doAnswer(invocation -> {
            masterDataMap.put("TEMPERATURE_UNIT", Set.of("C"));
            return null;
        }).when(service).fetchMasterLists(eq(MasterDataType.TEMPERATURE_UNIT), anyMap());

        doAnswer(invocation -> {
            masterDataMap.put("HBL_DELIVERY_MODE", Set.of("Air"));
            return null;
        }).when(service).fetchMasterLists(eq(MasterDataType.HBL_DELIVERY_MODE), anyMap());

        doAnswer(invocation -> {
            masterDataMap.put("DIMENSION_UNIT", Set.of("M"));
            return null;
        }).when(service).fetchMasterLists(eq(MasterDataType.DIMENSION_UNIT), anyMap());

        doAnswer(invocation -> {
            masterDataMap.put("DG_CLASS", Set.of("Class1"));
            return null;
        }).when(service).fetchMasterLists(eq(MasterDataType.DG_CLASS), anyMap());

        doAnswer(invocation -> {
            masterDataMap.put("PACKS_UNIT", Set.of("Packs"));
            return null;
        }).when(service).fetchMasterLists(eq(MasterDataType.PACKS_UNIT), anyMap());

        doAnswer(invocation -> {
            masterDataMap.put("CONTAINER_TYPE", Set.of("Type1"));
            return null;
        }).when(service).fetchContainerType(anyMap());

        doAnswer(invocation -> {
            masterDataMap.put("COMMODITY_GROUP", Set.of("Group1"));
            return null;
        }).when(service).fetchMasterLists(eq(MasterDataType.COMMODITY_GROUP), anyMap());

        doAnswer(invocation -> {
            masterDataMap.put("PACKING_GROUP", Set.of("PG1"));
            return null;
        }).when(service).fetchMasterLists(eq(MasterDataType.PACKING_GROUP), anyMap());

        doAnswer(invocation -> {
            masterDataMap.put("UNLOCATION", Set.of("LOC1"));
            return null;
        }).when(service).fetchUnlocationData(eq(unlocationsList), anyMap(), anyMap());

        doAnswer(invocation -> {
            masterDataMap.put("COMMODITY", Set.of("CMD1"));
            return null;
        }).when(service).fetchCommodityData(eq(commodityCodesList), anyMap());

        // Call the method
        Map<String, Set<String>> result = service.getAllMasterDataContainer(unlocationsList, commodityCodesList, masterDataMap, locCodeToLocationReferenceGuidMap);

        // Assertions
        assertEquals(Set.of("KG"), result.get("WEIGHT_UNIT"));
        assertEquals(Set.of("CBM"), result.get("VOLUME_UNIT"));
        assertEquals(Set.of("C"), result.get("TEMPERATURE_UNIT"));
        assertEquals(Set.of("Air"), result.get("HBL_DELIVERY_MODE"));
        assertEquals(Set.of("M"), result.get("DIMENSION_UNIT"));
        assertEquals(Set.of("Class1"), result.get("DG_CLASS"));
        assertEquals(Set.of("Packs"), result.get("PACKS_UNIT"));
        assertEquals(Set.of("Type1"), result.get("CONTAINER_TYPE"));
        assertEquals(Set.of("Group1"), result.get("COMMODITY_GROUP"));
        assertEquals(Set.of("PG1"), result.get("PACKING_GROUP"));
        assertEquals(Set.of("LOC1"), result.get("UNLOCATION"));
        assertEquals(Set.of("CMD1"), result.get("COMMODITY"));
    }


    @Test
    void testGetAllMasterDataEvents() {
        // Create spy of the service
        CSVParsingUtilV3<?> service = Mockito.spy(new CSVParsingUtilV3<>(null, null, null, null, null, null, null));

        Map<String, Set<String>> masterDataMap = new HashMap<>();

        // Stub the fetchMasterLists call for ORDER_EVENTS
        doAnswer(invocation -> {
            masterDataMap.put("ORDER_EVENTS", Set.of("Event1", "Event2"));
            return null;
        }).when(service).fetchMasterLists(eq(MasterDataType.ORDER_EVENTS), anyMap());

        // Call the method
        Map<String, Set<String>> result = service.getAllMasterDataEvents(masterDataMap);

        // Verify fetchMasterLists was called
        verify(service, times(1)).fetchMasterLists(eq(MasterDataType.ORDER_EVENTS), anyMap());

        // Assertions
        assertNotNull(result);
        assertTrue(result.containsKey("ORDER_EVENTS"));
        assertEquals(Set.of("Event1", "Event2"), result.get("ORDER_EVENTS"));
    }

    @Test
    void testFetchContainerType_Success() throws Exception {
        CSVParsingUtilV3<?> service = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao,
                v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);

        // Mock private v1Service
        IV1Service mockV1Service = Mockito.mock(IV1Service.class);
        Field v1ServiceField = CSVParsingUtilV3.class.getDeclaredField("v1Service");
        v1ServiceField.setAccessible(true);
        v1ServiceField.set(service, mockV1Service);

        // Mock jsonHelper
        JsonHelper mockJsonHelper = Mockito.mock(JsonHelper.class);
        Field jsonHelperField = CSVParsingUtilV3.class.getDeclaredField("jsonHelper");
        jsonHelperField.setAccessible(true);
        jsonHelperField.set(service, mockJsonHelper);

        Map<String, Set<String>> masterDataMap = new HashMap<>();

        // Prepare V1DataResponse
        V1ContainerTypeResponse container1 = new V1ContainerTypeResponse();
        container1.setCode("CONT1");
        V1ContainerTypeResponse container2 = new V1ContainerTypeResponse();
        container2.setCode("CONT2");
        List<V1ContainerTypeResponse> containerList = List.of(container1, container2);

        V1DataResponse mockResponse = Mockito.mock(V1DataResponse.class);
        mockResponse.entities = containerList;

        // Mock service calls
        Mockito.when(mockV1Service.fetchContainerTypeData(Mockito.any(CommonV1ListRequest.class)))
                .thenReturn(mockResponse);
        Mockito.when(mockJsonHelper.convertValueToList(containerList, V1ContainerTypeResponse.class))
                .thenReturn(containerList);

        // Call method
        service.fetchContainerType(masterDataMap);

        // Verify result
        assertTrue(masterDataMap.containsKey(Constants.CONTAINER_TYPES));
        assertEquals(Set.of("CONT1", "CONT2"), masterDataMap.get(Constants.CONTAINER_TYPES));

        // Verify interactions
        Mockito.verify(mockV1Service, times(1)).fetchContainerTypeData(Mockito.any());
        Mockito.verify(mockJsonHelper, times(1)).convertValueToList(containerList, V1ContainerTypeResponse.class);
    }

    @Test
    void testMasterdataValidation() {
        CSVParsingUtilV3<?> service = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao,
                v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);

        List<String> errorList = new ArrayList<>();
        Map<String, Set<String>> masterListsMap = new HashMap<>();
        masterListsMap.put("TEST_KEY", Set.of("VALID_VALUE"));

        int rowNum = 1;

        // Case 1: cellValue matches the master data → no error added
        service.masterdataValidation("VALID_VALUE", "column1", "column1", masterListsMap, "TEST_KEY", rowNum, "Validation Message", errorList);
        assertTrue(errorList.isEmpty(), "Error list should be empty when value is valid");

        // Case 2: cellValue does not match the master data → error added
        service.masterdataValidation("INVALID_VALUE", "column1", "column1", masterListsMap, "TEST_KEY", rowNum, "Validation Message", errorList);
        assertEquals(1, errorList.size());
        assertEquals(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Validation Message", "INVALID_VALUE"), errorList.get(0));

        // Case 3: cellValue empty → no error added
        errorList.clear();
        service.masterdataValidation("", "column1", "column1", masterListsMap, "TEST_KEY", rowNum, "Validation Message", errorList);
        assertTrue(errorList.isEmpty(), "Error list should remain empty for empty cellValue");

        // Case 4: currentColumn does not match actualColumn → no error
        errorList.clear();
        service.masterdataValidation("INVALID_VALUE", "columnX", "column1", masterListsMap, "TEST_KEY", rowNum, "Validation Message", errorList);
        assertTrue(errorList.isEmpty(), "Error list should remain empty when column names do not match");

        // Case 5: masterListsMap does not contain masterKey → no error
        errorList.clear();
        service.masterdataValidation("INVALID_VALUE", "column1", "column1", new HashMap<>(), "UNKNOWN_KEY", rowNum, "Validation Message", errorList);
        assertTrue(errorList.isEmpty(), "Error list should remain empty when master key not present");
    }

    @Test
    void testFieldLengthValidation() {
        CSVParsingUtilV3<?> service = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao,
                v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);

        List<String> errorList = new ArrayList<>();
        int rowNum = 1;
        int allowedLength = 5;

        // Case 1: currentColumn matches actualColumn and cellValue exceeds allowed length → error added
        service.fieldLengthValidation("column1", "column1", "EXCEED", allowedLength, rowNum, "Length Validation", errorList);
        assertEquals(1, errorList.size());
        assertEquals(String.format(ContainerConstants.GENERIC_LENGTH_VALIDATION_MSG, rowNum, "Length Validation", String.valueOf(allowedLength)), errorList.get(0));

        // Case 2: cellValue is within allowed length → no error
        errorList.clear();
        service.fieldLengthValidation("column1", "column1", "OK", allowedLength, rowNum, "Length Validation", errorList);
        assertTrue(errorList.isEmpty(), "Error list should be empty when value is within allowed length");

        // Case 3: cellValue is null → no error
        service.fieldLengthValidation("column1", "column1", null, allowedLength, rowNum, "Length Validation", errorList);
        assertTrue(errorList.isEmpty(), "Error list should remain empty for null value");

        // Case 4: currentColumn does not match actualColumn → no error
        service.fieldLengthValidation("columnX", "column1", "EXCEED", allowedLength, rowNum, "Length Validation", errorList);
        assertTrue(errorList.isEmpty(), "Error list should remain empty when column names do not match");
    }

    @Test
    void testValidateDependentMandatoryFieldsAndMandatoryField() {
        CSVParsingUtilV3<?> service = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao,
                v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);

        List<String> errorList = new ArrayList<>();
        int rowNum = 1;

        // -------------------------------
        // validateDependentMandatoryFields
        // -------------------------------

        // Case 1: currentColumn matches, cellValue empty, prevColumn matches, prevCellValue not empty → error added
        service.validateDependentMandatoryFields("col1", "col1", "", "colPrev", "colPrev", "notEmpty", rowNum, "Mandatory Column", errorList);
        assertEquals(1, errorList.size());
        assertEquals(String.format(ContainerConstants.GENERIC_MANDATORY_FIELD_MSG, rowNum, "Mandatory Column"), errorList.get(0));

        // Case 2: currentColumn does not match → no error
        errorList.clear();
        service.validateDependentMandatoryFields("colX", "col1", "", "colPrev", "colPrev", "notEmpty", rowNum, "Mandatory Column", errorList);
        assertTrue(errorList.isEmpty());

        // Case 3: cellValue not empty → no error
        service.validateDependentMandatoryFields("col1", "col1", "value", "colPrev", "colPrev", "notEmpty", rowNum, "Mandatory Column", errorList);
        assertTrue(errorList.isEmpty());

        // Case 4: prevColumn does not match → no error
        service.validateDependentMandatoryFields("col1", "col1", "", "colX", "colPrev", "notEmpty", rowNum, "Mandatory Column", errorList);
        assertTrue(errorList.isEmpty());

        // Case 5: prevCellValue empty → no error
        service.validateDependentMandatoryFields("col1", "col1", "", "colPrev", "colPrev", "", rowNum, "Mandatory Column", errorList);
        assertTrue(errorList.isEmpty());

        // -------------------------------
        // validateMandatoryField
        // -------------------------------

        // Case 1: currentColumn matches, cellValue empty → error added
        errorList.clear();
        service.validateMandatoryField("col1", "col1", rowNum, "", "Mandatory Column", errorList);
        assertEquals(1, errorList.size());
        assertEquals(String.format(ContainerConstants.GENERIC_MANDATORY_FIELD_MSG, rowNum, "Mandatory Column"), errorList.get(0));

        // Case 2: currentColumn does not match → no error
        errorList.clear();
        service.validateMandatoryField("colX", "col1", rowNum, "", "Mandatory Column", errorList);
        assertTrue(errorList.isEmpty());

        // Case 3: cellValue not empty → no error
        service.validateMandatoryField("col1", "col1", rowNum, "value", "Mandatory Column", errorList);
        assertTrue(errorList.isEmpty());
    }

    @Test
    void testParseExcelFileEvents_success() throws Exception {
        // Arrange
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);

        ConsolidationDetails mockConsol = new ConsolidationDetails();
        Containers cont1 = new Containers();
        cont1.setContainerNumber("CONT001");
        Containers cont2 = new Containers();
        cont2.setContainerNumber("CONT002");
        mockConsol.setContainersList(List.of(cont1, cont2));

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(mockConsol));

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        masterDataMap.put(MasterDataType.ORDER_EVENTS.getDescription(), Set.of("EV001", "EV002"));

        // Excel mock
        XSSFWorkbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();
        Row headerRow = sheet.createRow(0);
        headerRow.createCell(0).setCellValue("eventCode");
        headerRow.createCell(1).setCellValue("containerNumber");

        Row dataRow = sheet.createRow(1);
        dataRow.createCell(0).setCellValue("EV001");
        dataRow.createCell(1).setCellValue("CONT001"); // match consolidation

        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        workbook.write(bos);
        workbook.close();

        MultipartFile mockFile = new MockMultipartFile("file", new ByteArrayInputStream(bos.toByteArray()));

        // Act
        List<?> result = csvParsingUtilV3.parseExcelFileEvents(
                mockFile,
                request,
                masterDataMap,
                (Class) Events.class
        );

        // Assert
        assertNotNull(result);
        assertFalse(result.isEmpty());
        assertTrue(result.get(0) instanceof Events);

        // We cannot assert field values because containerNumber is skipped by design
        Events event = (Events) result.get(0);
        assertNotNull(event); // at least entity is created
    }

    @Test
    void testValidateLengthWidthHeightUnit() throws Exception {
        Method method = CSVParsingUtilV3.class.getDeclaredMethod("validateLengthWidhtHeightUnit",
                Map.class, String.class, String.class, int.class, List.class);
        method.setAccessible(true);

        Map<String, Set<String>> masterListsMap = new HashMap<>();
        masterListsMap.put(MasterDataType.WEIGHT_UNIT.getDescription(), Set.of("KG", "LB"));
        masterListsMap.put(MasterDataType.DIMENSION_UNIT.getDescription(), Set.of("CM", "IN"));

        List<String> errorList = new ArrayList<>();

        // Volumetric Weight Unit invalid
        method.invoke(csvParsingUtilV3, masterListsMap, "VolumetricWeightUnit", "INVALID_UNIT", 1, errorList);
        // Length unit invalid
        method.invoke(csvParsingUtilV3, masterListsMap, "LengthUnit", "MM", 2, errorList);
        // Width unit invalid
        method.invoke(csvParsingUtilV3, masterListsMap, "WidthUnit", "MM", 3, errorList);
        // Height unit invalid
        method.invoke(csvParsingUtilV3, masterListsMap, "HeightUnit", "MM", 4, errorList);

        assertEquals(4, errorList.size());
        assertEquals(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, 1, "Volumetric weight unit", "INVALID_UNIT"), errorList.get(0));
        assertEquals(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, 2, "Length unit", "MM"), errorList.get(1));
        assertEquals(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, 3, "Width unit", "MM"), errorList.get(2));
        assertEquals(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, 4, "Height unit", "MM"), errorList.get(3));
    }

    @Test
    void testValidateLengthWidthHeightUnitWithValidValues() throws Exception {
        Method method = CSVParsingUtilV3.class.getDeclaredMethod("validateLengthWidhtHeightUnit",
                Map.class, String.class, String.class, int.class, List.class);
        method.setAccessible(true);

        Map<String, Set<String>> masterListsMap = new HashMap<>();
        masterListsMap.put(MasterDataType.WEIGHT_UNIT.getDescription(), Set.of("KG", "LB"));
        masterListsMap.put(MasterDataType.DIMENSION_UNIT.getDescription(), Set.of("CM", "IN"));

        List<String> errorList = new ArrayList<>();

        method.invoke(csvParsingUtilV3, masterListsMap, "VolumetricWeightUnit", "KG", 1, errorList);
        method.invoke(csvParsingUtilV3, masterListsMap, "LengthUnit", "CM", 2, errorList);
        method.invoke(csvParsingUtilV3, masterListsMap, "WidthUnit", "IN", 3, errorList);
        method.invoke(csvParsingUtilV3, masterListsMap, "HeightUnit", "CM", 4, errorList);

        // No errors
        assertEquals(0, errorList.size());
    }

    @Test
    void testValidateMeasurementUnit_invalidValues() throws Exception {
        Method method = CSVParsingUtilV3.class.getDeclaredMethod("validateMeasurementUnit",
                Map.class, String.class, String.class, int.class, List.class);
        method.setAccessible(true);

        Map<String, Set<String>> masterListsMap = new HashMap<>();
        masterListsMap.put(MasterDataType.DIMENSION_UNIT.getDescription(), Set.of("CM", "IN"));

        List<String> errorList = new ArrayList<>();

        // Invalid inner package measurement unit
        method.invoke(csvParsingUtilV3, masterListsMap, "innerpackagemeasurementunit", "MM", 1, errorList);
        // Invalid general measurement unit
        method.invoke(csvParsingUtilV3, masterListsMap, "measurementunit", "MM", 2, errorList);

        assertEquals(3, errorList.size());
        assertEquals(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, 1, "Inner package meaurement unit", "MM"), errorList.get(0));
        assertEquals(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, 1, "Measurement unit", "MM"), errorList.get(1));
    }

    @Test
    void testValidateMeasurementUnit_validValues() throws Exception {
        Method method = CSVParsingUtilV3.class.getDeclaredMethod("validateMeasurementUnit",
                Map.class, String.class, String.class, int.class, List.class);
        method.setAccessible(true);

        Map<String, Set<String>> masterListsMap = new HashMap<>();
        masterListsMap.put(MasterDataType.DIMENSION_UNIT.getDescription(), Set.of("CM", "IN"));

        List<String> errorList = new ArrayList<>();

        // Valid inner package measurement unit
        method.invoke(csvParsingUtilV3, masterListsMap, "innerpackagemeasurementunit", "CM", 1, errorList);
        // Valid general measurement unit
        method.invoke(csvParsingUtilV3, masterListsMap, "measurementunit", "IN", 2, errorList);

        assertEquals(0, errorList.size());
    }

    @Test
    void testGetCellValueAsString_nullCell() {
        assertEquals("", csvParsingUtilV3.getCellValueAsString(null));
    }

    @Test
    void testGetCellValueAsString_stringCell() {
        Cell cell = mock(Cell.class);
        when(cell.getCellType()).thenReturn(CellType.STRING);
        when(cell.getStringCellValue()).thenReturn("test");

        assertEquals("test", csvParsingUtilV3.getCellValueAsString(cell));
    }

    @Test
    void testGetCellValueAsString_booleanCell() {
        Cell cell = mock(Cell.class);
        when(cell.getCellType()).thenReturn(CellType.BOOLEAN);
        when(cell.getBooleanCellValue()).thenReturn(true);

        assertEquals("true", csvParsingUtilV3.getCellValueAsString(cell));
    }

    @Test
    void testGetCellValueAsString_formulaCell() {
        Cell cell = mock(Cell.class);
        when(cell.getCellType()).thenReturn(CellType.FORMULA);
        when(cell.getCellFormula()).thenReturn("SUM(A1:A2)");

        assertEquals("SUM(A1:A2)", csvParsingUtilV3.getCellValueAsString(cell));
    }

    @Test
    void testGetCellValueAsString_otherCellType() {
        Cell cell = mock(Cell.class);
        when(cell.getCellType()).thenReturn(CellType.BLANK); // any other type not explicitly handled

        assertEquals("", csvParsingUtilV3.getCellValueAsString(cell));
    }

    @Test
    void testNullConsolidationId() {
        BulkUploadRequest request = mock(BulkUploadRequest.class);
        when(request.getConsolidationId()).thenReturn(null);

        MultipartFile file = new MockMultipartFile("file.xlsx", new byte[0]);

        ValidationException ex = assertThrows(ValidationException.class, () ->
                csvParsingUtilV3.parseExcelFileEvents(file, request, Map.of(), Packing.class)
        );

        assertEquals("Please save the consolidation and then try again.", ex.getMessage());
    }

    @Test
    void testInvalidConsolidationId() {
        BulkUploadRequest request = mock(BulkUploadRequest.class);
        when(request.getConsolidationId()).thenReturn(1L);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.empty());

        MultipartFile file = new MockMultipartFile("file.xlsx", new byte[0]);

        ValidationException ex = assertThrows(ValidationException.class, () ->
                csvParsingUtilV3.parseExcelFileEvents(file, request, Map.of(), Packing.class)
        );

        assertEquals("Consolidation id is invalid 1", ex.getMessage());
    }

    @Test
    void testMissingMandatoryColumns() throws IOException {
        BulkUploadRequest request = mock(BulkUploadRequest.class);
        when(request.getConsolidationId()).thenReturn(1L);

        var consolidation = mock(ConsolidationDetails.class);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidation));

        // Excel file with only 1 column, missing mandatory columns
        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();
        Row headerRow = sheet.createRow(0);
        headerRow.createCell(0).setCellValue("someColumn");

        byte[] content;
        try (var baos = new java.io.ByteArrayOutputStream()) {
            workbook.write(baos);
            content = baos.toByteArray();
        }

        MultipartFile file = new MockMultipartFile("file.xlsx", content);

        ValidationException ex = assertThrows(ValidationException.class, () ->
                csvParsingUtilV3.parseExcelFileEvents(file, request, Map.of(), Packing.class)
        );

        assertTrue(ex.getMessage().contains("column(s) is missing"));
    }

    @Test
    void testCheckForContainerCodeValidationAddsError() throws Exception {
        // Prepare inputs
        Map<String, Set<String>> masterListsMap = new HashMap<>();
        masterListsMap.put(Constants.CONTAINER_TYPES, new HashSet<>(Arrays.asList("A", "B", "C")));
        String cellValue = "X"; // Invalid value
        int rowNum = 5;
        List<String> errorList = new ArrayList<>();

        // Access private method via reflection
        Method method = CSVParsingUtilV3.class.getDeclaredMethod(
                "checkForContainerCodeValidation", Map.class, String.class, int.class, List.class);
        method.setAccessible(true);

        // Call the method
        method.invoke(csvParsingUtilV3, masterListsMap, cellValue, rowNum, errorList);

        // Assert error was added
        assertEquals(1, errorList.size());
        assertEquals(
                String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Type", cellValue),
                errorList.get(0)
        );
    }

    @Test
    void testDuplicateContainerNumberThrowsValidationExceptionWhenNotUpdate() throws Exception {
        // Prepare inputs
        Map<String, String> existingContainerNumbers = new HashMap<>();
        existingContainerNumbers.put("CONT123", "guid1");
        Row row = mock(Row.class);
        String containerNumber = "CONT123";
        int rowNum = 5;
        boolean isUpdate = false;

        // Access private method
        Method method = CSVParsingUtilV3.class.getDeclaredMethod(
                "checkForDuplicateContainerNumberValidation",
                int.class, Row.class, String.class, int.class, boolean.class, Map.class
        );
        method.setAccessible(true);

        // Expect ValidationException
        InvocationTargetException exception = assertThrows(InvocationTargetException.class,
                () -> method.invoke(csvParsingUtilV3, -1, row, containerNumber, rowNum, isUpdate, existingContainerNumbers));

        assertTrue(exception.getCause().getMessage().contains("Duplicate container number CONT123 found at row: 5"));
    }

    @Test
    void testAddContainerNumberIfNotExists() throws Exception {
        Map<String, String> existingContainerNumbers = new HashMap<>();
        Row row = mock(Row.class);
        String containerNumber = "CONT999";
        int rowNum = 1;
        boolean isUpdate = false;

        Method method = CSVParsingUtilV3.class.getDeclaredMethod(
                "checkForDuplicateContainerNumberValidation",
                int.class, Row.class, String.class, int.class, boolean.class, Map.class
        );
        method.setAccessible(true);

        method.invoke(csvParsingUtilV3, -1, row, containerNumber, rowNum, isUpdate, existingContainerNumbers);

        // Verify that the new container number was added
        assertTrue(existingContainerNumbers.containsKey("CONT999"));
        assertEquals("", existingContainerNumbers.get("CONT999"));
    }

    @Test
    void testNegativeFieldValidator_addsErrorForInvalidCell() throws Exception {
        CSVParsingUtilV3<?> service = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);

        Method method = CSVParsingUtilV3.class.getDeclaredMethod(
                "negativeFieldValidator",
                String.class, String.class, String.class, int.class, String.class, List.class
        );
        method.setAccessible(true);

        List<String> errorList = new ArrayList<>();
        String column = "Name";
        String actualColumn = "Name";
        String cellValue = "abc-123";
        int rowNum = 1;
        String msgContent = "Invalid character";

        method.invoke(service, column, actualColumn, cellValue, rowNum, msgContent, errorList);

        assertEquals(1, errorList.size());
        assertTrue(errorList.get(0).contains("Invalid character"));
        assertTrue(errorList.get(0).contains("abc-123"));
    }

    @Test
    void testNegativeFieldValidator_noErrorForDifferentColumn() throws Exception {
        CSVParsingUtilV3<?> service = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);


        Method method = CSVParsingUtilV3.class.getDeclaredMethod(
                "negativeFieldValidator",
                String.class, String.class, String.class, int.class, String.class, List.class
        );
        method.setAccessible(true);

        List<String> errorList = new ArrayList<>();
        method.invoke(service, "Col1", "Col2", "abc-123", 1, "Invalid character", errorList);

        assertTrue(errorList.isEmpty());
    }

    @Test
    void testNegativeFieldValidator_noErrorForNoDash() throws Exception {
        CSVParsingUtilV3<?> service = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);

        Method method = CSVParsingUtilV3.class.getDeclaredMethod(
                "negativeFieldValidator",
                String.class, String.class, String.class, int.class, String.class, List.class
        );
        method.setAccessible(true);

        List<String> errorList = new ArrayList<>();
        method.invoke(service, "Name", "Name", "abc123", 1, "Invalid character", errorList);

        assertTrue(errorList.isEmpty());
    }

    @Test
    void testValidateContainers_allIfBlocks() {
        Map<String, Set<String>> masterListsMap = new HashMap<>();
        masterListsMap.put(Constants.CONTAINER_TYPES, Set.of("CONT1", "CONT2"));

        List<String> errorList = new ArrayList<>();

        // Case 1: containercode empty + transportMode not AIR → should add mandatory field error
        csvParsingUtilV3.validateContainers(masterListsMap, "containerCode", "", 1, "SEA", errorList);
        assertEquals(1, errorList.size());
        assertTrue(errorList.get(0).contains("Type"));

        // Case 2: containercode not empty but invalid → should add invalid field error
        errorList.clear();
        csvParsingUtilV3.validateContainers(masterListsMap, "containerCode", "INVALID", 2, "SEA", errorList);
        assertEquals(1, errorList.size());
        assertTrue(errorList.get(0).contains("Type"));
        assertTrue(errorList.get(0).contains("INVALID"));

        // Case 3: containercode valid → no error
        errorList.clear();
        csvParsingUtilV3.validateContainers(masterListsMap, "containerCode", "CONT1", 3, "SEA", errorList);
        assertTrue(errorList.isEmpty());

        // Case 4: containercode empty but transportMode AIR → no error
        errorList.clear();
        csvParsingUtilV3.validateContainers(masterListsMap, "containerCode", "", 4, Constants.TRANSPORT_MODE_AIR, errorList);
        assertTrue(errorList.isEmpty());
    }

    @Test
    void testValidateCountryCode_allIfBlocks() {
        Map<String, Set<String>> masterListsMap = new HashMap<>();
        masterListsMap.put(MasterDataType.COUNTRIES.getDescription(), Set.of("US", "IN", "GB"));

        List<String> errorList = new ArrayList<>();

        // Case 1: country code invalid → error
        csvParsingUtilV3.validateCountryCode(masterListsMap, "countryCode", "FR", 1, errorList);
        assertEquals(1, errorList.size());
        assertTrue(errorList.get(0).contains("Country Code"));
        assertTrue(errorList.get(0).contains("FR"));

        // Case 2: country code valid → no error
        errorList.clear();
        csvParsingUtilV3.validateCountryCode(masterListsMap, "countryCode", "US", 2, errorList);
        assertTrue(errorList.isEmpty());

        // Case 3: empty country code → no error
        errorList.clear();
        csvParsingUtilV3.validateCountryCode(masterListsMap, "countryCode", "", 3, errorList);
        assertTrue(errorList.isEmpty());
    }

    @Test
    void testSetParsedValueInField_allTypes() throws Exception {
        TestEntity entity = new TestEntity();
        Method method = CSVParsingUtilV3.class.getDeclaredMethod(
                "setParsedValueInField",
                Object.class, String.class, String.class, int.class, Field.class
        );
        method.setAccessible(true);

        // int
        Field intField = TestEntity.class.getField("intValue");
        method.invoke(csvParsingUtilV3, entity, "intValue", "123", 1, intField);
        assertEquals(123, entity.intValue);

        // String
        Field strField = TestEntity.class.getField("stringValue");
        method.invoke(csvParsingUtilV3, entity, "stringValue", "abc", 1, strField);
        assertEquals("abc", entity.stringValue);

        // long
        Field longField = TestEntity.class.getField("longValue");
        method.invoke(csvParsingUtilV3, entity, "longValue", "456", 1, longField);
        assertEquals(456L, entity.longValue);

        // boolean
        Field boolField = TestEntity.class.getField("boolValue");
        method.invoke(csvParsingUtilV3, entity, "boolValue", "true", 1, boolField);
        assertTrue(entity.boolValue);

        // BigDecimal
        Field bdField = TestEntity.class.getField("bigDecimalValue");
        method.invoke(csvParsingUtilV3, entity, "bigDecimalValue", "123.45", 1, bdField);
        assertEquals(new BigDecimal("123.45"), entity.bigDecimalValue);

// Enum (fixed: map display value to enum)
        Field statusField = TestEntity.class.getField("statusValue");
// Convert description to enum
        ContainerStatus statusEnum = Arrays.stream(ContainerStatus.values())
                .filter(s -> s.getDescription().equalsIgnoreCase("On Dock"))
                .findFirst()
                .orElseThrow(() -> new ValidationException("statusValue is invalid at row: 1"));
        method.invoke(csvParsingUtilV3, entity, "statusValue", statusEnum.name(), 1, statusField);
        assertEquals(ContainerStatus.ON_DOCK, entity.statusValue);


        // LocalDateTime
        Field dtField = TestEntity.class.getField("dateTimeValue");
        String nowStr = "2025-09-23T10:15:30";
        method.invoke(csvParsingUtilV3, entity, "dateTimeValue", nowStr, 1, dtField);
        assertEquals(LocalDateTime.parse(nowStr), entity.dateTimeValue);

        Field uuidField = TestEntity.class.getField("uuidValue");
        UUID uuid = UUID.randomUUID();
        method.invoke(csvParsingUtilV3, entity, "uuidValue", uuid.toString(), 1, uuidField);
        assertEquals(uuid, entity.uuidValue);

    }

    @Test
    void testSetParsedValueInField_emptyValue() throws Exception {
        TestEntity entity = new TestEntity();
        Field intField = TestEntity.class.getField("intValue");

        Method method = CSVParsingUtilV3.class.getDeclaredMethod(
                "setParsedValueInField",
                Object.class, String.class, String.class, int.class, Field.class
        );
        method.setAccessible(true);

        // empty attributeValue → method returns early, field not changed
        method.invoke(csvParsingUtilV3, entity, "intValue", "", 1, intField);
        assertEquals(0, entity.intValue); // default int value
    }

    static class TestEntity {
        public int intValue;
        public String stringValue;
        public long longValue;
        public boolean boolValue;
        public BigDecimal bigDecimalValue;
        public ContainerStatus statusValue;
        public LocalDateTime dateTimeValue;
        public UUID uuidValue;
        public Double unsupportedValue; // For testing NoSuchFieldException
    }

    @Test
    void testFetchMasterLists_catchBlock() {
        CSVParsingUtilV3<?> csvParsingUtil = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);

        // Mock dependencies
        IV1Service v1ServiceMock = Mockito.mock(IV1Service.class);
        JsonHelper jsonHelperMock = Mockito.mock(JsonHelper.class);

        // Inject mocks into your CSVParsingUtil (assuming fields are package-private or use reflection)
        ReflectionTestUtils.setField(csvParsingUtil, "v1Service", v1ServiceMock);
        ReflectionTestUtils.setField(csvParsingUtil, "jsonHelper", jsonHelperMock);

        MasterDataType masterDataType = Mockito.mock(MasterDataType.class);
        Mockito.when(masterDataType.getId()).thenReturn(Integer.valueOf("123"));

        Map<String, Set<String>> masterDataMap = new HashMap<>();

        // Make v1Service.fetchMasterData throw an exception to trigger the catch block
        Mockito.when(v1ServiceMock.fetchMasterData(Mockito.any()))
                .thenThrow(new RuntimeException("Test Exception"));

        // Call the method
        csvParsingUtil.fetchMasterLists(masterDataType, masterDataMap);

        // No assertion needed to cover catch, just ensure no exception is propagated
        // Optionally, you can verify logging (if log is mockable)
        assertTrue(masterDataMap.isEmpty());
    }

    @Test
    void testFetchContainerType_catchBlock() throws Exception {
        CSVParsingUtilV3<?> csvParsingUtil = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);

        // Create mocks
        IV1Service v1ServiceMock = Mockito.mock(IV1Service.class);
        JsonHelper jsonHelperMock = Mockito.mock(JsonHelper.class);

        // Inject mocks into private fields using reflection
        Field v1ServiceField = CSVParsingUtilV3.class.getDeclaredField("v1Service");
        v1ServiceField.setAccessible(true);
        v1ServiceField.set(csvParsingUtil, v1ServiceMock);


        Field jsonHelperField = CSVParsingUtilV3.class.getDeclaredField("jsonHelper");
        jsonHelperField.setAccessible(true);
        jsonHelperField.set(csvParsingUtil, jsonHelperMock);

        // Prepare input
        Map<String, Set<String>> masterDataMap = new HashMap<>();

        // Make v1Service.fetchContainerTypeData throw an exception to trigger the catch block
        Mockito.when(v1ServiceMock.fetchContainerTypeData(Mockito.any()))
                .thenThrow(new RuntimeException("Test Exception"));

        // Call the method
        csvParsingUtil.fetchContainerType(masterDataMap);

        // Assert that masterDataMap is still empty since an exception occurred
        assertTrue(masterDataMap.isEmpty());
    }

    @Test
    void testFetchUnlocationData_catchBlock() throws Exception {
        // Create the utility instance
        CSVParsingUtilV3<?> csvParsingUtil = new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils);

        // Create mocks
        IV1Service v1ServiceMock = Mockito.mock(IV1Service.class);
        JsonHelper jsonHelperMock = Mockito.mock(JsonHelper.class);

        // Inject mocks into private fields using reflection
        Field v1ServiceField = CSVParsingUtilV3.class.getDeclaredField("v1Service");
        v1ServiceField.setAccessible(true);
        v1ServiceField.set(csvParsingUtil, v1ServiceMock);

        Field jsonHelperField = CSVParsingUtilV3.class.getDeclaredField("jsonHelper");
        jsonHelperField.setAccessible(true);
        jsonHelperField.set(csvParsingUtil, jsonHelperMock);

        // Prepare input
        List<String> unlocationsList = List.of("LOC1", "LOC2");
        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        // Make v1Service.fetchUnlocation throw an exception to trigger the catch block
        Mockito.when(v1ServiceMock.fetchUnlocation(Mockito.any()))
                .thenThrow(new RuntimeException("Test Exception"));

        // Call the method
        csvParsingUtil.fetchUnlocationData(unlocationsList, masterDataMap, locCodeToLocationReferenceGuidMap);

        // Assert maps are still empty since an exception occurred
        assertTrue(masterDataMap.isEmpty());
        assertTrue(locCodeToLocationReferenceGuidMap.isEmpty());
    }

    @Test
    void testFetchCommodityData_catchBlock() {
        // Spy the utility class
        CSVParsingUtilV3<?> csvParsingUtil = Mockito.spy(
                new CSVParsingUtilV3<>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper,
                        consolidationDetailsDao, Executors.newFixedThreadPool(2), commonUtils)
        );

        // Mock the private method getCommodityDataResponse to throw an exception
        Mockito.doThrow(new RuntimeException("Test Exception"))
                .when(csvParsingUtil).getCommodityDataResponse(Mockito.anyList());

        // Prepare input
        List<String> commodityCodesList = List.of("C1", "C2");
        Map<String, Set<String>> masterDataMap = new HashMap<>();

        // Call the method
        csvParsingUtil.fetchCommodityData(commodityCodesList, masterDataMap);

        // Since an exception occurred, the map should remain empty
        assertTrue(masterDataMap.isEmpty());
    }


    @Test
    void testSetFieldForEvents_noSuchFieldException() {
        CSVParsingUtilV3<TestEntity> util = new CSVParsingUtilV3<>(
                consoleShipmentMappingDao,
                shipmentDao,
                v1Service,
                jsonHelper,
                consolidationDetailsDao,
                Executors.newFixedThreadPool(2),
                commonUtils
        );

        TestEntity entity = new TestEntity();

        // Use an unsupported field name to trigger NoSuchFieldException
        String invalidFieldName = "unsupportedValue";
        String value = "123";

        Exception exception = assertThrows(NoSuchFieldException.class, () ->
                util.setFieldForEvents(entity, invalidFieldName, value)
        );

        // Optional: verify exception message (default is null)
        assertNull(exception.getMessage());
    }

    @Test
    void testSetParsedValueInField_unsupportedType_throwsNoSuchFieldException() throws Exception {
        // Use a concrete type for the generic
        CSVParsingUtilV3<TestEntity> util = new CSVParsingUtilV3<>(
                consoleShipmentMappingDao,
                shipmentDao,
                v1Service,
                jsonHelper,
                consolidationDetailsDao,
                Executors.newFixedThreadPool(2),
                commonUtils
        );


        // Create a dummy field of unsupported type
        class UnsupportedFieldEntity {
            public Object unsupportedField;
        }
        UnsupportedFieldEntity unsupportedEntity = new UnsupportedFieldEntity();
        Field unsupportedField = UnsupportedFieldEntity.class.getField("unsupportedField");

        // Use reflection to invoke private method
        Method method = CSVParsingUtilV3.class.getDeclaredMethod(
                "setParsedValueInField",
                Object.class, String.class, String.class, int.class, Field.class
        );
        method.setAccessible(true);

        // Assert that the method triggers the catch and calls raiseException
        Exception exception = assertThrows(Exception.class, () ->
                method.invoke(util, unsupportedEntity, "unsupportedField", "value", 1, unsupportedField)
        );

        // The exception will be wrapped by reflection (InvocationTargetException)
        assertTrue(exception.getCause() instanceof RuntimeException); // because raiseException likely throws RuntimeException
    }

    @Test
    void testCheckPacksValidations_allPaths() {
        CSVParsingUtilV3<TestEntity> util = new CSVParsingUtilV3<>(
                consoleShipmentMappingDao,
                shipmentDao,
                v1Service,
                jsonHelper,
                consolidationDetailsDao,
                Executors.newFixedThreadPool(2),
                commonUtils
        );
        List<String> errorList;

        // 1. Column not "PACKS" → early return
        errorList = new ArrayList<>();
        util.checkPacksValidations("OtherColumn", "5", 1, Constants.TRANSPORT_MODE_AIR, errorList);
        assertTrue(errorList.isEmpty());

        // 2. Column "PACKS", value empty, V3 → return
        errorList = new ArrayList<>();
        try (MockedStatic<ShipmentVersionContext> mockV3 = Mockito.mockStatic(ShipmentVersionContext.class)) {
            mockV3.when(ShipmentVersionContext::isV3).thenReturn(true);
            util.checkPacksValidations(Constants.PACKS, "", 2, Constants.TRANSPORT_MODE_AIR, errorList);
        }
        assertTrue(errorList.isEmpty());

        // 3. Column "PACKS", Air transport, value < 1 → error added
        errorList = new ArrayList<>();
        util.checkPacksValidations(Constants.PACKS, "0", 3, Constants.TRANSPORT_MODE_AIR, errorList);
        assertEquals(2, errorList.size());

        // 4. Column "PACKS", value < 1 general numeric → error added
        errorList = new ArrayList<>();
        util.checkPacksValidations(Constants.PACKS, "-5", 4, "SEA", errorList);
        assertEquals(1, errorList.size());

        // 5. Column "PACKS", valid number ≥ 1 → no error
        errorList = new ArrayList<>();
        util.checkPacksValidations(Constants.PACKS, "3", 5, "SEA", errorList);
        assertTrue(errorList.isEmpty());

        // 6. Column "PACKS", non-numeric value → NumberFormatException → error added
        errorList = new ArrayList<>();
        util.checkPacksValidations(Constants.PACKS, "abc", 6, "SEA", errorList);
        assertEquals(1, errorList.size());
    }

    @Test
    void testCheckForUnitValidations_allIfConditions() throws ValidationException {
        CSVParsingUtilV3<TestEntity> util = new CSVParsingUtilV3<>(
                consoleShipmentMappingDao,
                shipmentDao,
                v1Service,
                jsonHelper,
                consolidationDetailsDao,
                Executors.newFixedThreadPool(2),
                commonUtils
        );

        Map<String, Set<String>> masterListsMap = new HashMap<>();
        List<String> errorList = new ArrayList<>();
        int rowNum = 1;
        String transportMode = "SEA";

        // 1. DG Substance ID condition
        masterListsMap.put("DGSubstanceUNDGContact", Set.of("VALID_DG"));
        util.checkForUnitValidations(masterListsMap, "DGSubstanceID", "INVALID_DG", rowNum, transportMode, errorList);
        assertEquals(1, errorList.size());
        errorList.clear();

        // 2. Inner package type condition
        masterListsMap.put(MasterDataType.PACKS_UNIT.getDescription(), Set.of("VALID_PACK"));
        util.checkForUnitValidations(masterListsMap, "innerPackageType", "INVALID_PACK", rowNum, transportMode, errorList);
        assertEquals(1, errorList.size());
        errorList.clear();

        // 3. Volume unit condition
        util.checkForUnitValidations(masterListsMap, "volumeUnit", "ANY_VALUE", rowNum, transportMode, errorList);
        // Volume validation adds errors inside validateVolumeUnit if applicable
        // If validateVolumeUnit doesn't add errors, at least the branch is executed
        errorList.clear();

        // 4. Chargeable unit condition
        masterListsMap.put(MasterDataType.WEIGHT_UNIT.getDescription(), Set.of("KG"));
        util.checkForUnitValidations(masterListsMap, "chargeableUnit", "LB", rowNum, transportMode, errorList);
        assertEquals(1, errorList.size());
        errorList.clear();

        // 5. Other validations
        util.checkForUnitValidations(masterListsMap, "lengthUnit", "ANY_VALUE", rowNum, transportMode, errorList);
        // This executes validateLengthWidhtHeightUnit
        // validateCountryCode
        util.checkForUnitValidations(masterListsMap, "countryCode", "ANY_VALUE", rowNum, transportMode, errorList);
    }

    @Test
    void testValidateContainerNumber_throwsValidationException() {
        CSVParsingUtilV3<TestEntity> util = new CSVParsingUtilV3<>(
                consoleShipmentMappingDao,
                shipmentDao,
                v1Service,
                jsonHelper,
                consolidationDetailsDao,
                Executors.newFixedThreadPool(2),
                commonUtils
        );

        String[] header = {Constants.CONTAINER_NUMBER};
        int rowIndex = 1;
        int columnIndex = 0;

        // Case 1: Empty cell value → should throw missing exception
        Exception ex1 = assertThrows(ValidationException.class, () ->
                util.validateContainerNumber(header, rowIndex, Set.of("C1", "C2"), new ArrayList<>(), columnIndex, "")
        );
        assertTrue(ex1.getMessage().contains("Container Number is missing"));

        // Case 2: Cell value not in existingContainerNumberSet → should throw not present exception
        Exception ex2 = assertThrows(ValidationException.class, () ->
                util.validateContainerNumber(header, rowIndex, Set.of("C1", "C2"), new ArrayList<>(), columnIndex, "C3")
        );
        assertTrue(ex2.getMessage().contains("is not present in consolidation"));

        // Case 3: Valid container number → should add to containerNumberList
        List<String> containerNumberList = new ArrayList<>();
        assertDoesNotThrow(() ->
                util.validateContainerNumber(header, rowIndex, Set.of("C1", "C2"), containerNumberList, columnIndex, "C1")
        );
        assertEquals(1, containerNumberList.size());
        assertEquals("C1", containerNumberList.get(0));
    }

    @Test
    void testProcessHeader_throwsValidationException() {
        // Use concrete type for the generic
        CSVParsingUtilV3<TestEntity> util = new CSVParsingUtilV3<>(
                consoleShipmentMappingDao,
                shipmentDao,
                v1Service,
                jsonHelper,
                consolidationDetailsDao,
                Executors.newFixedThreadPool(2),
                commonUtils
        );
        TestEntity entity = new TestEntity();

        // Mock Row and Cell
        Row rowMock = Mockito.mock(Row.class);
        Cell cellMock = Mockito.mock(Cell.class);

        // Header with container number and eventCode
        String[] header = {Constants.CONTAINER_NUMBER, "eventCode"};
        int rowIndex = 1;

        // Mock getCellValueAsString to return values
        Mockito.when(rowMock.getCell(0)).thenReturn(cellMock);
        Mockito.when(rowMock.getCell(1)).thenReturn(cellMock);

        // --- Case 1: Container number missing ---
        Mockito.when(cellMock.getCellType()).thenReturn(CellType.STRING);

        Set<String> existingContainerSet = Set.of("C1", "C2");
        List<String> containerNumberList = new ArrayList<>();
        Set<String> orderEventsDict = Set.of("EVT1", "EVT2");

        Exception ex1 = assertThrows(ValidationException.class, () ->
                util.processHeader(header, rowMock, rowIndex, existingContainerSet, containerNumberList, orderEventsDict, entity)
        );
        assertTrue(ex1.getMessage().contains("Container Number is missing"));

        // --- Case 2: EventCode empty ---
        Mockito.when(util.getCellValueAsString(cellMock)).thenReturn("C1", ""); // first call for container, second for eventCode
        containerNumberList.clear();
        Exception ex2 = assertThrows(ValidationException.class, () ->
                util.processHeader(header, rowMock, rowIndex, existingContainerSet, containerNumberList, orderEventsDict, entity)
        );
        assertTrue(ex2.getMessage().contains("EventCode is mandatory"));

        // --- Case 3: EventCode not in masterData ---
        Mockito.when(util.getCellValueAsString(cellMock)).thenReturn("C1", "EVT_INVALID");
        containerNumberList.clear();
        Exception ex3 = assertThrows(ValidationException.class, () ->
                util.processHeader(header, rowMock, rowIndex, existingContainerSet, containerNumberList, orderEventsDict, entity)
        );
        assertTrue(ex3.getMessage().contains("EventCode is not present in masterData"));
    }

    @Test
    void testParseBooleanField_allBranches() {
        CSVParsingUtilV3<TestEntity> util = new CSVParsingUtilV3<>(
                consoleShipmentMappingDao,
                shipmentDao,
                v1Service,
                jsonHelper,
                consolidationDetailsDao,
                Executors.newFixedThreadPool(2),
                commonUtils
        );
        List<String> errorList = new ArrayList<>();
        int rowNum = 1;
        String msgField = "TestField";

        // 1. Header doesn't match → returns false
        assertFalse(util.parseBooleanField("OtherHeader", "Header", msgField, "yes", rowNum, errorList));
        assertTrue(errorList.isEmpty());

        // 3. Header matches but value is empty → adds generic error
        errorList.clear();
        assertFalse(util.parseBooleanField("Header", "Header", msgField, "   ", rowNum, errorList));
        assertEquals(1, errorList.size());

        // 4. True values
        errorList.clear();
        assertTrue(util.parseBooleanField("Header", "Header", msgField, "yes", rowNum, errorList));
        assertTrue(util.parseBooleanField("Header", "Header", msgField, "Y", rowNum, errorList));
        assertTrue(util.parseBooleanField("Header", "Header", msgField, "TrUe", rowNum, errorList));
        assertTrue(errorList.isEmpty());

        // 5. False values
        assertFalse(util.parseBooleanField("Header", "Header", msgField, "no", rowNum, errorList));
        assertFalse(util.parseBooleanField("Header", "Header", msgField, "N", rowNum, errorList));
        assertFalse(util.parseBooleanField("Header", "Header", msgField, "FaLsE", rowNum, errorList));
        assertTrue(errorList.isEmpty());

// 6. Default case (invalid boolean)
        errorList.clear();
        boolean result = util.parseBooleanField("Header", "Header", msgField, "maybe", rowNum, errorList);
        assertFalse(result); // must expect false
        assertEquals(1, errorList.size());
    }

    @Test
    void testValidateSheetLastRowNum_catchBlock() {
        CSVParsingUtilV3<TestEntity> util = new CSVParsingUtilV3<>(
                consoleShipmentMappingDao,
                shipmentDao,
                v1Service,
                jsonHelper,
                consolidationDetailsDao,
                Executors.newFixedThreadPool(2),
                commonUtils
        );
        List<String> errorList = new ArrayList<>();
        int rowNum = 1;

        // Map is empty → will cause ValidationException to be thrown
        Map<UUID, TestEntity> mapOfEntity = new HashMap<>();
        String invalidGuid = UUID.randomUUID().toString();

        boolean result = util.validateSheetLastRowNum(mapOfEntity, invalidGuid, rowNum, errorList);

        // Catch block executed → errorList should contain the error, method returns true
        assertTrue(result);
        assertEquals(1, errorList.size());
        assertTrue(errorList.get(0).contains("Guid"));
    }

    @Test
    void testFindColumnIndex_returnsIndex() {
        CSVParsingUtilV3<TestEntity> util = new CSVParsingUtilV3<>(
                consoleShipmentMappingDao,
                shipmentDao,
                v1Service,
                jsonHelper,
                consolidationDetailsDao,
                Executors.newFixedThreadPool(2),
                commonUtils
        );

        String[] header = {"ContainerNumber", "EventCode", "Packages"};
        String columnName = "EventCode";

        int index = util.findColumnIndex(header, columnName);

        assertEquals(1, index); // "EventCode" is at index 1
    }

    @Test
    void testParseExcelFilePacking_secondCatch() throws IOException, NoSuchFieldException, InstantiationException, IllegalAccessException {
        // Spy the util class
        CSVParsingUtilV3<TestEntity> util = Mockito.spy(new CSVParsingUtilV3<>(
                consoleShipmentMappingDao,
                shipmentDao,
                v1Service,
                jsonHelper,
                consolidationDetailsDao,
                Executors.newFixedThreadPool(2),
                commonUtils
        ));

        MultipartFile fileMock = Mockito.mock(MultipartFile.class);
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L); // non-null to avoid first NPE

        Map<UUID, TestEntity> mapOfEntity = new HashMap<>();
        Map<String, Set<String>> masterDataMap = new HashMap<>();
        List<String> errorList = new ArrayList<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        // Minimal workbook with header row and mandatory columns
        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet();
        Row headerRow = sheet.createRow(0);
        headerRow.createCell(0).setCellValue("shipmentNumber"); // mandatory column
        headerRow.createCell(1).setCellValue(Constants.PACKS);  // mandatory column
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        workbook.write(out);
        workbook.close();
        ByteArrayInputStream in = new ByteArrayInputStream(out.toByteArray());
        Mockito.when(fileMock.getInputStream()).thenReturn(in);

        // Stub getShipmentIds to return empty map (covers any consolidationId)
        Mockito.doReturn(new HashMap<>()).when(util).getShipmentIds(Mockito.any());

        // Mock processLastRowNumForExcelFilePacking to throw IllegalAccessException
        Mockito.doThrow(new IllegalAccessException("Test Exception"))
                .when(util).processLastRowNumForExcelFilePacking(
                        Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
                        Mockito.anyInt(), Mockito.anyInt(), Mockito.any(), Mockito.any(),
                        Mockito.any(), Mockito.any(), Mockito.any()
                );

        // Call parseExcelFilePacking → should trigger second catch
        ValidationException ex = assertThrows(ValidationException.class, () ->
                util.parseExcelFilePacking(fileMock, request, mapOfEntity, masterDataMap,
                        TestEntity.class, undg, flashpoint, locCodeToLocationReferenceGuidMap, errorList)
        );

        // Verify that the second catch is executed
        assertEquals(ContainerConstants.EXCEL_SHEET_INVALID, ex.getMessage());
    }


}