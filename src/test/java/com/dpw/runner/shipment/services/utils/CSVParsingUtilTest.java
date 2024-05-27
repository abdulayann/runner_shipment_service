package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.dao.impl.ConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.ContainerEventExcelModel;
import com.dpw.runner.shipment.services.dto.request.ContainersExcelModel;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1ContainerTypeResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferDGSubstance;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.response.CommodityResponse;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.MDC;
import org.springframework.data.domain.PageImpl;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.util.*;
import java.util.concurrent.Executors;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class CSVParsingUtilTest {

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

    @InjectMocks
    private CSVParsingUtil<Packing> csvParsingUtil;

    private ContainerResponse containerResponse;

    private JsonTestUtility jsonTestUtility;
    private ObjectMapper objectMapper;

    final Set<String> requiredFields = Set.of(Constants.CONTAINER_NUMBER, "volumeUtilization", "weightUtilization", "achievedVolume",
            "achievedVolumeUnit", "achievedWeight", "achievedWeightUnit", "grossVolume", "grossVolumeUnit",
            "allocatedWeight", "allocatedWeightUnit", Constants.NET_WEIGHT, "netWeightUnit", Constants.GROSS_WEIGHT, "grossWeightUnit", "remarks",
            "extraParams", "chargeable", "chargeableUnit", "ownType", Constants.PACKS, "packsType", "marksNums", "innerPackageMeasurementUnit", "pacrNumber");

    private final Set<String> hiddenFields = Set.of("pickupAddress",
            "deliveryAddress", "eventsList", "packsList", "shipmentsList", "bookingCharges");

    private static MultipartFile mockPackingFile;
    private static MultipartFile mockPackingInvalidFile;
    private static MultipartFile mockPackingInvalidDGSubstanceIdFile;
    private static MultipartFile mockContainerFile;
    private static MultipartFile mockContainerInvalidFile;
    private static MultipartFile mockExcelFileEvents_missingEventCode;
    private static CSVParsingUtil<Containers> csvParsingUtilContainer;
    private static CSVParsingUtil<Events> csvParsingUtilContainerEvents;
    private static MultipartFile mockExcelFileEvents;


    @BeforeEach
    void setUp() throws IOException {
        objectMapper = JsonTestUtility.getMapper();
        jsonTestUtility = new JsonTestUtility();
        csvParsingUtilContainer = new CSVParsingUtil<Containers>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao);
        containerResponse = objectMapper.convertValue(jsonTestUtility.getTestContainer(), ContainerResponse.class);
        csvParsingUtil = new CSVParsingUtil<Packing>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao);
        csvParsingUtil.executorService = Executors.newFixedThreadPool(10);
        csvParsingUtilContainerEvents = new CSVParsingUtil<Events>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao);
    }

    @BeforeAll
    static void setupBeforeAll() throws IOException {
        mockExcelFileEvents = getFileFromResources("TestContainerEvents.xlsx");
        mockContainerFile = getFileFromResources("TestContainers.xlsx");
        mockPackingFile = getFileFromResources("TestCargoDetails.xlsx");
        mockPackingInvalidFile = getFileFromResources("TestCargoDetailsInvalid.xlsx");
        mockPackingInvalidDGSubstanceIdFile = getFileFromResources("CargoDetails_InvalidDGSubstanceId.xlsx");
        mockExcelFileEvents_missingEventCode = getFileFromResources("TestContainerEvents_MissingEventCode.xlsx");
    }

    private static MultipartFile getFileFromResources(String fileName) throws IOException {
        try (InputStream is = CSVParsingUtilTest.class.getClassLoader().getResourceAsStream(fileName)) {
            if (is == null) {
                throw new IOException("File not found: " + fileName);
            }
            return new MockMultipartFile("file", fileName, "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", is);
        }
    }

    private MultipartFile getInvalidFileFromResources(String fileName) throws IOException {
        try (InputStream is = getClass().getClassLoader().getResourceAsStream(fileName)) {
            if (is == null) {
                throw new IOException("File not found: " + fileName);
            }
            return new MockMultipartFile("file", fileName, "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", is);
        }
    }

    @Test
    void testInValidInput_InvalidDGSubstanceId() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"),jsonTestUtility.getTestPacking(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"),jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a" , "b"));
        // Mock responses
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(ConsoleShipmentMapping.builder().shipmentId(12L).consolidationId(13L).build()));
        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        when(shipmentDao.findAll(any(),any())).thenReturn(new PageImpl<>(List.of(sd1)));

        assertThrows(ValidationException.class, () -> csvParsingUtil.parseExcelFilePacking(mockPackingInvalidDGSubstanceIdFile, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap));
    }

    @Test
    void testInValidInput_validDGSubstanceId() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"),jsonTestUtility.getTestPacking(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"),jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a" , "b"));
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(ConsoleShipmentMapping.builder().shipmentId(12L).consolidationId(13L).build()));
        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        when(shipmentDao.findAll(any(),any())).thenReturn(new PageImpl<>(List.of(sd1)));
        List<Packing> result = (List<Packing>) csvParsingUtil.parseExcelFilePacking(mockPackingFile, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap);

        assertNotNull(result);
        assertEquals(2, result.size());
    }

    @Test
    void testValidInput_ForPacking() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"),jsonTestUtility.getTestPacking(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"),jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a" , "b"));
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
        when(shipmentDao.findAll(any(),any())).thenReturn(new PageImpl<>(List.of(sd1)));
        List<Packing> result = (List<Packing>) csvParsingUtil.parseExcelFilePacking(mockPackingFile, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap);

        assertNotNull(result);
        assertEquals(2, result.size());
    }
    /*

        when(v1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().entities(new ArrayList<>()).build());
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setLocCode("Loc1");
        unlocationsResponse.setLocationsReferenceGUID(UUID.randomUUID().toString());

        UnlocationsResponse unlocationsResponse2 = new UnlocationsResponse();
        unlocationsResponse2.setLocCode("DEF");
        unlocationsResponse2.setLocationsReferenceGUID(UUID.randomUUID().toString());


        when(jsonHelper.convertValueToList(any(Object.class), eq(UnlocationsResponse.class))).thenReturn(List.of(unlocationsResponse, unlocationsResponse2));

     */


    @Test
    void testInValidInput_ForPacking_InvalidOrigin_ThrowsException() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"),jsonTestUtility.getTestPacking(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"),jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a" , "b"));
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
        when(shipmentDao.findAll(any(),any())).thenReturn(new PageImpl<>(List.of(sd1)));
        assertThrows(ValidationException.class, () -> csvParsingUtil.parseExcelFilePacking(mockPackingFile, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap));
    }

    @Test
    void testValidInput_ForContainerEvents() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Events> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"),jsonTestUtility.getTestEventData(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"),jsonTestUtility.getTestEventData());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        var consol = jsonTestUtility.getTestNewConsolidation();
        consol.setContainersList(List.of(Containers.builder().containerNumber("ABCD1231").build()));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consol));
        MDC.setContextMap(Map.of("a" , "b"));
        List<EntityTransferDGSubstance> entityTransferDGSubstanceList = List.of(EntityTransferDGSubstance.builder().Id(2L).UNIDNo(123L).FlashPoint("FlashPoint").build());
        CommodityResponse commodityResponse = new CommodityResponse();

        commodityResponse.setCode("1231");
        List<CommodityResponse> commodityResponseList = List.of(commodityResponse);

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        List<Events> result = (List<Events>) csvParsingUtilContainerEvents.parseExcelFile(mockExcelFileEvents, request, mapOfEntity, masterDataMap, Events.class, ContainerEventExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap);

        assertNotNull(result);
        assertEquals(1, result.size());
    }

    @Test
    void testValidInput_ForContainerEvents_MissingEventCodeColumn_ThrowsException() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Events> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"),jsonTestUtility.getTestEventData(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"),jsonTestUtility.getTestEventData());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        var consol = jsonTestUtility.getTestNewConsolidation();
        consol.setContainersList(List.of(Containers.builder().containerNumber("ABCD1231").build()));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consol));
        MDC.setContextMap(Map.of("a" , "b"));
        List<EntityTransferDGSubstance> entityTransferDGSubstanceList = List.of(EntityTransferDGSubstance.builder().Id(2L).UNIDNo(123L).FlashPoint("FlashPoint").build());
        CommodityResponse commodityResponse = new CommodityResponse();

        commodityResponse.setCode("1231");
        List<CommodityResponse> commodityResponseList = List.of(commodityResponse);

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        assertThrows(ValidationException.class, () -> csvParsingUtilContainerEvents.parseExcelFile(mockExcelFileEvents_missingEventCode, request, mapOfEntity, masterDataMap, Events.class, ContainerEventExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap));
    }

    @Test
    void testValidInput_ForContainerEvents_MissingConsolidation_ThrowsException() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        Map<UUID, Events> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"),jsonTestUtility.getTestEventData(),
                UUID.fromString("cbd3bb18-bba8-46ad-85e6-4e94290fc21b"),jsonTestUtility.getTestEventData());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        var consol = jsonTestUtility.getTestNewConsolidation();
        consol.setContainersList(List.of(Containers.builder().containerNumber("ABCD1231").build()));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
        MDC.setContextMap(Map.of("a" , "b"));
        List<EntityTransferDGSubstance> entityTransferDGSubstanceList = List.of(EntityTransferDGSubstance.builder().Id(2L).UNIDNo(123L).FlashPoint("FlashPoint").build());
        CommodityResponse commodityResponse = new CommodityResponse();

        commodityResponse.setCode("1231");
        List<CommodityResponse> commodityResponseList = List.of(commodityResponse);

        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        assertThrows(ValidationException.class, () -> csvParsingUtilContainerEvents.parseExcelFile(mockExcelFileEvents_missingEventCode, request, mapOfEntity, masterDataMap, Events.class, ContainerEventExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap));
    }



    @Test
    void testValidInput_ForContainers() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        var container = jsonTestUtility.getTestContainer();
        container.setGuid(UUID.fromString("bcf5d7cc-9448-4418-90f1-a94245f5d2d7"));
        Map<UUID, Containers> mapOfEntity = Map.of(
                UUID.fromString("bcf5d7cc-9448-4418-90f1-a94245f5d2d7"),container);

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();


        when(v1Service.fetchContainerTypeData(any())).thenReturn(V1DataResponse.builder().entities(new ArrayList<>()).build());
        V1ContainerTypeResponse v1ContainerTypeResponse = new V1ContainerTypeResponse();
        v1ContainerTypeResponse.setCode("20GP");
        when(jsonHelper.convertValueToList(any(Object.class), eq(V1ContainerTypeResponse.class))).thenReturn(List.of(v1ContainerTypeResponse));


        MDC.setContextMap(Map.of("a" , "b"));
        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
        List<Containers> result = csvParsingUtilContainer.parseExcelFile(mockContainerFile, request, mapOfEntity, masterDataMap, Containers.class, ContainersExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap);

        assertNotNull(result);
        assertEquals(1, result.size());
    }

    @Test
    void testInValidInput_ForContainers_GuidNotPresentInMapOfEntity() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);
        var container = jsonTestUtility.getTestContainer();
        container.setGuid(UUID.fromString("bcf5d7cc-9448-4418-90f1-a94245f5d2d7"));
        Map<UUID, Containers> mapOfEntity = new HashMap<>();

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a" , "b"));
        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);
//        List<Containers> result = csvParsingUtilContainer.parseExcelFile(mockContainerFile, request, mapOfEntity, masterDataMap, Containers.class, ContainersExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap);

        assertThrows(ValidationException.class, () -> csvParsingUtilContainer.parseExcelFile(mockContainerFile, request, mapOfEntity, masterDataMap, Containers.class, ContainersExcelModel.class, undg, flashpoint, locCodeToLocationReferenceGuidMap));

    }

    @Test
    void testInValidInput_ForPacking() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);

        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"),jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a" , "b"));
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(ConsoleShipmentMapping.builder().shipmentId(12L).consolidationId(13L).build()));
        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);

        when(shipmentDao.findAll(any(),any())).thenReturn(new PageImpl<>(List.of(sd1)));

        assertThrows(ValidationException.class, () -> csvParsingUtil.parseExcelFilePacking(mockPackingFile, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap));
    }

    @Test
    void testInValidInputFile() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(123L);

        Map<UUID, Packing> mapOfEntity = Map.of(
                UUID.fromString("87930938-8fdf-42d4-a70c-204b511a1684"),jsonTestUtility.getTestPacking());

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<Long, Long> undg = new HashMap<>();
        Map<Long, String> flashpoint = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();

        MDC.setContextMap(Map.of("a" , "b"));
        // Mock responses
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(ConsoleShipmentMapping.builder().shipmentId(12L).consolidationId(13L).build()));
        ShipmentDetails sd1 = jsonTestUtility.getTestShipment();
        sd1.setShipmentId("SHP000109322");
        sd1.setId(12L);

        when(shipmentDao.findAll(any(),any())).thenReturn(new PageImpl<>(List.of(sd1)));

        assertThrows(ValidationException.class, () -> csvParsingUtil.parseExcelFilePacking(mockPackingInvalidFile, request, mapOfEntity, masterDataMap, Packing.class, undg, flashpoint, locCodeToLocationReferenceGuidMap));
    }



    @Test
    void getHeadersForContainer() {
        List<Field> containerFields = Arrays.stream(ContainerResponse.class.getDeclaredFields()).toList();
        var response = csvParsingUtil.getHeadersForContainer();
        final Set<String> requiredFields = Set.of(Constants.CONTAINER_NUMBER, "volumeUtilization", "weightUtilization", "achievedVolume",
                "achievedVolumeUnit", "achievedWeight", "achievedWeightUnit", "grossVolume", "grossVolumeUnit",
                "allocatedWeight", "allocatedWeightUnit", Constants.NET_WEIGHT, "netWeightUnit", Constants.GROSS_WEIGHT, "grossWeightUnit", "remarks",
                "extraParams", "chargeable", "chargeableUnit", "ownType", Constants.PACKS, "packsType", "marksNums", "innerPackageMeasurementUnit", "pacrNumber");
        var expected = containerFields.stream().filter(field -> {
            if (requiredFields.contains(field.getName())) return true;
            else return false;
        }).map(x -> x.getName()).toList();
        assertEquals(response, expected);
    }

    @Test
    void testGetAllAttributeValuesAsListContainer() throws IllegalAccessException {
        ContainerResponse response = containerResponse;
        List<String> actualValues = csvParsingUtil.getAllAttributeValuesAsListContainer(response);
        assertNotNull(actualValues);
        assertNotNull(response);
    }

    @Test
    void testGetAllAttributeValuesAsListContainer_withNullValues() throws IllegalAccessException {
        ContainerResponse response = new ContainerResponse();

        List<String> expectedValues = Arrays.asList(
                "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""
        );

        List<String> actualValues = csvParsingUtil.getAllAttributeValuesAsListContainer(response);

        assertEquals(expectedValues.size(), actualValues.size());
        assertEquals(expectedValues, actualValues);
    }

    @Test
    void testFetchInBulkMasterList_NullRequest() {
        Map<Integer, Map<String, MasterData>> result = csvParsingUtil.fetchInBulkMasterList(MasterListRequestV2.builder().build());
        assertTrue(result.isEmpty());
    }

    @Test
    void testFetchInBulkMasterList_EmptyRequestList() {
        MasterListRequestV2 request = new MasterListRequestV2();
        request.setMasterListRequests(new ArrayList<>());
        Map<Integer, Map<String, MasterData>> result = csvParsingUtil.fetchInBulkMasterList(request);
        assertTrue(result.isEmpty());
    }

    @Test
    void testFetchInBulkMasterList_NonEmptyRequestList_ValidData() {
        MasterListRequestV2 request = new MasterListRequestV2();
        request.setMasterListRequests(Arrays.asList(new MasterListRequest()));

        V1DataResponse response = new V1DataResponse();
        List<MasterData> entities = Arrays.asList(MasterData.builder().itemType(1).itemValue("A").build() , MasterData.builder().itemType(1).itemValue("B").build());
        response.setEntities(entities);

        when(v1Service.fetchMultipleMasterData(request)).thenReturn(response);
        when(jsonHelper.convertValueToList(response.getEntities(), MasterData.class)).thenReturn(entities);

        Map<Integer, Map<String, MasterData>> result = csvParsingUtil.fetchInBulkMasterList(request);
        assertFalse(result.isEmpty());
        assertEquals(1, result.size());
        assertTrue(result.get(1).containsKey("A"));
        assertTrue(result.get(1).containsKey("B"));
    }

}