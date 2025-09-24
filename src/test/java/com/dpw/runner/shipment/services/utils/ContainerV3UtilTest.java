package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentVersionContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.impl.ConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.impl.ShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.request.ContainersExcelModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerBaseResponse;
import com.dpw.runner.shipment.services.dto.response.MdmContainerTypeResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.MultiValidationException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.masterdata.response.CommodityResponse;
import com.dpw.runner.shipment.services.service.impl.ContainerV3FacadeService;
import com.dpw.runner.shipment.services.service.interfaces.IApplicationConfigService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.v3.ShipmentsV3Util;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.*;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.*;
import java.util.List;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class ContainerV3UtilTest extends CommonMocks {

    private static Containers testContainer;

    private static JsonTestUtility jsonTestUtility;

    @Mock
    private ShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Mock
    private IV1Service v1Service;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ContainerValidationUtil containerValidationUtil;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private KafkaProducer producer;

    @Mock
    private IContainerDao containerDao;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    private ConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private ObjectMapper mockObjectMapper;

    private static ObjectMapper objectMapper;

    private static ShipmentDetails testShipment;

    private static Packing testPacking;

    @Mock
    private ContainerV3FacadeService containerV3FacadeService;

    @Mock
    private static MultipartFile mockFile;

    @Mock
    private CSVParsingUtil<Containers> parser;

    private static BulkUploadRequest requestData;

    @Mock
    private ThreadPoolTaskExecutor hsCodeValidationExecutor;

    @Mock
    private IMDMServiceAdapter mdmServiceAdapter;

    @Mock
    private ShipmentsV3Util shipmentsV3Util;

    @Mock
    private IApplicationConfigService applicationConfigService;

    @Mock
    private IPackingDao packingDao;

    @InjectMocks
    @Spy
    private ContainerV3Util containerV3Util;

    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
            testContainer = jsonTestUtility.getTestContainer();
            objectMapper = JsonTestUtility.getMapper();
            requestData = new BulkUploadRequest();
            mockFile = new MockMultipartFile(
                    "file",
                    "containers.xlsx",
                    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                    "test content".getBytes()
            );
            requestData.setFile(mockFile);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUp() {
        testContainer = jsonTestUtility.getTestContainer();
        testShipment = jsonTestUtility.getTestShipment();
        testPacking = jsonTestUtility.getTestPacking();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").multipleShipmentEnabled(true).build());
    }

    @Test
    void downloadContainers(){
        HttpServletResponse response = new MockHttpServletResponse();
        BulkDownloadRequest request = new BulkDownloadRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setConsolidationId("3");
        request.setShipmentId("6");
        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testContainer)));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(jsonTestUtility.getTestConsolidation()));
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testContainer)));
        when(commonUtils.convertToList(anyList(), eq(ContainersExcelModel.class))).thenReturn(List.of(objectMapper.convertValue(testContainer, ContainersExcelModel.class)));
        assertDoesNotThrow(() -> containerV3Util.downloadContainers(response, request));
        ShipmentVersionContext.isV3();
        assertDoesNotThrow(() -> containerV3Util.downloadContainers(response, request));
        ShipmentVersionContext.remove();
    }

    @Test
    void downloadContainers_success() {
        MockHttpServletResponse response = new MockHttpServletResponse();
        BulkDownloadRequest request = new BulkDownloadRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setConsolidationId("3");
        request.setShipmentId("6");

        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(new ConsolidationDetails()));
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testContainer)));
        when(commonUtils.convertToList(anyList(), eq(ContainersExcelModel.class)))
                .thenReturn(List.of(new ContainersExcelModel()));

        assertDoesNotThrow(() -> containerV3Util.downloadContainers(response, request));
        assertEquals("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", response.getContentType());
    }

    @Test
    void downloadContainers_shipmentNotFound_shouldReturnJsonError() throws Exception {
        MockHttpServletResponse response = new MockHttpServletResponse();
        BulkDownloadRequest request = new BulkDownloadRequest();
        request.setShipmentId("999");

        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
        when(mockObjectMapper.writeValueAsString(any())).thenReturn("{\"success\":false,\"message\":\"Shipment not found\"}");

        containerV3Util.downloadContainers(response, request);

        assertEquals(500, response.getStatus());
        assertTrue(response.getContentType().startsWith("application/json"));

        String responseBody = response.getContentAsString();
        assertTrue(responseBody.contains("Shipment not found"));
    }

    @Test
    void downloadContainers_consolidationNotFound_shouldReturnJsonError() throws Exception {
        MockHttpServletResponse response = new MockHttpServletResponse();
        BulkDownloadRequest request = new BulkDownloadRequest();
        request.setConsolidationId("888");

        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
        when(mockObjectMapper.writeValueAsString(any()))
                .thenReturn("{\"success\":false,\"message\":\"Consolidation not found\"}");

        containerV3Util.downloadContainers(response, request);

        assertEquals(500, response.getStatus());
        assertTrue(response.getContentType().startsWith("application/json"));
        assertTrue(response.getContentAsString().contains("Consolidation not found"));
    }

    @Test
    void downloadContainers_noContainersFound_shouldReturnJsonError() {
        MockHttpServletResponse response = new MockHttpServletResponse();
        BulkDownloadRequest request = new BulkDownloadRequest();
        request.setShipmentId("6");

        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
        when(shipmentsContainersMappingDao.findByShipmentId(any()))
                .thenReturn(List.of(new ShipmentsContainersMapping(1L, 2L))); // container ID
        when(containerDao.findAll(any(), any())).thenReturn(Page.empty());

        containerV3Util.downloadContainers(response, request);

        assertEquals(200, response.getStatus());
        assertFalse(response.getContentType().startsWith("application/json"));
    }

    @Test
    void downloadContainers_convertToListFails_shouldReturnJsonError() throws Exception {
        MockHttpServletResponse response = new MockHttpServletResponse();
        BulkDownloadRequest request = new BulkDownloadRequest();
        request.setShipmentId("6");

        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
        when(shipmentsContainersMappingDao.findByShipmentId(any()))
                .thenReturn(List.of(new ShipmentsContainersMapping(1L, 2L)));
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testContainer)));
        when(commonUtils.convertToList(anyList(), eq(ContainersExcelModel.class)))
                .thenThrow(new RuntimeException("Mapping failed"));
        when(mockObjectMapper.writeValueAsString(any()))
                .thenReturn("{\"success\":false,\"message\":\"Failed to generate Excel file. Please try again.\"}");

        containerV3Util.downloadContainers(response, request);

        assertEquals(500, response.getStatus());
        assertTrue(response.getContentType().startsWith("application/json"));
        assertTrue(response.getContentAsString().contains("Failed to generate Excel"));
    }

    @Test
    void testAddAllUnlocationInSingleCallList() {
        List<ContainerBaseResponse> containerResponseList = List.of(objectMapper.convertValue(testContainer, ContainerBaseResponse.class));
        containerV3Util.addAllUnlocationInSingleCallList(containerResponseList, Map.of("1", testContainer));
        assertNotNull(containerResponseList);
    }

    @Test
    void testAddAllCommodityTypesInSingleCall(){
        List<ContainerBaseResponse> containerResponseList = List.of(objectMapper.convertValue(testContainer, ContainerBaseResponse.class));
        containerV3Util.addAllCommodityTypesInSingleCall(containerResponseList, Map.of("1", testContainer));
        assertNotNull(containerResponseList);
    }

    @Test
    void testAddAllMasterDataInSingleCall(){
        List<ContainerBaseResponse> containerResponseList = List.of(objectMapper.convertValue(testContainer, ContainerBaseResponse.class));
        containerV3Util.addAllMasterDataInSingleCallList(containerResponseList, Map.of("1", testContainer));
        assertNotNull(containerResponseList);
    }

    @Test
    void testAddAllContainerTypesInSingleCall(){
        List<ContainerBaseResponse> containerResponseList = List.of(objectMapper.convertValue(testContainer, ContainerBaseResponse.class));
        containerV3Util.addAllContainerTypesInSingleCall(containerResponseList, Map.of("1", testContainer));
        assertNotNull(containerResponseList);
    }

    @Test
    void testGetAddedWeight() throws RunnerException{
        testContainer.setId(1L);
        testContainer.setGrossWeight(BigDecimal.ONE);
        testContainer.setGrossWeightUnit("KG");
        testContainer.setNetWeight(BigDecimal.ONE);
        testContainer.setNetWeightUnit("KG");
        assertNotNull(containerV3Util.getAddedWeight(testContainer.getGrossWeight(), testContainer.getGrossWeightUnit(), testContainer.getNetWeight(), testContainer.getNetWeightUnit()));
    }

    @Test
    void testGetAddedWeight1() throws RunnerException{
        testContainer.setId(1L);
        testContainer.setGrossWeight(BigDecimal.ONE);
        testContainer.setGrossWeightUnit("KG");
        testContainer.setNetWeight(BigDecimal.ONE);
        testContainer.setNetWeightUnit("KG");
        mockShipmentSettings();
        BigDecimal validResult = containerV3Util.getAddedWeight(
                BigDecimal.TEN,
                "KG",
                BigDecimal.ONE,
                testContainer.getNetWeightUnit()
        );
        assertNotNull(validResult);
    }

    @Test
    void testGetAddedVolume() throws RunnerException{
        testContainer.setId(1L);
        testContainer.setGrossVolume(BigDecimal.ONE);
        testContainer.setGrossVolumeUnit("M3");
        testContainer.setNetWeight(BigDecimal.ONE);
        testContainer.setNetWeightUnit("M3");
        assertNotNull(containerV3Util.getAddedVolume(testContainer.getGrossVolume(), testContainer.getGrossVolumeUnit(), testContainer.getNetWeight(), testContainer.getNetWeightUnit()));
    }

    @Test
    void testGetAddedVolume2() throws RunnerException{
        testContainer.setId(1L);
        testContainer.setGrossVolume(BigDecimal.ONE);
        testContainer.setGrossVolumeUnit("M3");
        testContainer.setNetWeight(BigDecimal.ONE);
        testContainer.setNetWeightUnit("M3");
        mockShipmentSettings();
        BigDecimal nullResult = containerV3Util.getAddedVolume(null, null, null, testContainer.getNetWeightUnit());
        assertNull(nullResult);

        // Test with valid inputs - should return non-null
        BigDecimal validResult = containerV3Util.getAddedVolume(
                BigDecimal.TEN,
                "M3",
                BigDecimal.ONE,
                testContainer.getNetWeightUnit()
        );
        assertNotNull(validResult);
    }

    @Test
    void testSetContainerNetWeight() throws RunnerException {
        testContainer.setId(1L);
        testContainer.setGrossWeight(BigDecimal.ONE);
        testContainer.setGrossWeightUnit("KG");
        testContainer.setNetWeight(BigDecimal.ONE);
        testContainer.setNetWeightUnit("KG");
        testContainer.setTareWeight(BigDecimal.ONE);
        testContainer.setTareWeightUnit("KG");
        containerV3Util.setContainerNetWeight(Collections.singletonList(testContainer));
        assertNotNull(testContainer.getNetWeight());
    }

    @Test
    void testSetContainerNetWeight1() throws RunnerException {
        testContainer.setId(1L);
        testContainer.setGrossWeight(null);
        testContainer.setGrossWeightUnit("KG");
        testContainer.setNetWeight(null);
        testContainer.setNetWeightUnit(null);
        testContainer.setTareWeight(BigDecimal.ONE);
        testContainer.setTareWeightUnit("KG");
        containerV3Util.setContainerNetWeight(Collections.singletonList(testContainer));
        assertNotNull(testContainer.getNetWeight());
    }

    @Test
    void testSetContainerNetWeight2() throws RunnerException {
        testContainer.setId(1L);
        testContainer.setGrossWeight(null);
        testContainer.setGrossWeightUnit(null);
        testContainer.setNetWeight(null);
        testContainer.setNetWeightUnit(null);
        testContainer.setTareWeight(BigDecimal.ONE);
        testContainer.setTareWeightUnit("KG");
        mockShipmentSettings();
        containerV3Util.setContainerNetWeight(Collections.singletonList(testContainer));
        assertNotNull(testContainer.getNetWeight());
    }

    @Test
    void testSetContainerNetWeight3() throws RunnerException {
        testContainer.setId(1L);
        testContainer.setGrossWeight(null);
        testContainer.setGrossWeightUnit(null);
        testContainer.setNetWeight(null);
        testContainer.setNetWeightUnit(null);
        testContainer.setTareWeight(BigDecimal.ONE);
        testContainer.setTareWeightUnit(null);
        containerV3Util.setContainerNetWeight(Collections.singletonList(testContainer));
        assertNull(testContainer.getNetWeight());
    }

    @Test
    void testResetContainerDataForRecalculation() {
        testContainer.setId(1L);
        testContainer.setGrossWeight(BigDecimal.ONE);
        testContainer.setGrossWeightUnit("KG");
        testContainer.setNetWeight(BigDecimal.ONE);
        testContainer.setNetWeightUnit("KG");
        testContainer.setTareWeight(BigDecimal.ONE);
        testContainer.setTareWeightUnit("KG");
        containerV3Util.resetContainerDataForRecalculation(testContainer);
        assertNotNull(testContainer.getNetWeight());
    }

    @Test
    void testAddNoOfPackagesToContainerFromShipment() {
        testContainer.setId(1L);
        testContainer.setGrossWeight(BigDecimal.ONE);
        testContainer.setGrossWeightUnit("KG");
        testContainer.setNetWeight(BigDecimal.ONE);
        testContainer.setNetWeightUnit("KG");
        testContainer.setTareWeight(BigDecimal.ONE);
        testContainer.setTareWeightUnit("KG");
        testShipment.setId(1L);
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        testShipment.setContainerAssignedToShipmentCargo(1L);
        testPacking.setId(1L);
        testPacking.setShipmentId(1L);
        testPacking.setContainerId(1L);
        containerV3Util.addNoOfPackagesToContainer(testContainer, 1, "BKG");
        assertNotNull(testContainer.getPacks());
    }

    @Test
    void testAddNoOfPackagesToContainerFromShipment2() {
        testContainer.setId(1L);
        testContainer.setGrossWeight(BigDecimal.ONE);
        testContainer.setGrossWeightUnit("KG");
        testContainer.setNetWeight(BigDecimal.ONE);
        testContainer.setNetWeightUnit("KG");
        testContainer.setTareWeight(BigDecimal.ONE);
        testContainer.setTareWeightUnit("KG");
        testContainer.setPacks(null);
        testContainer.setPacksType("BBG");
        testShipment.setId(1L);
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        testShipment.setContainerAssignedToShipmentCargo(1L);
        testPacking.setId(1L);
        testPacking.setShipmentId(1L);
        testPacking.setContainerId(1L);
        containerV3Util.addNoOfPackagesToContainer(testContainer, 1, "BKG");
        assertNotNull(testContainer.getPacks());
    }

    @Test
    void testAddNoOfPackagesToContainerFromShipment3() {
        testContainer.setId(1L);
        testContainer.setGrossWeight(BigDecimal.ONE);
        testContainer.setGrossWeightUnit("KG");
        testContainer.setNetWeight(BigDecimal.ONE);
        testContainer.setNetWeightUnit("KG");
        testContainer.setTareWeight(BigDecimal.ONE);
        testContainer.setTareWeightUnit("KG");
        testContainer.setPacks(null);
        testContainer.setPacksType("BKG");
        testShipment.setId(1L);
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        testShipment.setContainerAssignedToShipmentCargo(1L);
        testPacking.setId(1L);
        testPacking.setShipmentId(1L);
        testPacking.setContainerId(1L);
        containerV3Util.addNoOfPackagesValueToContainer(testContainer, "1", "BKG");
        assertNotNull(testContainer.getPacks());
    }

    @Test
    void testAddNoOfPackagesToContainerFromShipment4() {
        testContainer.setId(1L);
        testContainer.setGrossWeight(BigDecimal.ONE);
        testContainer.setGrossWeightUnit("KG");
        testContainer.setNetWeight(BigDecimal.ONE);
        testContainer.setNetWeightUnit("KG");
        testContainer.setTareWeight(BigDecimal.ONE);
        testContainer.setTareWeightUnit("KG");
        testContainer.setPacks(null);
        testContainer.setPacksType("BBG");
        testShipment.setId(1L);
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        testShipment.setContainerAssignedToShipmentCargo(1L);
        testPacking.setId(1L);
        testPacking.setShipmentId(1L);
        testPacking.setContainerId(1L);
        containerV3Util.addNoOfPackagesValueToContainer(testContainer, "", "BKG");
        assertNull(testContainer.getPacks());
    }

    @Test
    void testAddNoOfPackagesToContainerFromShipment5() {
        testContainer.setId(1L);
        testContainer.setGrossWeight(BigDecimal.ONE);
        testContainer.setGrossWeightUnit("KG");
        testContainer.setNetWeight(BigDecimal.ONE);
        testContainer.setNetWeightUnit("KG");
        testContainer.setTareWeight(BigDecimal.ONE);
        testContainer.setTareWeightUnit("KG");
        testContainer.setPacks(null);
        testContainer.setPacksType("BBG");
        testShipment.setId(1L);
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        testShipment.setContainerAssignedToShipmentCargo(1L);
        testPacking.setId(1L);
        testPacking.setShipmentId(1L);
        testPacking.setContainerId(1L);
        containerV3Util.addNoOfPackagesToContainer(testContainer, null, "");
        assertNull(testContainer.getPacks());
    }

    @Test
    void testAddNoOfPackagesToContainerFromShipment6() {
        testContainer.setId(1L);
        testContainer.setGrossWeight(BigDecimal.ONE);
        testContainer.setGrossWeightUnit("KG");
        testContainer.setNetWeight(BigDecimal.ONE);
        testContainer.setNetWeightUnit("KG");
        testContainer.setTareWeight(BigDecimal.ONE);
        testContainer.setTareWeightUnit("KG");
        testContainer.setPacks(null);
        testContainer.setPacksType("BBG");
        testShipment.setId(1L);
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        testShipment.setContainerAssignedToShipmentCargo(1L);
        testPacking.setId(1L);
        testPacking.setShipmentId(1L);
        testPacking.setContainerId(1L);
        containerV3Util.addNoOfPackagesToContainer(testContainer, null, "BKG");
        assertNull(testContainer.getPacks());
    }

    @Test
    void testSetWtVolUnits() {
        Containers containers = new Containers();
        when(commonUtils.getDefaultWeightUnit()).thenReturn("KG");
        when(commonUtils.getDefaultVolumeUnit()).thenReturn("M3");
        containerV3Util.setWtVolUnits(containers);
        assertEquals("KG", containers.getGrossWeightUnit());
    }

    @Test
    void testSetWtVolUnits1() {
        Containers containers = new Containers();
        containers.setGrossWeightUnit("KG");
        containers.setGrossVolumeUnit("M3");
        containerV3Util.setWtVolUnits(containers);
        assertEquals("KG", containers.getGrossWeightUnit());
    }

    @Test
    void uploadContainers_shouldThrowValidationException_whenRequestIsNull() {
        assertThrows(ValidationException.class,
                () -> containerV3Util.uploadContainers(null, CONSOLIDATION));
    }

    @Test
    void uploadContainers_shouldThrowValidationException_whenConsolidationIdIsNull() {
        requestData.setConsolidationId(null);
        assertThrows(ValidationException.class,
                () -> containerV3Util.uploadContainers(requestData, CONSOLIDATION));
    }

    @Test
    void uploadContainers_shouldCompleteSuccessfully_whenExcelIsEmpty() throws Exception {
        CSVParsingUtilV3<Containers> parserMock = Mockito.mock(CSVParsingUtilV3.class);
        Field parserField = ContainerV3Util.class.getDeclaredField("parserV3");
        parserField.setAccessible(true);
        parserField.set(containerV3Util, parserMock);
        when(parserMock.parseExcelFile(
                any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any()
        )).thenReturn(Collections.emptyList());
        DependentServiceResponse mockResponse = new DependentServiceResponse();
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mockResponse);
        requestData.setConsolidationId(123L);
        containerV3Util.uploadContainers(requestData, CONSOLIDATION);
        verify(containerV3FacadeService, never()).createUpdateContainer(any(), any());
    }

    @Test
    void validateHsCode_shouldThrowValidationException_whenHsCodeInvalid() {
        Containers container = createTestContainer();
        container.setHsCode("847040");
        List<Containers> containers = List.of(container);
        doAnswer(invocation -> {
            Runnable task = invocation.getArgument(0);
            task.run();
            return null;
        }).when(hsCodeValidationExecutor).execute(any(Runnable.class));
        CommodityResponse invalidCommodity = new CommodityResponse();
        invalidCommodity.setCode("232");
        V1DataResponse response = new V1DataResponse();
        response.setEntities(List.of(invalidCommodity));
        when(parser.getCommodityDataResponse(any()))
                .thenReturn(response);
        when(jsonHelper.convertValueToList(any(), eq(CommodityResponse.class)))
                .thenReturn(List.of(invalidCommodity));
        assertThrows(ValidationException.class,
                () -> containerV3Util.validateHsCode(containers));
        verify(hsCodeValidationExecutor).execute(any(Runnable.class));
        verify(parser).getCommodityDataResponse(any());
    }

    @Test
    void syncCommodityAndHsCode_shouldSyncCodes() {
        Containers container1 = new Containers();
        container1.setHsCode("847040");
        container1.setCommodityCode(null);
        Containers container2 = new Containers();
        container2.setHsCode(null);
        container2.setCommodityCode("COMM456");
        List<Containers> containers = List.of(container1, container2);
        Set<String> result = ContainerV3Util.syncCommodityAndHsCode(containers);
        assertEquals("847040", container1.getCommodityCode());
        assertEquals("COMM456", container2.getHsCode());
        assertEquals(2, result.size());
        assertTrue(result.contains("847040"));
        assertTrue(result.contains("COMM456"));
    }

    @Test
    void getValidHsCodes_shouldProcessBatchesConcurrently() {
        Set<String> hsCodes = Set.of("HS100");
        doAnswer(invocation -> {
            Runnable task = invocation.getArgument(0);
            task.run();
            return null;
        }).when(hsCodeValidationExecutor).execute(any(Runnable.class));
        when(parser.getCommodityDataResponse(anyList()))
                .thenAnswer(inv -> {
                    List<String> batch = inv.getArgument(0);
                    CommodityResponse commodity = new CommodityResponse();
                    commodity.setCode(batch.get(0));
                    V1DataResponse response = new V1DataResponse();
                    response.setEntities(List.of(commodity));
                    return response;
                });
        when(jsonHelper.convertValueToList(any(), eq(CommodityResponse.class)))
                .thenAnswer(inv -> {
                    List<?> entities = (List<?>) inv.getArgument(0);
                    return entities.stream()
                            .filter(CommodityResponse.class::isInstance)
                            .map(CommodityResponse.class::cast)
                            .collect(Collectors.toList());
                });
        Set<String> validCodes = containerV3Util.getValidHsCodes(hsCodes);
        assertEquals(1, validCodes.size(), "Should return a valid codes");
        assertTrue(validCodes.containsAll(hsCodes));
        verify(parser, times(1)).getCommodityDataResponse(anyList());
        verify(hsCodeValidationExecutor, times(1)).execute(any(Runnable.class));
    }

    @Test
    void getCodeTeuMapping_shouldReturnTeuMap() throws Exception {
        MdmContainerTypeResponse type1 = new MdmContainerTypeResponse();
        type1.setCode("TYPE1");
        type1.setTeu(BigDecimal.ONE);
        MdmContainerTypeResponse type2 = new MdmContainerTypeResponse();
        type2.setCode("TYPE2");
        type2.setTeu(BigDecimal.valueOf(2));
        DependentServiceResponse response = DependentServiceResponse.builder()
                .data(List.of(type1, type2))
                .build();
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(response);
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(type1, type2));
        Map<String, BigDecimal> result = containerV3Util.getCodeTeuMapping();
        assertEquals(2, result.size());
        assertEquals(BigDecimal.ONE, result.get("TYPE1"));
        assertEquals(BigDecimal.valueOf(2), result.get("TYPE2"));
    }

    @Test
    void createOrUpdateContainers_shouldProcessAllRequestsSuccessfully() throws RunnerException {
        List<ContainerV3Request> requests = List.of(
                new ContainerV3Request(),
                new ContainerV3Request(),
                new ContainerV3Request()
        );
        containerV3Util.createOrUpdateContainers(requests);
        verify(containerV3FacadeService, times(3))
                .createUpdateContainer(anyList(), eq("CONSOLIDATION"));
    }

    @Test
    void createOrUpdateContainers_shouldThrowRunnerException_whenSingleRequestFails() throws RunnerException {
        List<ContainerV3Request> requests = List.of(
                new ContainerV3Request(),
                new ContainerV3Request(),
                new ContainerV3Request()
        );
        when(containerV3FacadeService.createUpdateContainer(List.of(requests.get(0)), "CONSOLIDATION"))
                .thenReturn(new BulkContainerResponse());
        when(containerV3FacadeService.createUpdateContainer(List.of(requests.get(1)), "CONSOLIDATION"))
                .thenThrow(new RuntimeException("Database error"));
        RunnerException exception = assertThrows(RunnerException.class, () -> {
            containerV3Util.createOrUpdateContainers(requests);
        });
        assertEquals("Error processing row 2: Database error", exception.getMessage());
        verify(containerV3FacadeService, times(2))
                .createUpdateContainer(anyList(), eq("CONSOLIDATION"));
    }

    @Test
    void createOrUpdateContainers_shouldHandleEmptyList() throws RunnerException {
        List<ContainerV3Request> requests = Collections.emptyList();
        containerV3Util.createOrUpdateContainers(requests);
        verifyNoInteractions(containerV3FacadeService);
    }

    @Test
    void createOrUpdateContainers_shouldIncludeOriginalException() throws RunnerException {
        List<ContainerV3Request> requests = List.of(new ContainerV3Request());
        RuntimeException originalException = new RuntimeException("Original error");
        doThrow(originalException).when(containerV3FacadeService)
                .createUpdateContainer(anyList(), eq("CONSOLIDATION"));
        RunnerException thrownException = assertThrows(RunnerException.class, () -> {
            containerV3Util.createOrUpdateContainers(requests);
        });
        assertSame(originalException, thrownException.getCause());
        assertEquals("Error processing row 1: Original error", thrownException.getMessage());
    }

    @Test
    void testCreateOrUpdateContainers_AllSuccess() throws RunnerException {
        List<ContainerV3Request> requests = List.of(new ContainerV3Request(), new ContainerV3Request());
        when(containerV3FacadeService.createUpdateContainer(anyList(), eq("CONSOLIDATION")))
                .thenReturn(new BulkContainerResponse());
        containerV3Util.createOrUpdateContainers(requests);
        verify(containerV3FacadeService, times(2))
                .createUpdateContainer(anyList(), eq("CONSOLIDATION"));
    }

    @Test
    void testCreateOrUpdateContainers_ThrowsRunnerExceptionOnFailure() throws RunnerException {
        ContainerV3Request valid = new ContainerV3Request();
        ContainerV3Request failing = new ContainerV3Request();
        List<ContainerV3Request> requests = List.of(valid, failing);
        when(containerV3FacadeService.createUpdateContainer(
                argThat(list -> list != null && list.size() == 1 && list.get(0) == valid),
                eq("CONSOLIDATION"))
        ).thenReturn(null);
        when(containerV3FacadeService.createUpdateContainer(
                argThat(list -> list != null && list.size() == 1 && list.get(0) == failing),
                eq("CONSOLIDATION"))
        ).thenThrow(new RuntimeException("Some DB error"));
        RunnerException exception = assertThrows(RunnerException.class, () -> {
            containerV3Util.createOrUpdateContainers(requests);
        });
        assertTrue(exception.getMessage().contains("Error processing row 2"));
        assertTrue(exception.getMessage().contains("Some DB error"));
    }

    @Test
    void testCreateOrUpdateContainers_EmptyList_NoInteraction() throws RunnerException {
        containerV3Util.createOrUpdateContainers(Collections.emptyList());
        verifyNoInteractions(containerV3FacadeService);
    }

    @Test
    void testSetIdAndTeuInContainers() throws Exception {
        UUID guid = UUID.randomUUID();
        String code = "20GP";
        Long expectedId = 123L;
        BigDecimal expectedTeu = new BigDecimal("1.0");
        Long consolidationId = 999L;
        Containers container = new Containers();
        container.setGuid(guid);
        container.setContainerCode(code);
        List<Containers> containers = List.of(container);
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(consolidationId);
        Map<UUID, Long> guidToIdMap = Map.of(guid, expectedId);
        Map<String, BigDecimal> codeTeuMap = Map.of(code, expectedTeu);
        Method method = ContainerV3Util.class.getDeclaredMethod("setIdAndTeuInContainers",
                BulkUploadRequest.class, List.class, Map.class, Map.class);
        method.setAccessible(true);
        method.invoke(null, request, containers, guidToIdMap, codeTeuMap);
        Containers updated = containers.get(0);
        assertEquals(expectedId, updated.getId());
        assertEquals(expectedTeu, updated.getTeu());
        assertEquals(consolidationId, updated.getConsolidationId());
    }

    @Test
    void testGetValidHsCodes_whenEmptyInput_thenReturnsEmptySet() {
        Set<String> emptyHsCodes = new HashSet<>();
        Set<String> result = containerV3Util.getValidHsCodes(emptyHsCodes);
        assertNotNull(result);
        assertTrue(result.isEmpty(), "Expected result to be empty for empty input set");
    }

    @Test
    void validationContainerUploadInShipment_shouldReturnCorrectMap() {
        UUID guid1 = UUID.randomUUID();
        Containers container1 = new Containers();
        container1.setGuid(guid1);
        container1.setGrossVolume(new BigDecimal("10.5"));
        container1.setGrossVolumeUnit("CBM");
        container1.setGrossWeight(new BigDecimal("100.2"));
        container1.setGrossWeightUnit("KG");
        container1.setNetWeight(new BigDecimal(1));
        container1.setNetWeightUnit("KG");
        container1.setPacks("5");
        container1.setPacksType("BOX");
        List<Containers> containersList = List.of(container1);
        Map<UUID, Map<String, Object>> result = containerV3Util.validationContainerUploadInShipment(containersList);
        assertNotNull(result);
        assertTrue(result.containsKey(guid1));
        Map<String, Object> details = result.get(guid1);
        assertEquals(new BigDecimal("10.5"), details.get("grossVolume"));
        assertEquals("CBM", details.get("grossVolumeUnit"));
        assertEquals(new BigDecimal("100.2"), details.get("cargoWeight"));
        assertEquals("KG", details.get("cargoWeightUnit"));
        assertEquals("5", details.get("packs"));
        assertEquals("BOX", details.get("packsType"));
    }

    @Test
    void getContainerByModule_shouldReturnContainersOrThrowValidationException() {
        BulkUploadRequest requestWithConsolidation = new BulkUploadRequest();
        requestWithConsolidation.setConsolidationId(123L);
        BulkUploadRequest requestWithShipment = new BulkUploadRequest();
        requestWithShipment.setShipmentId(456L);
        List<Containers> consolidationContainers = List.of(new Containers());
        List<Containers> shipmentContainers = List.of(new Containers());
        when(containerDao.findByConsolidationId(123L)).thenReturn(consolidationContainers);
        when(containerDao.findByShipmentId(456L)).thenReturn(shipmentContainers);
        ValidationException ex1 = assertThrows(ValidationException.class, () -> containerV3Util.getContainerByModule(null, "CONSOLIDATION"));
        assertEquals("Please add the container and then try again.", ex1.getMessage());
        BulkUploadRequest req2 = new BulkUploadRequest();
        ValidationException ex2 = assertThrows(ValidationException.class, () -> containerV3Util.getContainerByModule(req2, "CONSOLIDATION"));
        assertEquals("Please add the consolidation and then try again.", ex2.getMessage());
        List<Containers> resultConsol = containerV3Util.getContainerByModule(requestWithConsolidation, "CONSOLIDATION");
        assertSame(consolidationContainers, resultConsol);
        BulkUploadRequest req4 = new BulkUploadRequest();
        ValidationException ex4 = assertThrows(ValidationException.class, () -> containerV3Util.getContainerByModule(req4, "SHIPMENT"));
        assertEquals("Please add the shipment Id and then try again.", ex4.getMessage());
        List<Containers> resultShip = containerV3Util.getContainerByModule(requestWithShipment, "SHIPMENT");
        assertSame(shipmentContainers, resultShip);
        ValidationException ex6 = assertThrows(ValidationException.class, () -> containerV3Util.getContainerByModule(requestWithShipment, "UNKNOWN_MODULE"));
        assertEquals("Module: UNKNOWN_MODULE; not found", ex6.getMessage());
    }

    @Test
    void validateBeforeAndAfterValues_shouldNotThrow_whenValuesAreEqual() {
        UUID containerId = UUID.randomUUID();
        Map<String, Object> from = new HashMap<>();
        from.put("grossWeight", new BigDecimal("10.0"));
        from.put("packs", "ABC");
        Map<String, Object> to = new HashMap<>();
        to.put("grossWeight", new BigDecimal("10.0"));
        to.put("packs", "ABC");
        assertDoesNotThrow(() -> ContainerV3Util.validateBeforeAndAfterValues(containerId, to, from, 1,new ArrayList<>()));
    }

    @Test
    void validateBeforeAndAfterValues_shouldNotThrow_whenValuesAreEqual2() {
        UUID containerId = UUID.randomUUID();
        Map<String, Object> from = new HashMap<>();
        from.put("grossWeight", null);
        from.put("packs", "ABC");
        Map<String, Object> to = new HashMap<>();
        to.put("grossWeight", null);
        to.put("packs", "ABC");
        assertDoesNotThrow(() -> ContainerV3Util.validateBeforeAndAfterValues(containerId, to, from, 1,new ArrayList<>()));
    }

    @Test
    void validateBeforeAndAfterValues_shouldThrow_whenBigDecimalIncreased() {
        UUID containerId = UUID.randomUUID();
        String errorLog = "Row# 3 - Gross Weight update is not allowed as Container is already assigned to package/ shipment.";
        List<String> errorList = new ArrayList<>();
        Map<String, Object> from = Map.of("grossWeight", new BigDecimal("10.0"));
        Map<String, Object> to = Map.of("grossWeight", new BigDecimal("20.0"));
        ContainerV3Util.validateBeforeAndAfterValues(containerId, to, from, 1, errorList);
        assertTrue(errorList.contains(errorLog));
    }

    @Test
    void validateBeforeAndAfterValues_shouldThrow_whenNonBigDecimalDiffers() {
        UUID containerId = UUID.randomUUID();
        String errorLog = "Row# 3 - Package update is not allowed as Container is already assigned to package/ shipment.";
        Map<String, Object> from = Map.of("packs", "ABC");
        Map<String, Object> to = Map.of("packs", "XYZ");
        List<String> errorList = new ArrayList<>();
        ContainerV3Util.validateBeforeAndAfterValues(containerId, to, from, 1, errorList);
        assertTrue(errorList.contains(errorLog));
    }

    @Test
    void validateIfPacksOrVolume_shouldCallValidationForConsolidation() {
        BulkUploadRequest request = new BulkUploadRequest();
        UUID guid = UUID.randomUUID();
        Containers container = new Containers();
        container.setId(1L);
        Map<UUID, Map<String, Object>> from = Map.of(guid, Map.of("packs", "A"));
        Map<UUID, Map<String, Object>> to = Map.of(guid, Map.of("packs", "A"));
        List<Containers> containersList = List.of(container);
        Map<UUID, Long> guidToIdMap = new HashMap<>();
        guidToIdMap.put(guid, 1L);
        assertDoesNotThrow(() -> containerV3Util.validateIfPacksOrVolume(from, to, request, "CONSOLIDATION", containersList, new ArrayList<>(), guidToIdMap ));
    }

    @Test
    void validateIfPacksOrVolume_shouldThrowWhenShipmentNotFound() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setShipmentId(1L);
        Map<UUID, Long> guidToIdMap = new HashMap<>();
        UUID guid = UUID.randomUUID();
        guidToIdMap.put(guid, 1L);
        when(shipmentDao.findById(1L)).thenReturn(Optional.empty());
        ValidationException ex = assertThrows(
                ValidationException.class,
                () -> callValidateIfPacksOrVolume(request, guidToIdMap)
        );
        assertEquals("Shipment Id not exists", ex.getMessage());
    }
    private void callValidateIfPacksOrVolume(BulkUploadRequest request, Map<UUID, Long> guidToIdMap) {
        containerV3Util.validateIfPacksOrVolume(Map.of(), Map.of(), request, "SHIPMENT", List.of(), new ArrayList<>(),guidToIdMap );
    }

    @Test
    void validateIfPacksOrVolume_shouldSkipValidation_whenPackingDaoEmpty() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setShipmentId(1L);
        ShipmentDetails details = new ShipmentDetails();
        details.setShipmentType("FCL");
        UUID guid = UUID.randomUUID();
        Map<UUID, Long> guidToIdMap = new HashMap<>();
        guidToIdMap.put(guid, 1L);
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(details));
        when(packingDao.findByContainerIdIn(anyList())).thenReturn(Collections.emptyList());
        assertDoesNotThrow(() -> containerV3Util.validateIfPacksOrVolume(Map.of(), Map.of(), request, "SHIPMENT", List.of(new Containers()), new ArrayList<>(), guidToIdMap));
    }

    @Test
    void validateIfPacksOrVolume_shouldThrowWhenChangedValueDetectedInShipment() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setShipmentId(1L);
        String errorLog = "Row# 2 - Package update is not allowed as Container is already assigned to package/ shipment.";
        UUID containerId = UUID.randomUUID();
        ShipmentDetails details = new ShipmentDetails();
        details.setShipmentType("FCL");
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(details));
        when(packingDao.findByContainerIdIn(anyList())).thenReturn(List.of(new Packing()));
        Map<UUID, Long> guidToIdMap = new HashMap<>();
        guidToIdMap.put(containerId, 1L);
        Map<UUID, Map<String, Object>> from = Map.of(containerId, Map.of("packs", "ABC"));
        Map<UUID, Map<String, Object>> to = Map.of(containerId, Map.of("packs", "XYZ"));
        List<Containers> containersList = List.of(new Containers() {{
            setId(100L);
            setGuid(containerId);
        }});
        List<String> errorList = new ArrayList<>();
        callValidateIfPacksOrVolume(from, to, request, containersList, guidToIdMap, errorList);
        assertTrue(errorList.contains(errorLog));
    }

    private void callValidateIfPacksOrVolume(
            Map<UUID, Map<String, Object>> from,
            Map<UUID, Map<String, Object>> to,
            BulkUploadRequest request,
            List<Containers> containersList, Map<UUID, Long> guidToIdMap, List<String> errorList
    ) {
        containerV3Util.validateIfPacksOrVolume(from, to, request, "SHIPMENT", containersList, errorList, guidToIdMap);
    }

    @Test
    void setShipmentOrConsoleId_shouldSetIdsBasedOnModule() {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setShipmentId(101L);
        request.setConsolidationId(202L);
        ContainerV3Request req1 = new ContainerV3Request();
        ContainerV3Request req2 = new ContainerV3Request();
        List<ContainerV3Request> requests = Arrays.asList(req1, req2);
        ContainerV3Util.setShipmentOrConsoleId(request, "SHIPMENT", requests);
        for (ContainerV3Request r : requests) {
            assertEquals(101L, r.getShipmentId());
            assertNull(r.getConsolidationId());
        }
        requests.forEach(r -> {
            r.setShipmentId(null);
            r.setConsolidationId(null);
        });
        ContainerV3Util.setShipmentOrConsoleId(request, "CONSOLIDATION", requests);
        for (ContainerV3Request r : requests) {
            assertEquals(202L, r.getConsolidationId());
            assertNull(r.getShipmentId());
        }
    }

    private Containers createTestContainer() {
        Containers container = new Containers();
        container.setGuid(UUID.randomUUID());
        container.setContainerCode("TEST");
        container.setHsCode("847040");
        container.setCommodityCode("VALID_COMMODITY");
        container.setContainerNumber("CONT123");
        return container;
    }

    @Test
    void testSetWtVolUnitsWithPacking() throws RunnerException {
        Containers container = new Containers();
        Packing packing = new Packing();
        packing.setWeight(BigDecimal.TEN);
        packing.setWeightUnit("KG");
        packing.setVolume(BigDecimal.ONE);
        packing.setVolumeUnit("M3");
        packing.setPacks("5");
        packing.setPacksType("BAG");

        containerV3Util.setWtVolUnits(container, packing);

        assertEquals("KG", container.getGrossWeightUnit());
        assertEquals("M3", container.getGrossVolumeUnit());
        assertEquals("BAG", container.getPacksType());
    }

    @Test
    void testSetWtVolUnitsWithShipmentDetails() throws RunnerException {
        Containers container = new Containers();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setWeight(BigDecimal.TEN);
        shipmentDetails.setWeightUnit("KG");
        shipmentDetails.setVolume(BigDecimal.ONE);
        shipmentDetails.setVolumeUnit("M3");
        shipmentDetails.setNoOfPacks(5);
        shipmentDetails.setPacksUnit("BAG");

        containerV3Util.setWtVolUnits(container, shipmentDetails);

        assertEquals("KG", container.getGrossWeightUnit());
        assertEquals("M3", container.getGrossVolumeUnit());
        assertEquals("BAG", container.getPacksType());
    }

    @Test
    void testSetContainerWeightUnit_WithNullSourceWeight_ShouldNotModifyContainer() throws RunnerException {

        Containers container = new Containers();
        container.setGrossWeight(BigDecimal.TEN);
        container.setGrossWeightUnit("KG");

        containerV3Util.setContainerWeightUnit(container, null, "LB");

        assertEquals(BigDecimal.TEN, container.getGrossWeight());
        assertEquals("KG", container.getGrossWeightUnit());
        verifyNoInteractions(commonUtils);
    }

    @Test
    void testSetContainerWeightUnit_WithNullGrossWeight_ShouldSetSourceWeightUnit() throws RunnerException {
        // Arrange
        Containers container = new Containers();
        container.setGrossWeight(null);
        container.setGrossWeightUnit("KG");

        containerV3Util.setContainerWeightUnit(container, BigDecimal.ONE, "LB");

        assertNull(container.getGrossWeight());
        assertEquals("LB", container.getGrossWeightUnit());
        verifyNoInteractions(commonUtils);
    }
    @Test
    void testSetContainerWeightUnit_WithDifferentWeightUnits_ShouldConvertToDefaultUnit() throws RunnerException {

        Containers container = new Containers();
        container.setGrossWeight(new BigDecimal("10.0"));
        container.setGrossWeightUnit("LB");

        when(commonUtils.getDefaultWeightUnit()).thenReturn("KG");

        // Mock the static method using MockedStatic
        try (MockedStatic<UnitConversionUtility> mockedStatic = mockStatic(UnitConversionUtility.class)) {
            mockedStatic.when(() -> UnitConversionUtility.convertUnit(
                    eq(Constants.MASS),
                    eq(new BigDecimal("10.0")),
                    eq("LB"),
                    eq("KG")
            )).thenReturn(4.536); // 10 LB = 4.536 KG

            containerV3Util.setContainerWeightUnit(container, BigDecimal.ONE, "TON");

            assertEquals(new BigDecimal("4.536"), container.getGrossWeight());
            assertEquals("KG", container.getGrossWeightUnit());
        }
    }

    @Test
    void testSetContainerVolumeUnit_WithDifferentVolumeUnits_ShouldConvertToDefaultUnit() throws RunnerException {
        // Arrange
        Containers container = new Containers();
        container.setGrossVolume(new BigDecimal("35.31"));
        container.setGrossVolumeUnit("FT3");

        when(commonUtils.getDefaultVolumeUnit()).thenReturn("M3");

        // Mock the static method using MockedStatic
        try (MockedStatic<UnitConversionUtility> mockedStatic = mockStatic(UnitConversionUtility.class)) {
            mockedStatic.when(() -> UnitConversionUtility.convertUnit(
                    eq(Constants.VOLUME),
                    eq(new BigDecimal("35.31")),
                    eq("FT3"),
                    eq("M3")
            )).thenReturn(1.0); // 35.31 FT3 = 1.0 M3

            // Act
            containerV3Util.setContainerVolumeUnit(container, BigDecimal.ONE, "L");

            // Assert
            assertEquals(new BigDecimal("1.0"), container.getGrossVolume());
            assertEquals("M3", container.getGrossVolumeUnit());

            // Verify interactions
            verify(commonUtils).getDefaultVolumeUnit();
            mockedStatic.verify(() -> UnitConversionUtility.convertUnit(
                    Constants.VOLUME,
                    new BigDecimal("35.31"),
                    "FT3",
                    "M3"
            ));
        }
    }



    @Test
    void testSetContainerPacksUnit() {
        Containers container = new Containers();
        container.setPacks("0");
        container.setPacksType(null);

        containerV3Util.setContainerPacksUnit(container, "5", "BAG");

        assertEquals("BAG", container.getPacksType());
    }

    @Test
    void testGetContainerNumberOrTypeWithContainerId() {
        Containers container = new Containers();
        container.setContainerNumber("12345");
        container.setContainerCode("20GP");
        when(containerDao.findById(anyLong())).thenReturn(Optional.of(container));

        String result = containerV3Util.getContainerNumberOrType(1L);

        assertEquals("12345", result);
    }

    @Test
    void testGetContainerNumberOrTypeWithContainer() {
        Containers container = new Containers();
        container.setContainerNumber(null);
        container.setContainerCode("20GP");

        String result = containerV3Util.getContainerNumberOrType(container);

        assertEquals("20GP", result);
    }

    @Test
    void testUpdatedContainerResponseForLCLandLTL_WithContainerAssigned() throws RunnerException {
        // Arrange
        ContainerBaseResponse container = new ContainerBaseResponse();
        container.setId(1L);

        List<ContainerBaseResponse> containerList = Collections.singletonList(container);

        ShipmentDetails shipmentDetails = mock(ShipmentDetails.class);
        when(shipmentDetails.getContainerAssignedToShipmentCargo()).thenReturn(1L);
        when(shipmentDetails.getWeight()).thenReturn(BigDecimal.valueOf(100));
        when(shipmentDetails.getWeightUnit()).thenReturn("KG");
        when(shipmentDetails.getVolume()).thenReturn(BigDecimal.valueOf(200));
        when(shipmentDetails.getVolumeUnit()).thenReturn("M3");
        when(shipmentDetails.getNoOfPacks()).thenReturn(5);
        when(shipmentDetails.getPacksUnit()).thenReturn("BAG");

        // Act
        containerV3Util.updatedContainerResponseForLCLandLTL(containerList, shipmentDetails);

        // Assert
        assertEquals(BigDecimal.valueOf(100), container.getGrossWeight());
        assertEquals("KG", container.getGrossWeightUnit());
        assertEquals(BigDecimal.valueOf(100), container.getNetWeight());
        assertEquals("KG", container.getNetWeightUnit());
        assertEquals(BigDecimal.valueOf(200), container.getGrossVolume());
        assertEquals("M3", container.getGrossVolumeUnit());
        assertEquals("5", container.getPacks());
        assertEquals("BAG", container.getPacksType());
    }

    @Test
    void testUpdatedContainerResponseForLCLandLTL_WithoutContainerAssigned_CallsUpdateContainersFromAssignedPackings() throws RunnerException {
        // Arrange
        ContainerBaseResponse container = new ContainerBaseResponse();
        container.setId(1L);

        List<ContainerBaseResponse> containerList = Collections.singletonList(container);

        ShipmentDetails shipmentDetails = mock(ShipmentDetails.class);
        when(shipmentDetails.getContainerAssignedToShipmentCargo()).thenReturn(null);
        when(shipmentDetails.getId()).thenReturn(101L);

        when(packingDao.findByShipmentId(101L)).thenReturn(Collections.emptyList());

        // Act
        containerV3Util.updatedContainerResponseForLCLandLTL(containerList, shipmentDetails);

        // Assert
        verify(packingDao).findByShipmentId(101L);
        // Since no packings, container details remain unchanged
        assertNull(container.getGrossWeight());
        assertNull(container.getGrossVolume());
        assertNull(container.getPacks());
    }

    @Test
    void testUpdatedContainerResponseForLCLandLTL_TriggersInternalUpdates() throws RunnerException {
        // Arrange
        ContainerBaseResponse container = new ContainerBaseResponse();
        container.setId(1L);
        List<ContainerBaseResponse> containerList = List.of(container);

        Packing packing1 = new Packing();
        packing1.setWeight(BigDecimal.valueOf(10));
        packing1.setWeightUnit("KG");
        packing1.setVolume(BigDecimal.valueOf(5));
        packing1.setVolumeUnit("M3");
        packing1.setPacks("2");
        packing1.setContainerId(1L);
        packing1.setPacksType("PKG");

        Packing packing2 = new Packing();
        packing2.setWeight(BigDecimal.valueOf(15));
        packing2.setWeightUnit("KG");
        packing2.setVolume(BigDecimal.valueOf(10));
        packing2.setVolumeUnit("M3");
        packing2.setPacks("3");
        packing2.setContainerId(1L);
        packing2.setPacksType("BAG");

        List<Packing> packings = Arrays.asList(packing1, packing2);
        testShipment.setContainerAssignedToShipmentCargo(null);
        testShipment.setId(100L);
        when(packingDao.findByShipmentId(100L)).thenReturn(packings);

        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setWeightChargeableUnit("KG");
        settings.setVolumeChargeableUnit("M3");
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(settings);

        when(shipmentsV3Util.resolveUnit(anyList(), eq("KG"))).thenReturn("KG");
        when(shipmentsV3Util.resolveUnit(anyList(), eq("M3"))).thenReturn("M3");
        when(shipmentsV3Util.resolveUnit(anyList(), eq("PKG"))).thenReturn("PKG");

        // Act
        containerV3Util.updatedContainerResponseForLCLandLTL(containerList, testShipment);

        // Assert
        assertEquals(BigDecimal.valueOf(25.0), container.getGrossWeight());
        assertEquals("KG", container.getGrossWeightUnit());
        assertEquals(BigDecimal.valueOf(25.0), container.getNetWeight());
        assertEquals("KG", container.getNetWeightUnit());
        assertEquals(BigDecimal.valueOf(15.0), container.getGrossVolume());
        assertEquals("M3", container.getGrossVolumeUnit());
        assertEquals("5", container.getPacks());
        assertEquals("PKG", container.getPacksType());
    }

    @Test
    void testProcessErrorListThrowsMultiValidationException() throws Exception {
        List<String> excelHeaders = Arrays.asList("ContainerNumber", "Type", "Weight");
        List<String> errorList = Arrays.asList(
                "Row 2: ContainerNumber is invalid",
                "Row 3: Type is invalid",
                "Row 2: ContainerNumber is invalid" // duplicate to test distinct()
        );
        List<Containers> containersList = Arrays.asList(new Containers(), new Containers(), new Containers());
        Method method = ContainerV3Util.class.getDeclaredMethod(
                "processErrorList", List.class, List.class, List.class
        );
        method.setAccessible(true);
        InvocationTargetException wrappedException = assertThrows(InvocationTargetException.class,
                () -> method.invoke(containerV3Util, excelHeaders, errorList, containersList));
        Throwable cause = wrappedException.getCause();
        assertTrue(cause instanceof MultiValidationException);
        MultiValidationException mvException = (MultiValidationException) cause;
        assertEquals("2 out of 3 Container Details contains errors, Please rectify and upload.", mvException.getMessage());
        assertEquals(2, mvException.getErrors().size()); // distinct and sorted
        assertTrue(mvException.getErrors().contains("Row 2: ContainerNumber is invalid"));
        assertTrue(mvException.getErrors().contains("Row 3: Type is invalid"));
    }

    @Test
    void testProcessErrorListDoesNotThrowWhenNoErrors() throws Exception {
        List<String> excelHeaders = Arrays.asList("ContainerNumber", "Type", "Weight");
        List<String> errorList = Collections.emptyList();
        List<Containers> containersList = Arrays.asList(new Containers(), new Containers());
        Method method = ContainerV3Util.class.getDeclaredMethod(
                "processErrorList", List.class, List.class, List.class
        );
        method.setAccessible(true);
        assertDoesNotThrow(() -> method.invoke(containerV3Util, excelHeaders, errorList, containersList));
    }

    @Test
    void testExtractHeaderOrderReturnsCorrectIndex() throws Exception {
        Map<String, Integer> headerOrder = new HashMap<>();
        headerOrder.put("containernumber", 0);
        headerOrder.put("type", 1);
        headerOrder.put("weight", 2);
        Method method = ContainerV3Util.class.getDeclaredMethod("extractHeaderOrder", String.class, Map.class);
        method.setAccessible(true);
        int index1 = (int) method.invoke(containerV3Util, "Row 2: ContainerNumber is invalid", headerOrder);
        assertEquals(0, index1);
        int index2 = (int) method.invoke(containerV3Util, "Row 3: Type is invalid", headerOrder);
        assertEquals(1, index2);
        int index3 = (int) method.invoke(containerV3Util, "Row 4: Weight is missing", headerOrder);
        assertEquals(2, index3);
        int indexUnknown = (int) method.invoke(containerV3Util, "Row 5: UnknownField is invalid", headerOrder);
        assertEquals(Integer.MAX_VALUE, indexUnknown);
    }

    @Test
    void testExtractRowNumCatchBlock() throws Exception {
        Method method = ContainerV3Util.class.getDeclaredMethod("extractRowNum", String.class);
        method.setAccessible(true);
        int result1 = (int) method.invoke(containerV3Util, "Some invalid message without row info");
        assertEquals(Integer.MAX_VALUE, result1);
        int result2 = (int) method.invoke(containerV3Util, "Row#ABC: ContainerNumber invalid");
        assertEquals(Integer.MAX_VALUE, result2);
        int result3 = (int) method.invoke(containerV3Util, "Row#5 ContainerNumber invalid");
        assertEquals(Integer.MAX_VALUE, result3);
    }

}
