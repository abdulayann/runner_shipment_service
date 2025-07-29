package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.impl.ShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.ContainersExcelModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ContainerBaseResponse;
import com.dpw.runner.shipment.services.dto.response.MdmContainerTypeResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.masterdata.response.CommodityResponse;
import com.dpw.runner.shipment.services.service.impl.ContainerV3FacadeService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.*;
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
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

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
        MockitoAnnotations.initMocks(this);
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
    void downloadContainers_noContainersFound_shouldReturnJsonError() throws Exception {
        MockHttpServletResponse response = new MockHttpServletResponse();
        BulkDownloadRequest request = new BulkDownloadRequest();
        request.setShipmentId("6");

        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
        when(shipmentsContainersMappingDao.findByShipmentId(any()))
                .thenReturn(List.of(new ShipmentsContainersMapping(1L, 2L))); // container ID
        when(containerDao.findAll(any(), any())).thenReturn(Page.empty());
        when(mockObjectMapper.writeValueAsString(any()))
                .thenReturn("{\"success\":false,\"message\":\"No containers found for given input.\"}");

        containerV3Util.downloadContainers(response, request);

        assertEquals(500, response.getStatus());
        assertTrue(response.getContentType().startsWith("application/json"));
        assertTrue(response.getContentAsString().contains("No containers found"));
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
        assertNotNull(containerV3Util.getAddedWeight(null, null, null, testContainer.getNetWeightUnit()));
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
        assertNotNull(containerV3Util.getAddedVolume(null, null, null, testContainer.getNetWeightUnit()));
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
        containerV3Util.setContainerNetWeight(testContainer);
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
        containerV3Util.setContainerNetWeight(testContainer);
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
        containerV3Util.setContainerNetWeight(testContainer);
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
        containerV3Util.setContainerNetWeight(testContainer);
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
                () -> containerV3Util.uploadContainers(null));
    }

    @Test
    void uploadContainers_shouldThrowValidationException_whenConsolidationIdIsNull() {
        requestData.setConsolidationId(null);
        assertThrows(ValidationException.class,
                () -> containerV3Util.uploadContainers(requestData));
    }

    @Test
    void uploadContainers_shouldCompleteSuccessfully_whenExcelIsEmpty() throws Exception {
        when(parser.parseExcelFile(any(), any(), any(), any(), any(), any(), any(), any(), any()))
                .thenReturn(Collections.emptyList());
        DependentServiceResponse mockResponse = new DependentServiceResponse();
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mockResponse);
        requestData.setConsolidationId(123L);
        containerV3Util.uploadContainers(requestData);
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

    private Containers createTestContainer() {
        Containers container = new Containers();
        container.setGuid(UUID.randomUUID());
        container.setContainerCode("TEST");
        container.setHsCode("847040");
        container.setCommodityCode("VALID_COMMODITY");
        container.setContainerNumber("CONT123");
        return container;
    }
}
