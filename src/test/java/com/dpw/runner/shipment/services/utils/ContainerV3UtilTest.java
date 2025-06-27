package com.dpw.runner.shipment.services.utils;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.dao.impl.ShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.ContainersExcelModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ContainerBaseResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import javax.servlet.http.HttpServletResponse;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.context.junit.jupiter.SpringExtension;

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

    @InjectMocks
    private ContainerV3Util containerV3Util;

    @Mock
    private ObjectMapper mockObjectMapper;

    private static ObjectMapper objectMapper;

    private static ShipmentDetails testShipment;

    private static Packing testPacking;


    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
            testContainer = jsonTestUtility.getTestContainer();
            objectMapper = JsonTestUtility.getMapper();
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

//    @Test
//    void downloadContainers_success() {
//        MockHttpServletResponse response = new MockHttpServletResponse();
//        BulkDownloadRequest request = new BulkDownloadRequest();
//        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
//        request.setConsolidationId("3");
//        request.setShipmentId("6");
//
//        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
//        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(new ConsolidationDetails()));
//        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testContainer)));
//        when(commonUtils.convertToList(anyList(), eq(ContainersExcelModel.class)))
//                .thenReturn(List.of(new ContainersExcelModel()));
//
//        assertDoesNotThrow(() -> containerV3Util.downloadContainers(response, request));
//        assertEquals("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", response.getContentType());
//    }
//
//    @Test
//    void downloadContainers_shipmentNotFound_shouldReturnJsonError() throws Exception {
//        MockHttpServletResponse response = new MockHttpServletResponse();
//        BulkDownloadRequest request = new BulkDownloadRequest();
//        request.setShipmentId("999");
//
//        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
//        when(mockObjectMapper.writeValueAsString(any())).thenReturn("{\"success\":false,\"message\":\"Shipment not found\"}");
//
//        containerV3Util.downloadContainers(response, request);
//
//        assertEquals(500, response.getStatus());
//        assertTrue(response.getContentType().startsWith("application/json"));
//
//        String responseBody = response.getContentAsString();
//        assertTrue(responseBody.contains("Shipment not found"));
//    }
//
//    @Test
//    void downloadContainers_consolidationNotFound_shouldReturnJsonError() throws Exception {
//        MockHttpServletResponse response = new MockHttpServletResponse();
//        BulkDownloadRequest request = new BulkDownloadRequest();
//        request.setConsolidationId("888");
//
//        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
//        when(mockObjectMapper.writeValueAsString(any()))
//                .thenReturn("{\"success\":false,\"message\":\"Consolidation not found\"}");
//
//        containerV3Util.downloadContainers(response, request);
//
//        assertEquals(500, response.getStatus());
//        assertTrue(response.getContentType().startsWith("application/json"));
//        assertTrue(response.getContentAsString().contains("Consolidation not found"));
//    }
//
//    @Test
//    void downloadContainers_noContainersFound_shouldReturnJsonError() throws Exception {
//        MockHttpServletResponse response = new MockHttpServletResponse();
//        BulkDownloadRequest request = new BulkDownloadRequest();
//        request.setShipmentId("6");
//
//        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
//        when(shipmentsContainersMappingDao.findByShipmentId(any()))
//                .thenReturn(List.of(new ShipmentsContainersMapping(1L, 2L))); // container ID
//        when(containerDao.findAll(any(), any())).thenReturn(Page.empty());
//        when(mockObjectMapper.writeValueAsString(any()))
//                .thenReturn("{\"success\":false,\"message\":\"No containers found for given input.\"}");
//
//        containerV3Util.downloadContainers(response, request);
//
//        assertEquals(500, response.getStatus());
//        assertTrue(response.getContentType().startsWith("application/json"));
//        assertTrue(response.getContentAsString().contains("No containers found"));
//    }
//
//    @Test
//    void downloadContainers_convertToListFails_shouldReturnJsonError() throws Exception {
//        MockHttpServletResponse response = new MockHttpServletResponse();
//        BulkDownloadRequest request = new BulkDownloadRequest();
//        request.setShipmentId("6");
//
//        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
//        when(shipmentsContainersMappingDao.findByShipmentId(any()))
//                .thenReturn(List.of(new ShipmentsContainersMapping(1L, 2L)));
//        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testContainer)));
//        when(commonUtils.convertToList(anyList(), eq(ContainersExcelModel.class)))
//                .thenThrow(new RuntimeException("Mapping failed"));
//        when(mockObjectMapper.writeValueAsString(any()))
//                .thenReturn("{\"success\":false,\"message\":\"Failed to generate Excel file. Please try again.\"}");
//
//        containerV3Util.downloadContainers(response, request);
//
//        assertEquals(500, response.getStatus());
//        assertTrue(response.getContentType().startsWith("application/json"));
//        assertTrue(response.getContentAsString().contains("Failed to generate Excel"));
//    }

    @Test
    void testAddAllUnlocationInSingleCallList() throws RunnerException {
        List<ContainerBaseResponse> containerResponseList = List.of(objectMapper.convertValue(testContainer, ContainerBaseResponse.class));
        containerV3Util.addAllUnlocationInSingleCallList(containerResponseList, Map.of("1", testContainer));
        assertNotNull(containerResponseList);
    }

    @Test
    void testAddAllCommodityTypesInSingleCall() throws RunnerException{
        List<ContainerBaseResponse> containerResponseList = List.of(objectMapper.convertValue(testContainer, ContainerBaseResponse.class));
        containerV3Util.addAllCommodityTypesInSingleCall(containerResponseList, Map.of("1", testContainer));
        assertNotNull(containerResponseList);
    }

    @Test
    void testAddAllMasterDataInSingleCall() throws RunnerException{
        List<ContainerBaseResponse> containerResponseList = List.of(objectMapper.convertValue(testContainer, ContainerBaseResponse.class));
        containerV3Util.addAllMasterDataInSingleCallList(containerResponseList, Map.of("1", testContainer));
        assertNotNull(containerResponseList);
    }

    @Test
    void testAddAllContainerTypesInSingleCall() throws RunnerException{
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
    void testResetContainerDataForRecalculation() throws RunnerException {
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
    void testAddNoOfPackagesToContainerFromShipment() throws RunnerException {
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
    void testAddNoOfPackagesToContainerFromShipment2() throws RunnerException {
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
    void testAddNoOfPackagesToContainerFromShipment3() throws RunnerException {
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
        containerV3Util.addNoOfPackagesToContainerFromPacks(testContainer, "1", "BKG");
        assertNotNull(testContainer.getPacks());
    }

    @Test
    void testAddNoOfPackagesToContainerFromShipment4() throws RunnerException {
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
        containerV3Util.addNoOfPackagesToContainerFromPacks(testContainer, "", "BKG");
        assertNull(testContainer.getPacks());
    }

    @Test
    void testAddNoOfPackagesToContainerFromShipment5() throws RunnerException {
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
    void testAddNoOfPackagesToContainerFromShipment6() throws RunnerException {
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
    void testContainerBeforeSave() throws RunnerException {
        testContainer.setId(1L);
        testContainer.setContainerCode("Code");
        testContainer.setContainerCount(2L);
        testContainer.setConsolidationId(1L);
        testContainer.setContainerNumber("12345678910");
        containerV3Util.containerBeforeSave(List.of(testContainer));
        assertNotNull(testContainer);
    }
}
