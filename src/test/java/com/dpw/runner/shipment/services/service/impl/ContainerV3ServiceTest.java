package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.ShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingV3Request;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerBaseResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerListResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.AssignContainerRequest;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.UnAssignContainerRequest;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.projection.ContainerDeleteInfoProjection;
import com.dpw.runner.shipment.services.projection.ShipmentDetailsProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.service_bus.model.ContainerBoomiUniversalJson;
import com.dpw.runner.shipment.services.utils.ContainerV3Util;
import com.dpw.runner.shipment.services.utils.ContainerValidationUtil;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.ConsolidationValidationV3Util;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.Executors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.NETWORK_TRANSFER;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class ContainerV3ServiceTest extends CommonMocks {

    private static Containers testContainer;
    private static ConsolidationDetails testConsole;

    private static JsonTestUtility jsonTestUtility;

    @Mock
    private ShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Mock
    private ContainerV3Service self;

    @Mock
    private IV1Service v1Service;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private ISBUtils sbUtils;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IPackingDao packingDao;

    @Mock
    private ContainerValidationUtil containerValidationUtil;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private ConsolidationValidationV3Util consolidationValidationV3Util;
    @Mock
    private IShipmentsContainersMappingDao ishipmentsContainersMappingDao;
    @Mock
    private KafkaProducer producer;

    @Mock
    private IContainerDao containerDao;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private IPackingV3Service packingV3Service;

    @Mock
    private IShipmentServiceV3 shipmentService;

    @Mock
    private ContainerV3Util containerV3Util;

    @Mock
    private IContainerRepository containerRepository;

    @Mock
    private IConsolidationV3Service consolidationV3Service;

    @Mock
    private DependentServiceHelper dependentServiceHelper;

    @Mock
    private IAuditLogService auditLogService;

    @InjectMocks
    private ContainerV3Service containerV3Service;

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
        testConsole = jsonTestUtility.getTestConsolidation();
        testConsole.setShipmentsList(new HashSet<>());
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        containerV3Service.executorService = Executors.newFixedThreadPool(2);
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").multipleShipmentEnabled(true).build());
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void calculateContainerSummary() throws RunnerException {
        List<Containers> containersList = List.of(testContainer);
        mockShipmentSettings();
        mockTenantSettings();
        when(shipmentsContainersMappingDao.findByContainerIdIn(any())).thenReturn(List.of(new ShipmentsContainersMapping()));
        ContainerSummaryResponse containerSummaryResponse = containerV3Service.getContainerSummaryResponse(containersList, false, null);
        assertNotNull(containerSummaryResponse);
    }

    @Test
    void testDGPacks1(){
        Containers containers1 = new Containers();
        Packing packing = new Packing();
        packing.setHazardous(true);
        containers1.setPacksList(List.of(packing));

        int packCount = 0;
        int result = containerV3Service.getTotalDGPacks(containers1, packCount);
        assertEquals(1, result);
    }

    @Test
    void testDGPacks2(){
        Containers containers2 = new Containers();
        Packing packing2 = new Packing();
        packing2.setHazardous(false);
        containers2.setPacksList(List.of(packing2));

        int packCount = 0;
        int result = containerV3Service.getTotalDGPacks(containers2, packCount);
        assertEquals(packCount, result);
    }
    @Test
    void calculateContainerSummary_Branches() throws RunnerException{
        testContainer.setPacks(null);
        testContainer.setContainerCount(null);
        List<Containers> containersList = List.of(testContainer);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setWeightChargeableUnit(null);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setVolumeChargeableUnit(null);
        mockShipmentSettings();
        mockTenantSettings();
        when(shipmentsContainersMappingDao.findByContainerIdIn(any())).thenReturn(List.of(new ShipmentsContainersMapping()));
        ContainerSummaryResponse containerSummaryResponse = containerV3Service.getContainerSummaryResponse(containersList, false, null);
        assertNotNull(containerSummaryResponse);
    }

    @Test
    void testValidateContainerNumber_Success() {
        String validContainerNumber = "ABCD123456";
        ContainerNumberCheckResponse response = containerV3Service.validateContainerNumber(validContainerNumber);
        Assertions.assertNotNull(response);
        assertTrue(response.isSuccess());
    }

    @Test
    void testValidateContainerNumber_InvalidLength() {
        String invalidLengthContainerNumber = "ABC123";
        when(v1Service.fetchMasterData(any())).thenReturn(V1DataResponse.builder().entities(new ArrayList<>()).build());
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(new ArrayList<>());
        ContainerNumberCheckResponse response = containerV3Service.validateContainerNumber(invalidLengthContainerNumber);
        Assertions.assertNotNull(response);
        assertFalse(response.isSuccess());
    }

    @Test
    void testValidateContainerNumber_InvalidCharacters() {
        String invalidCharactersContainerNumber = "1234ABCD56";
        ContainerNumberCheckResponse response = containerV3Service.validateContainerNumber(invalidCharactersContainerNumber);
        Assertions.assertNotNull(response);
        assertFalse(response.isSuccess());
    }

    @Test
    void testValidateContainerNumber_WrongCheckDigit() {
        String invalidLengthContainerNumber = "CONT0000001";
        ContainerNumberCheckResponse response = containerV3Service.validateContainerNumber(invalidLengthContainerNumber);
        Assertions.assertNotNull(response);
        assertFalse(response.isSuccess());
    }

    @Test
    void testCreateBulkContainers() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ContainerV3Request request1 = ContainerV3Request.builder()
                .containerNumber("CNT123")
                .containerCode("20GP")
                .containerCount(1L)
                .commodityGroup("Metals")
                .consolidationId(101L)
                .build();

        ContainerV3Request request2 = ContainerV3Request.builder()
                .containerNumber("CNT456")
                .containerCode("20GP")
                .containerCount(1L)
                .commodityGroup("Metals")
                .consolidationId(101L)
                .build();

        List<ContainerV3Request> containerRequests = List.of(request1, request2);
        Containers container1 = new Containers();
        container1.setId(1L);
        container1.setContainerNumber("CNT123");

        Containers container2 = new Containers();
        container2.setId(2L);
        container2.setContainerNumber("CNT456");
        List<Containers> containers = List.of(container1, container2);

        ContainerResponse containerResponse1 = new ContainerResponse();
        containerResponse1.setContainerNumber("CNT123");
        ContainerResponse containerResponse2 = new ContainerResponse();
        containerResponse2.setContainerNumber("CNT456");
        List<ContainerResponse> responseList = List.of(containerResponse1, containerResponse2);

        doNothing().when(containerValidationUtil).validateCreateBulkRequest(containerRequests);
        doNothing().when(containerValidationUtil).validateContainerNumberUniquenessForCreateBulk(containerRequests);
        doNothing().when(dependentServiceHelper).pushToKafkaForDownStream(any(), anyString());
        when(jsonHelper.convertValueToList(containerRequests, Containers.class)).thenReturn(containers);
        when(containerDao.saveAll(containers)).thenReturn(containers);
        doNothing().when(auditLogService).addAuditLog(any());
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });

        when(jsonHelper.convertValueToList(containers, ContainerResponse.class)).thenReturn(responseList);

        // When
        BulkContainerResponse result = containerV3Service.createBulk(containerRequests, "CONSOLIDATION");

        // Then
        assertNotNull(result);
        assertEquals(2, result.getContainerResponseList().size());
        assertTrue(result.getMessage().contains("Bulk edit success! All selected containers have been updated."));
    }

    @Test
    void testContainerCreate() throws RunnerException {
        ContainerV3Request containerV3Request =ContainerV3Request.builder().id(1L).containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build();
        when(containerDao.findByConsolidationId(containerV3Request.getConsolidationId())).thenReturn(List.of(testContainer));
        when(jsonHelper.convertValue(any(), eq(Containers.class))).thenReturn(testContainer);
        doNothing().when(containerValidationUtil).validateContainerNumberUniqueness(anyString(), anyList());
        when(consolidationV3Service.fetchConsolidationDetails(any())).thenReturn(testConsole);
        when(containerDao.save(testContainer)).thenReturn(testContainer);
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);

        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(new ContainerResponse());
        ContainerResponse response = containerV3Service.create(containerV3Request, "CONSOLIDATION");
        assertNotNull(response);
    }

    @Test
    void testContainerUpdate() throws RunnerException {
        ContainerV3Request containerV3Request =ContainerV3Request.builder().id(1L).containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build();
        when(containerDao.findByConsolidationId(containerV3Request.getConsolidationId())).thenReturn(List.of(testContainer));
        when(jsonHelper.convertValueToList(any(), eq(Containers.class))).thenReturn(List.of(testContainer));
        when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
        when(jsonHelper.convertValueToList(any(), eq(ContainerResponse.class))).thenReturn(List.of(objectMapper.convertValue(testContainer, ContainerResponse.class)));
        when(consolidationV3Service.fetchConsolidationDetails(any())).thenReturn(testConsole);
        BulkContainerResponse response = containerV3Service.updateBulk(new ArrayList<>(List.of(containerV3Request)), "CONSOLIDATION");
        assertNotNull(response);
    }

    @Test
    void testContainerUpdate1() throws RunnerException {
        ContainerV3Request containerV3Request =ContainerV3Request.builder().id(1L).containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build();
        Containers containers = new Containers();
        containers.setId(1L);
        when(jsonHelper.convertValueToList(any(), eq(Containers.class))).thenReturn(List.of(containers));
        when(jsonHelper.convertValueToList(any(), eq(ContainerResponse.class))).thenReturn(List.of(objectMapper.convertValue(containers, ContainerResponse.class)));
        BulkContainerResponse response = containerV3Service.updateBulk(new ArrayList<>(List.of(containerV3Request)), "CONSOLIDATIONS");
        assertNotNull(response);
    }

    @Test
    void testContainerUpdate4() throws RunnerException {
        ContainerV3Request containerV3Request =ContainerV3Request.builder().containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build();
        Containers containers = new Containers();
        containers.setId(1L);
        when(jsonHelper.convertValueToList(any(), eq(Containers.class))).thenReturn(List.of(containers));
        when(jsonHelper.convertValueToList(any(), eq(ContainerResponse.class))).thenReturn(List.of(objectMapper.convertValue(containers, ContainerResponse.class)));
        BulkContainerResponse response = containerV3Service.updateBulk(new ArrayList<>(List.of(containerV3Request)), "CONSOLIDATIONS");
        assertNotNull(response);
    }

    @Test
    void testContainerUpdate2() throws RunnerException {
        ContainerV3Request containerV3Request =ContainerV3Request.builder().id(1L).containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build();
        when(containerDao.findByConsolidationId(containerV3Request.getConsolidationId())).thenReturn(List.of(testContainer));
        when(jsonHelper.convertValueToList(any(), eq(Containers.class))).thenReturn(List.of(testContainer));
        when(jsonHelper.convertValueToList(any(), eq(ContainerResponse.class))).
                thenReturn(List.of(objectMapper.convertValue(testContainer, ContainerResponse.class), objectMapper.convertValue(testContainer, ContainerResponse.class)));
        when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
        when(consolidationV3Service.fetchConsolidationDetails(any())).thenReturn(testConsole);
        BulkContainerResponse response = containerV3Service.updateBulk(new ArrayList<>(List.of(containerV3Request)), "CONSOLIDATION");
        assertNotNull(response);
    }


    @Test
    void testList() throws RunnerException {
        testContainer.setId(1L);
        ListCommonRequest request = new ListCommonRequest();
        IRunnerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerBaseResponse.class);
        Page<Containers> page = new PageImpl<>(List.of(testContainer) , PageRequest.of(0 , 10) , 1);

        when(containerDao.findAll(any(), any())).thenReturn(page);
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(),any())).thenReturn(containerResponse);
        when(packingDao.findByContainerIdIn(any())).thenReturn(new ArrayList<>());
        ContainerListResponse response = containerV3Service.list(request, anyBoolean(), anyString());

        assertNotNull(response);
    }

    @Test
    void testList_shouldThrowRunnerException_whenDaoFails() {
        // Arrange
        ListCommonRequest request = new ListCommonRequest();

        // Simulate exception during fetchData or dao interaction
        when(containerDao.findAll(any(), any())).thenThrow(new RuntimeException("DB failure"));

        // Act & Assert
        RunnerException exception = assertThrows(RunnerException.class, () -> {
            containerV3Service.list(request, true, null);
        });

        assertEquals("DB failure", exception.getMessage());  // Or DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG if null
    }

    @Test
    void testDeleteBulk() throws RunnerException {
        when(containerDao.findByIdIn(any())).thenReturn(new ArrayList<>(List.of(testContainer)));
        List<ContainerV3Request> containerV3Requests = List.of(ContainerV3Request.builder().id(1L).containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build());
        when(consolidationV3Service.fetchConsolidationDetails(any())).thenReturn(testConsole);
        BulkContainerResponse response = containerV3Service.deleteBulk(containerV3Requests, "CONSOLIDATION");
        assertNotNull(response);
    }

    @Test
    void testDeleteBulk2(){
        when(containerDao.findByIdIn(any())).thenReturn(new ArrayList<>());
        List<ContainerV3Request> containerV3Requests = List.of(ContainerV3Request.builder().id(1L).containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build());
        assertThrows(IllegalArgumentException.class, () ->containerV3Service.deleteBulk(containerV3Requests, "CONSOLIDATION"));
    }

    @Test
    void testDeleteBulk3() {
        // Arrange
        when(containerDao.findByIdIn(any())).thenReturn(List.of(testContainer));

        List<ContainerV3Request> containerV3Requests = List.of(
                ContainerV3Request.builder()
                        .id(1L)
                        .containerCode("Code")
                        .commodityGroup("FCR")
                        .containerCount(2L)
                        .consolidationId(1L)
                        .containerNumber("12345678910")
                        .build()
        );

        ContainerDeleteInfoProjection containerDeleteInfoProjection = mock(ContainerDeleteInfoProjection.class);
        when(containerDeleteInfoProjection.getContainerId()).thenReturn(1L);
        when(containerDeleteInfoProjection.getContainerNumber()).thenReturn("CONT123");
        when(containerDeleteInfoProjection.getShipmentId()).thenReturn("SHIP456");
        when(containerDeleteInfoProjection.getPacks()).thenReturn("2");
        when(containerDeleteInfoProjection.getPacksType()).thenReturn("Boxes");

        // Simulate the container being assigned to both packing and cargo
        List<ContainerDeleteInfoProjection> packingProjections = List.of(containerDeleteInfoProjection);
        List<ContainerDeleteInfoProjection> shipmentProjections = List.of(containerDeleteInfoProjection);

        when(containerDao.filterContainerIdsAttachedToPacking(any())).thenReturn(packingProjections);
        when(containerDao.filterContainerIdsAttachedToShipmentCargo(any())).thenReturn(shipmentProjections);

        // Act + Assert
        assertThrows(IllegalArgumentException.class,
                () -> containerV3Service.deleteBulk(containerV3Requests, "CONSOLIDATION"));
    }

    @Test
    void testDeleteBulk4(){
        when(containerDao.findByIdIn(any())).thenReturn(new ArrayList<>(List.of(testContainer)));
        List<ContainerV3Request> containerV3Requests = List.of(ContainerV3Request.builder().id(1L).containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build());
        ContainerDeleteInfoProjection containerDeleteInfoProjection = mock(ContainerDeleteInfoProjection.class);
        when(containerDeleteInfoProjection.getContainerNumber()).thenReturn("CONT123");
        when(containerDeleteInfoProjection.getShipmentId()).thenReturn("SHIP456");
        when(containerDeleteInfoProjection.getPacks()).thenReturn("2");
        List<ContainerDeleteInfoProjection> containerDeleteInfoProjections = List.of(containerDeleteInfoProjection);
        when(containerDao.filterContainerIdsAttachedToPacking(any())).thenReturn(containerDeleteInfoProjections);
        assertThrows(IllegalArgumentException.class, () -> containerV3Service.deleteBulk(containerV3Requests, "CONSOLIDATION"));
    }

    @Test
    void testDeleteBulk5(){
        when(containerDao.findByIdIn(any())).thenReturn(new ArrayList<>(List.of(testContainer)));
        List<ContainerV3Request> containerV3Requests = List.of(ContainerV3Request.builder().id(1L).containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build());
        ContainerDeleteInfoProjection containerDeleteInfoProjection = mock(ContainerDeleteInfoProjection.class);
        when(containerDeleteInfoProjection.getContainerNumber()).thenReturn("CONT123");
        when(containerDeleteInfoProjection.getShipmentId()).thenReturn("SHIP456");
        List<ContainerDeleteInfoProjection> containerDeleteInfoProjections = List.of(containerDeleteInfoProjection);
        when(containerDao.filterContainerIdsAttachedToShipmentCargo(any())).thenReturn(containerDeleteInfoProjections);
        assertThrows(IllegalArgumentException.class, () -> containerV3Service.deleteBulk(containerV3Requests, "CONSOLIDATION"));
    }

    @Test
    void testDeleteBulk5_1() {
        when(containerDao.findByIdIn(any())).thenReturn(new ArrayList<>(List.of(testContainer)));
        List<ContainerV3Request> containerV3Requests = List.of(
                ContainerV3Request.builder().id(1L).containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build());
        ContainerDeleteInfoProjection containerDeleteInfoProjection = mock(ContainerDeleteInfoProjection.class);
        when(containerDeleteInfoProjection.getContainerNumber()).thenReturn("CONT123");
        when(containerDeleteInfoProjection.getShipmentId()).thenReturn("SHIP456");
        List<ContainerDeleteInfoProjection> containerDeleteInfoProjections = List.of(containerDeleteInfoProjection);
        when(containerDao.filterContainerIdsAttachedToShipment(any())).thenReturn(containerDeleteInfoProjections);
        assertThrows(IllegalArgumentException.class, () -> containerV3Service.deleteBulk(containerV3Requests, "CONSOLIDATION"));
    }

    @Test
    void calculateContainerSummaryTest() throws RunnerException{
        List<Containers> containersList = List.of(testContainer);
        mockShipmentSettings();
        mockTenantSettings();
        when(containerDao.findByShipmentId(any())).thenReturn(containersList);
        ContainerSummaryResponse containerSummaryResponse = containerV3Service.calculateContainerSummary(1L, null, "CONSOLIDATION");
        assertNotNull(containerSummaryResponse);
    }

    @Test
    void calculateContainerSummaryTest2() throws RunnerException{
        List<Containers> containersList = List.of(testContainer);
        mockShipmentSettings();
        mockTenantSettings();
        when(containerDao.findByConsolidationId(any())).thenReturn(containersList);
        ContainerSummaryResponse containerSummaryResponse = containerV3Service.calculateContainerSummary(null, 1L, "CONSOLIDATION");
        assertNotNull(containerSummaryResponse);
    }

    @Test
    void calculateContainerSummaryTestNT() throws RunnerException{
        List<Containers> containersList = List.of(testContainer);
        mockShipmentSettings();
        mockTenantSettings();
        when(containerDao.findByShipmentIdWithoutTenantFilter(any())).thenReturn(containersList);
        ContainerSummaryResponse containerSummaryResponse = containerV3Service.calculateContainerSummary(1L, null, NETWORK_TRANSFER);
        assertNotNull(containerSummaryResponse);
    }

    @Test
    void calculateContainerSummaryTestNT2() throws RunnerException{
        List<Containers> containersList = List.of(testContainer);
        mockShipmentSettings();
        mockTenantSettings();
        when(containerDao.findByConsolidationIdWithoutTenantFilter(any())).thenReturn(containersList);
        ContainerSummaryResponse containerSummaryResponse = containerV3Service.calculateContainerSummary(null, 1L, NETWORK_TRANSFER);
        assertNotNull(containerSummaryResponse);
    }

    @Test
    void calculateContainerSummaryTestThrowsException(){
        assertThrows(RunnerException.class, () -> containerV3Service.calculateContainerSummary(null, null, "CONSOLIDATION"));
    }

    @Test
    void testFetchShipmentContainers() throws RunnerException{
        testContainer.setId(1L);
        IRunnerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerBaseResponse.class);
        Page<Containers> page = new PageImpl<>(List.of(testContainer) , PageRequest.of(0 , 10) , 1);
        when(containerDao.findAll(any(), any())).thenReturn(page);
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(),any())).thenReturn(containerResponse);
        ContainerListResponse containerListResponse = containerV3Service.fetchShipmentContainers(ListCommonRequest.builder().entityId("1").build(), Constants.SHIPMENT);
        assertNotNull(containerListResponse);
    }

    @Test
    void testFetchConsolidationContainers() throws RunnerException{
        testContainer.setId(1L);
        IRunnerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerBaseResponse.class);
        Page<Containers> page = new PageImpl<>(List.of(testContainer) , PageRequest.of(0 , 10) , 1);
        when(containerDao.findAll(any(), any())).thenReturn(page);
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(),any())).thenReturn(containerResponse);
        ContainerListResponse containerListResponse = containerV3Service.fetchConsolidationContainers(ListCommonRequest.builder().entityId("1").build(), Constants.CONSOLIDATION);
        assertNotNull(containerListResponse);
    }

    @Test
    void testAssignContainers() throws RunnerException{
        AssignContainerRequest request = new AssignContainerRequest();
        request.setShipmentPackIds(Map.of(1L, List.of(1L)));
        request.setContainerId(1L);
        testContainer.setId(1L);
        testShipment.setId(1L);
        testPacking.setId(1L);
        ContainerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerResponse.class);
        when(containerDao.findById(any())).thenReturn(Optional.of(testContainer));
        when(packingDao.findByIdIn(any())).thenReturn(new ArrayList<>(List.of(testPacking)));
        when(shipmentDao.findShipmentsByIds(anySet())).thenReturn(List.of(testShipment));
        when(containerDao.save(any())).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(containerResponse);
        ContainerResponse response = containerV3Service.assignContainers(request, Constants.CONTAINER);
        assertNotNull(response);
    }

    @Test
    void testAssignContainers1() throws RunnerException{
        var spyService = Mockito.spy(containerV3Service);
        AssignContainerRequest request = new AssignContainerRequest();
        request.setShipmentPackIds(Map.of(1L, List.of(1L)));
        request.setContainerId(1L);
        request.setAllowCargoDetachIfRequired(true);
        testContainer.setId(1L);
        testShipment.setId(1L);
        testShipment.setContainerAssignedToShipmentCargo(2L);
        testShipment.setPackingList(new ArrayList<>());
        testPacking.setId(1L);
        ContainerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerResponse.class);
        when(containerDao.findById(any())).thenReturn(Optional.of(testContainer));
        when(packingDao.findByIdIn(any())).thenReturn(new ArrayList<>(List.of(testPacking)));
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(testShipment));
        doReturn(new ContainerResponse()).when(self).unAssignContainers(any(), any());
        when(containerDao.save(any())).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(containerResponse);
        ContainerResponse response = spyService.assignContainers(request, Constants.CONTAINER);
        assertNotNull(response);
    }

    @Test
    void testAssignContainers2() throws RunnerException{
        var spyService = Mockito.spy(containerV3Service);
        AssignContainerRequest request = new AssignContainerRequest();
        request.setShipmentPackIds(Map.of(1L, List.of(1L)));
        request.setContainerId(1L);
        request.setAllowCargoDetachIfRequired(true);
        testContainer.setId(1L);
        testShipment.setId(1L);
        testShipment.setContainerAssignedToShipmentCargo(1L);
        testShipment.setPackingList(new ArrayList<>());
        testPacking.setId(1L);
        ContainerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerResponse.class);
        when(containerDao.findById(any())).thenReturn(Optional.of(testContainer));
        when(packingDao.findByIdIn(any())).thenReturn(new ArrayList<>(List.of(testPacking)));
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(testShipment));
        when(containerDao.save(any())).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(containerResponse);
        ContainerResponse response = spyService.assignContainers(request, Constants.CONTAINER);
        assertNotNull(response);
    }

    @Test
    void testUnAssignContainers() throws RunnerException{
        UnAssignContainerRequest request = new UnAssignContainerRequest();
        request.setShipmentPackIds(Map.of(1L, List.of(1L)));
        request.setContainerId(1L);
        testContainer.setId(1L);
        testShipment.setId(1L);
        testPacking.setId(1L);
        testPacking.setShipmentId(1L);
        ContainerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerResponse.class);
        when(containerDao.findById(any())).thenReturn(Optional.of(testContainer));
        when(packingDao.findByShipmentIdInAndContainerId(any(), any())).thenReturn(new ArrayList<>(List.of(testPacking)));
        when(shipmentDao.findShipmentsByIds(anySet())).thenReturn(List.of(testShipment));
        when(containerDao.save(any())).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(containerResponse);
        ContainerResponse response = containerV3Service.unAssignContainers(request, Constants.CONTAINER);
        assertNotNull(response);
    }

    @Test
    void testUnAssignContainers1() throws RunnerException{
        UnAssignContainerRequest request = new UnAssignContainerRequest();
        request.setShipmentPackIds(Map.of(1L, List.of(1L, 2L)));
        request.setContainerId(1L);
        testContainer.setId(1L);
        testShipment.setId(1L);
        testPacking.setId(1L);
        testPacking.setShipmentId(1L);
        ContainerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerResponse.class);
        when(containerDao.findById(any())).thenReturn(Optional.of(testContainer));
        when(packingDao.findByShipmentIdInAndContainerId(any(), any())).thenReturn(new ArrayList<>(List.of(testPacking)));
        when(shipmentDao.findShipmentsByIds(anySet())).thenReturn(List.of(testShipment));
        when(containerDao.save(any())).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(containerResponse);
        ContainerResponse response = containerV3Service.unAssignContainers(request, Constants.CONTAINER);
        assertNotNull(response);
    }

    @Test
    void testUnAssignContainers2() throws RunnerException{
        UnAssignContainerRequest request = new UnAssignContainerRequest();
        request.setShipmentPackIds(Map.of(1L, List.of(1L, 2L)));
        request.setContainerId(1L);
        testContainer.setId(1L);
        testShipment.setId(1L);
        testPacking.setId(3L);
        testPacking.setShipmentId(1L);
        ContainerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerResponse.class);
        when(containerDao.findById(any())).thenReturn(Optional.of(testContainer));
        when(packingDao.findByShipmentIdInAndContainerId(any(), any())).thenReturn(new ArrayList<>(List.of(testPacking)));
        when(shipmentDao.findShipmentsByIds(anySet())).thenReturn(List.of(testShipment));
        when(containerDao.save(any())).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(containerResponse);
        ContainerResponse response = containerV3Service.unAssignContainers(request, Constants.CONTAINER);
        assertNotNull(response);
    }

    @Test
    void testUnAssignContainers3() throws RunnerException{
        UnAssignContainerRequest request = new UnAssignContainerRequest();
        request.setShipmentPackIds(Map.of(1L, List.of(1L)));
        request.setContainerId(1L);
        testContainer.setId(1L);
        testShipment.setId(1L);
        testPacking.setId(1L);
        testPacking.setShipmentId(1L);
        ShipmentDetails testShipment1 = objectMapper.convertValue(testShipment, ShipmentDetails.class);
        testShipment1.setId(2L);
        Packing testPacking1 = objectMapper.convertValue(testPacking, Packing.class);
        testPacking1.setId(2L);
        testPacking1.setShipmentId(2L);
        ContainerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerResponse.class);
        when(containerDao.findById(any())).thenReturn(Optional.of(testContainer));
        when(packingDao.findByShipmentIdInAndContainerId(any(), any())).thenReturn(new ArrayList<>(List.of(testPacking, testPacking1)));
        when(shipmentDao.findShipmentsByIds(anySet())).thenReturn(List.of(testShipment, testShipment1));
        when(containerDao.save(any())).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(containerResponse);
        ContainerResponse response = containerV3Service.unAssignContainers(request, Constants.CONTAINER);
        assertNotNull(response);
    }

    @Test
    void testUnAssignContainers4() throws RunnerException{
        UnAssignContainerRequest request = new UnAssignContainerRequest();
        request.setShipmentPackIds(Map.of(1L, List.of(1L)));
        request.setContainerId(1L);
        testContainer.setId(1L);
        testShipment.setId(1L);
        testPacking.setId(1L);
        testPacking.setShipmentId(1L);
        ShipmentDetails testShipment1 = objectMapper.convertValue(testShipment, ShipmentDetails.class);
        testShipment1.setId(2L);
        testShipment1.setContainerAssignedToShipmentCargo(1L);
        Packing testPacking1 = objectMapper.convertValue(testPacking, Packing.class);
        testPacking1.setId(2L);
        testPacking1.setShipmentId(2L);
        ContainerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerResponse.class);
        when(containerDao.findById(any())).thenReturn(Optional.of(testContainer));
        when(packingDao.findByShipmentIdInAndContainerId(any(), any())).thenReturn(new ArrayList<>(List.of(testPacking, testPacking1)));
        when(shipmentDao.findShipmentsByIds(anySet())).thenReturn(List.of(testShipment, testShipment1));
        when(containerDao.save(any())).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(containerResponse);
        ContainerResponse response = containerV3Service.unAssignContainers(request, Constants.CONTAINER);
        assertNotNull(response);
    }

    @Test
    void testUnAssignContainers5() throws RunnerException{
        UnAssignContainerRequest request = new UnAssignContainerRequest();
        request.setShipmentPackIds(Map.of(1L, List.of(1L)));
        request.setContainerId(1L);
        testContainer.setId(1L);
        testShipment.setId(1L);
        testPacking.setId(1L);
        testPacking.setShipmentId(1L);
        ShipmentDetails testShipment1 = objectMapper.convertValue(testShipment, ShipmentDetails.class);
        testShipment1.setId(2L);
        Packing testPacking1 = objectMapper.convertValue(testPacking, Packing.class);
        testPacking1.setId(2L);
        testPacking1.setShipmentId(2L);
        testPacking1.setContainerId(1L);
        ContainerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerResponse.class);
        when(containerDao.findById(any())).thenReturn(Optional.of(testContainer));
        when(packingDao.findByShipmentIdInAndContainerId(any(), any())).thenReturn(new ArrayList<>(List.of(testPacking, testPacking1)));
        when(shipmentDao.findShipmentsByIds(anySet())).thenReturn(List.of(testShipment, testShipment1));
        when(containerDao.save(any())).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(containerResponse);
        ContainerResponse response = containerV3Service.unAssignContainers(request, Constants.CONTAINER);
        assertNotNull(response);
    }

    @Test
    void testUpdateAttachedContainersData() throws RunnerException{
        List<Long> containerIds = List.of(1L);
        testContainer.setId(1L);
        testShipment.setId(1L);
        testShipment.setContainerAssignedToShipmentCargo(1L);
        testPacking.setId(1L);
        testPacking.setShipmentId(1L);
        testPacking.setContainerId(1L);
        when(containerRepository.findByIdIn(any())).thenReturn(new ArrayList<>(List.of(testContainer)));
        when(packingDao.findByContainerIdIn(any())).thenReturn(new ArrayList<>(List.of(testPacking)));
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testShipment)));
        containerV3Service.updateAttachedContainersData(containerIds);
        assertNotNull(containerIds);
    }

    @Test
    void testUpdateAttachedContainersData1() {
        List<Long> containerIds = new ArrayList<>();
        assertDoesNotThrow(() -> containerV3Service.updateAttachedContainersData(containerIds));
    }

    @Test
    void testProcessAuditLogIds() {
        Set<Long> idsToProcess = Set.of(1L, 2L);
        Map<Long, Containers> oldContainerMap = Map.of(1L, testContainer);
        Map<Long, Containers> newContainerMap = Map.of(2L, testContainer);
        DBOperationType operationType = DBOperationType.UPDATE;
        containerV3Service.processAuditLogIds(idsToProcess, oldContainerMap, newContainerMap, operationType);
        assertNotNull(idsToProcess);
    }

    @Test
    void testProcessContainersAfterShipmentAttachment() {
        testShipment.setId(1L);
        testShipment.setContainersList(Set.of(testContainer));
        List<ShipmentDetails> shipmentDetailsList = List.of(testShipment);
        Set<Long> attachedShipmentIds = Set.of(1L);
        Set<Long> interBranchRequestedShipIds = Set.of(2L);
        containerV3Service.processContainersAfterShipmentAttachment(1L, shipmentDetailsList, attachedShipmentIds, interBranchRequestedShipIds);
        assertNotNull(shipmentDetailsList);
    }

    @Test
    void testFindContainerIdsAttachedToEitherPackingOrShipment() {
        assertDoesNotThrow(() -> containerV3Service.findContainerIdsAttachedToEitherPackingOrShipment(List.of(1L)));
    }

    @Test
    void testGetContainers() {
        assertDoesNotThrow(() -> containerV3Service.getContainers(List.of(1L)));
    }

    @Test
    void testGetSiblingContainers() {
        List<Containers> containersList = containerV3Service.getSiblingContainers(new ContainerV3Request());
        assertEquals(0, containersList.size());
    }

    @Test
    void testGetSiblingContainersWithRequest() {
        ContainerV3Request request = new ContainerV3Request();
        request.setShipmentsId(1L);
        when(containerDao.findByShipmentId(anyLong())).thenReturn(List.of(new Containers()));
        List<Containers> containersList = containerV3Service.getSiblingContainers(request);
        assertEquals(1, containersList.size());
    }

    @Test
    void testGetSiblingContainersWithRequest1() {
        ContainerV3Request request = new ContainerV3Request();
        request.setBookingId(1L);
        when(containerDao.findByBookingIdIn(anyList())).thenReturn(List.of(new Containers()));
        List<Containers> containersList = containerV3Service.getSiblingContainers(request);
        assertEquals(1, containersList.size());
    }

    @ParameterizedTest
    @ValueSource(strings = { "CONTAINER", "CON000000", "CONT000000#" })
    void testValidateContainerNumberFormat_InvalidCases(String containerNumber) {
        ContainerNumberCheckResponse response = new ContainerNumberCheckResponse();
        response.setSuccess(true);

        response = containerV3Service.validateContainerNumberFormat(containerNumber, response);

        assertFalse(response.isSuccess());
    }

    @Test
    void testDownloadContainers() {
        assertDoesNotThrow(() -> containerV3Service.downloadContainers(null, new BulkDownloadRequest()));
    }

    @Test
    void testAfterSaveList() {
        List<Containers> containersList = List.of(new Containers());
        assertDoesNotThrow(() -> containerV3Service.afterSaveList(containersList, true));
    }

    @Test
    void testAssignContainerCalculationsAndLogic() {
        AssignContainerRequest assignContainerRequest = new AssignContainerRequest();
        assignContainerRequest.setContainerId(1L);
        assignContainerRequest.setShipmentPackIds(Map.of(1L, new ArrayList<>()));

        List<Packing> assignedPacks = List.of(testPacking);
        Set<Long> assignedShipIds = Set.of(1L);
        Map<Long, ShipmentDetails> shipmentDetailsMap = Map.of(1L, testShipment);

        assertThrows(ValidationException.class, () -> performAssignment(
                shipmentDetailsMap,
                assignedShipIds,
                assignContainerRequest,
                assignedPacks
        ));
    }

    private void performAssignment(
            Map<Long, ShipmentDetails> shipmentDetailsMap,
            Set<Long> assignedShipIds,
            AssignContainerRequest assignContainerRequest,
            List<Packing> assignedPacks
    ) throws RunnerException {
        containerV3Service.assignContainerCalculationsAndLogic(
                shipmentDetailsMap,
                assignedShipIds,
                assignContainerRequest,
                new ArrayList<>(),   // Assuming empty list for some param
                testContainer,
                new HashMap<>(),     // Assuming empty map for some param
                assignedPacks,
                new ArrayList<>(),   // Assuming empty list for some param
                Constants.CONTAINER
        );
    }


    @Test
    void testAssignContainerCalculationsAndLogic1() {
        AssignContainerRequest assignContainerRequest = new AssignContainerRequest();
        assignContainerRequest.setContainerId(1L);
        assignContainerRequest.setShipmentPackIds(Map.of(1L, new ArrayList<>()));
        testPacking.setContainerId(null);
        List<Packing> assignedPacks = List.of(testPacking);
        Set<Long> assignedShipIds = Set.of(1L);
        testShipment.setPackingList(new ArrayList<>(List.of(testPacking)));
        Map<Long, ShipmentDetails> shipmentDetailsMap = Map.of(1L, testShipment);
        assertDoesNotThrow(() -> containerV3Service.assignContainerCalculationsAndLogic(shipmentDetailsMap, assignedShipIds, assignContainerRequest, new ArrayList<>(), testContainer, new HashMap<>(), assignedPacks, new ArrayList<>(), Constants.CONTAINER));
    }

    @Test
    void testAddShipmentCargoToContainerInCreateFromBooking() {
        CustomerBookingV3Request customerBookingV3Request = new CustomerBookingV3Request();
        customerBookingV3Request.setId(1L);
        testContainer.setId(1L);
        testContainer.setContainerNumber("CONT-123");
        assertDoesNotThrow(() -> containerV3Service.addShipmentCargoToContainerInCreateFromBooking(testContainer, customerBookingV3Request));
    }

    @Test
    void testNullShipmentConsoleId_Creation(){
        ContainerV3Request request = new ContainerV3Request();
        assertThrows(ValidationException.class, () -> containerV3Service.create(request, "SHIPMENT"));
    }

    @Test
    void testNonNullShipmentConsoleId_Creation(){
        ContainerV3Request request = new ContainerV3Request();
        request.setShipmentsId(1L);
        request.setConsolidationId(1L);
        request.setBookingId(1L);
        assertThrows(ValidationException.class, () -> containerV3Service.create(request, "SHIPMENT"));
    }

    @Test
    void testPushContainersToDependentServices_Success() {
        // Arrange
        List<Containers> containersList = createValidContainersList();
        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setLogicAppIntegrationEnabled(true);
        tenantSettings.setTransportOrchestratorEnabled(true);

        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        ContainerBoomiUniversalJson containerBoomiUniversalJson = new ContainerBoomiUniversalJson();
        containerBoomiUniversalJson.setHazardous(true);
        lenient().when(modelMapper.map(any(), eq(ContainerBoomiUniversalJson.class))).thenReturn(containerBoomiUniversalJson);
        lenient().when(jsonHelper.convertToJson(any())).thenReturn("jsonBody");

        // Act
        containerV3Service.pushContainersToDependentServices(containersList);

        // Assert
        verify(commonUtils, times(1)).getCurrentTenantSettings();
    }

    @Test
    void testPushContainersToDependentServices_NullContainersList() {
        // Arrange
        List<Containers> containersList = null;
        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setLogicAppIntegrationEnabled(true);

        // Act
        containerV3Service.pushContainersToDependentServices(containersList);

        // Assert
//        verify(commonUtils, times(1)).getCurrentTenantSettings();
        verify(producer, never()).produceToKafka(anyString(), anyString(), anyString());
    }

    @Test
    void testPushContainersToDependentServices_EmptyContainersList() {
        // Arrange
        List<Containers> containersList = new ArrayList<>();
        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setLogicAppIntegrationEnabled(true);

//        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);

        // Act
        containerV3Service.pushContainersToDependentServices(containersList);

        // Assert
//        verify(commonUtils, times(1)).getCurrentTenantSettings();
        verify(producer, never()).produceToKafka(anyString(), anyString(), anyString());

    }

    @Test
    void testPushContainersToDependentServices_LogicAppIntegrationDisabled() {
        // Arrange
        List<Containers> containersList = createValidContainersList();
        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setLogicAppIntegrationEnabled(false);
        tenantSettings.setTransportOrchestratorEnabled(false);

        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);

        // Act
        containerV3Service.pushContainersToDependentServices(containersList);

        // Assert
        verify(commonUtils, times(1)).getCurrentTenantSettings();
        verify(producer, never()).produceToKafka(anyString(), anyString(), anyString());
    }

    @Test
    void testPushContainersToDependentServices_TransportOrchestratorEnabled() {
        // Arrange
        List<Containers> containersList = createValidContainersList();
        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setLogicAppIntegrationEnabled(false);
        tenantSettings.setTransportOrchestratorEnabled(true);

        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        ContainerBoomiUniversalJson containerBoomiUniversalJson = new ContainerBoomiUniversalJson();
        containerBoomiUniversalJson.setHazardous(true);
        lenient().when(modelMapper.map(any(), eq(ContainerBoomiUniversalJson.class))).thenReturn(containerBoomiUniversalJson);
        lenient().when(jsonHelper.convertToJson(any())).thenReturn("jsonBody");

        // Act
        containerV3Service.pushContainersToDependentServices(containersList);

        // Assert
        verify(commonUtils, times(1)).getCurrentTenantSettings();

    }

    @Test
    void testPushContainersToDependentServices_TransportOrchestratorDisabled() {
        // Arrange
        List<Containers> containersList = createValidContainersList();
        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setLogicAppIntegrationEnabled(true);
        tenantSettings.setTransportOrchestratorEnabled(false);

        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        ContainerBoomiUniversalJson containerBoomiUniversalJson = new ContainerBoomiUniversalJson();
        containerBoomiUniversalJson.setHazardous(true);
        when(modelMapper.map(any(), eq(ContainerBoomiUniversalJson.class))).thenReturn(containerBoomiUniversalJson);
        when(jsonHelper.convertToJson(any())).thenReturn("jsonBody");

        // Act
        containerV3Service.pushContainersToDependentServices(containersList);

        // Assert
        verify(commonUtils, times(1)).getCurrentTenantSettings();
        verify(producer, never()).produceToKafka(anyString(), anyString(), anyString());

    }

    @Test
    void testPushContainersToDependentServices_EmptyPayloadDetails() {
        // Arrange
        List<Containers> containersList = createContainersListWithoutValidShipments();
        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setLogicAppIntegrationEnabled(true);

        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        ContainerBoomiUniversalJson containerBoomiUniversalJson = new ContainerBoomiUniversalJson();
        containerBoomiUniversalJson.setHazardous(true);
        lenient().when(modelMapper.map(any(), eq(ContainerBoomiUniversalJson.class))).thenReturn(containerBoomiUniversalJson);
        lenient().when(jsonHelper.convertToJson(any())).thenReturn("jsonBody");

        // Act
        containerV3Service.pushContainersToDependentServices(containersList);

        // Assert
        verify(commonUtils, times(1)).getCurrentTenantSettings();
        verify(producer, never()).produceToKafka(anyString(), anyString(), anyString());
    }

    @Test
    void testPushContainersToDependentServices_HazardousContainer() {
        // Arrange
        List<Containers> containersList = createHazardousContainersList();
        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setLogicAppIntegrationEnabled(true);
        tenantSettings.setTransportOrchestratorEnabled(true);

        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        ContainerBoomiUniversalJson containerBoomiUniversalJson = new ContainerBoomiUniversalJson();
        containerBoomiUniversalJson.setHazardous(true);
        when(modelMapper.map(any(), eq(ContainerBoomiUniversalJson.class))).thenReturn(containerBoomiUniversalJson);
        when(jsonHelper.convertToJson(any())).thenReturn("jsonBody");

        // Act
        containerV3Service.pushContainersToDependentServices(containersList);

        // Assert
        verify(commonUtils, times(1)).getCurrentTenantSettings();

    }

    @Test
    void testFetchConsolidationContainersForPackageAssignment() throws RunnerException {
        ListCommonRequest request = new ListCommonRequest();
        request.setEntityId("1");
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>()));
        assertNotNull(containerV3Service.fetchConsolidationContainersForPackageAssignment(request));
    }

    @Test
    void testFetchConsolidationContainersForPackageAssignment1() throws RunnerException {
        ListCommonRequest request = new ListCommonRequest();
        request.setEntityId("1");
        request.setContainsText("123");
        FilterCriteria filterCriteria = new FilterCriteria();
        filterCriteria.setInnerFilter(new ArrayList<>());
        request.setFilterCriteria(new ArrayList<>(List.of(filterCriteria)));
        testContainer.setId(1L);
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>(List.of(testContainer))));
        ContainerBaseResponse containerBaseResponse = new ContainerBaseResponse();
        when(commonUtils.setIncludedFieldsToResponse(any(), any(), any())).thenReturn(containerBaseResponse);
        assertNotNull(containerV3Service.fetchConsolidationContainersForPackageAssignment(request));
    }

    @Test
    void testFetchConsolidationContainersForPackageAssignment2() throws RunnerException {
        ListCommonRequest request = new ListCommonRequest();
        request.setEntityId("1");
        request.setContainsText("123");
        FilterCriteria filterCriteria = new FilterCriteria();
        filterCriteria.setInnerFilter(new ArrayList<>());
        request.setFilterCriteria(new ArrayList<>(List.of(filterCriteria)));
        testContainer.setId(1L);
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>(List.of(testContainer))));
        ContainerBaseResponse containerBaseResponse = new ContainerBaseResponse();
        when(commonUtils.setIncludedFieldsToResponse(any(), any(), any())).thenReturn(containerBaseResponse);
        List<ShipmentDetailsProjection> projections = new ArrayList<>();
        ShipmentDetailsProjection shipmentDetailsProjection = new ShipmentDetailsProjection() {
            @Override
            public Integer getTenantId() {
                return null;
            }

            @Override
            public String getHblNumber() {
                return null;
            }

            @Override
            public String getShipmentId() {
                return null;
            }

            @Override
            public String getShipmentType() {
                return null;
            }

            @Override
            public String getTransportMode() {
                return null;
            }

            @Override
            public Long getId() {
                return null;
            }

            @Override
            public String getShipmentNumber() {
                return null;
            }

            @Override
            public Long getContainerId() {
                return 1L;
            }
        };

        projections.add(shipmentDetailsProjection);
        when(shipmentService.findShipmentDetailsByAttachedContainerIds(any())).thenReturn(projections);
        assertNotNull(containerV3Service.fetchConsolidationContainersForPackageAssignment(request));
    }

    // Helper methods for test data creation
    private List<Containers> createValidContainersList() {
        List<Containers> containersList = new ArrayList<>();
        Containers container = new Containers();
        container.setId(1L);
        container.setContainerNumber("ABCD123456");
        container.setHazardous(false);

        Set<ShipmentDetails> shipmentsList = new HashSet<>();
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setBookingReference("BK123456");
        shipmentsList.add(shipment);

        container.setShipmentsList(shipmentsList);
        containersList.add(container);

        return containersList;
    }

    private List<Containers> createContainersListWithoutValidShipments() {
        List<Containers> containersList = new ArrayList<>();
        Containers container = new Containers();
        container.setId(1L);
        container.setContainerNumber("ABCD123456");
        container.setShipmentsList(new HashSet<>());

        containersList.add(container);

        return containersList;
    }

    private List<Containers> createHazardousContainersList() {
        List<Containers> containersList = new ArrayList<>();
        Containers container = new Containers();
        container.setId(1L);
        container.setContainerNumber("ABCD123456");
        container.setHazardous(true);
        container.setDgClass("Class 1");

        Set<ShipmentDetails> shipmentsList = new HashSet<>();
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setBookingReference("BK123456");
        shipmentsList.add(shipment);

        container.setShipmentsList(shipmentsList);
        containersList.add(container);

        return containersList;
    }
}
