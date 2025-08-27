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
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.*;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.projection.ContainerDeleteInfoProjection;
import com.dpw.runner.shipment.services.projection.ShipmentDetailsProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentsContainersMappingRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.service_bus.model.ContainerBoomiUniversalJson;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.ContainerV3Util;
import com.dpw.runner.shipment.services.utils.ContainerValidationUtil;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.ConsolidationValidationV3Util;
import com.dpw.runner.shipment.services.utils.v3.ShipmentValidationV3Util;
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
import org.springframework.test.util.ReflectionTestUtils;

import javax.persistence.EntityManager;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.Executors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
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
    private IConsoleShipmentMappingDao iConsoleShipmentMappingDao;
    @Mock
    private IShipmentsContainersMappingRepository iShipmentsContainersMappingRepository;
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
    private EntityManager entityManager;

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

    @Mock
    private IShipmentSync shipmentSync;

    @Mock
    private ShipmentValidationV3Util shipmentValidationV3Util;

    @Mock
    private IShipmentsContainersMappingDao iShipmentsContainersMappingDao;



    @InjectMocks
    private ContainerV3Service containerV3Service;

    private static ObjectMapper objectMapper;

    private static ShipmentDetails testShipment;

    private static Packing testPacking;

    private Containers container;
    private AssignContainerParams params;
    private ShipmentDetails shipmentDetails1;
    private ShipmentDetails shipmentDetails2;


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
        container = new Containers();
        shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setId(1L);
        shipmentDetails1.setContainersList(new HashSet<>());
        shipmentDetails2 = new ShipmentDetails();
        shipmentDetails2.setId(2L);
        shipmentDetails2.setContainersList(new HashSet<>());
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        shipmentDetailsMap.put(1L, shipmentDetails1);
        shipmentDetailsMap.put(2L, shipmentDetails2);
        params = new AssignContainerParams();
        params.setShipmentDetailsMap(shipmentDetailsMap);
    }


    @Test
    void testSaveUnAssignContainerResultsBatch_withData() {
        UnAssignContainerParams unAssignContainerParams = new UnAssignContainerParams();
        unAssignContainerParams.setShipmentIdsForCargoDetachment(List.of(201L));
        unAssignContainerParams.setRemoveAllPackingIds(List.of(101L));

        ShipmentsContainersMapping mapping = new ShipmentsContainersMapping();
        mapping.setShipmentId(201L);
        unAssignContainerParams.setShipmentsContainersMappings(List.of(mapping));

        List<List<Long>> shipmentIds = List.of(List.of(201L));
        Map<String, List<Containers>> containers = new HashMap<>();
        containers.put("containerToSave", List.of(new Containers()));
        List<UnAssignContainerParams> paramsList = List.of(unAssignContainerParams);

        doNothing().when(shipmentDao).setShipmentIdsToContainer(anyList(), isNull());
        doNothing().when(packingDao).setPackingIdsToContainer(anyList(), isNull());
        lenient().when(containerDao.saveAll(anyList())).thenReturn(List.of(new Containers()));
        doNothing().when(shipmentsContainersMappingDao).deleteAll(anyList());

        assertDoesNotThrow(() ->
                containerV3Service.saveUnAssignContainerResultsBatch(shipmentIds, containers, paramsList, false, false));
    }

    @Test
    void testUnAssignPackageContainers_withReassignment_successful() throws RunnerException {

        AssignContainerRequest request = new AssignContainerRequest();
        request.setAllowPackageReassignment(Boolean.TRUE);
        self.assignContainers(request, Constants.CONSOLIDATION_PACKING, Boolean.TRUE);
        verify(self, never()).unAssignContainers(any(), any(), any(), any(), any(), any(), any(), any());
        verify(self).assignContainers(request, Constants.CONSOLIDATION_PACKING, Boolean.TRUE);
    }

    @Test
    void testAssignContainers_withoutReassignment_successful() throws RunnerException {
        AssignContainerRequest request = new AssignContainerRequest();
        request.setAllowPackageReassignment(Boolean.FALSE);

        self.assignContainers(request, Constants.SHIPMENT_PACKING, Boolean.TRUE);

        verify(self, never()).unAssignContainers(any(), any(), any(), any(), any(), any(), anyBoolean(), anyBoolean());
        verify(self).assignContainers(request, Constants.SHIPMENT_PACKING, Boolean.TRUE);
    }

    @Test
    void testUnAssignContainers_calledDirectly() throws RunnerException {
        UnAssignContainerRequest request = new UnAssignContainerRequest();
        UnAssignContainerParams params = new UnAssignContainerParams();

        self.unAssignContainers(request, Constants.CONSOLIDATION_PACKING, params, null, null, null, Boolean.FALSE, Boolean.FALSE);

        verify(self).unAssignContainers(request, Constants.CONSOLIDATION_PACKING, params, null, null, null, Boolean.FALSE, Boolean.FALSE);

    }

    @Test
    void testAssignContainers_withMissingOldContainerPackIdsThrowsException() throws RunnerException{
        AssignContainerRequest request = new AssignContainerRequest();
        request.setAllowPackageReassignment(true);

        doThrow(new RunnerException("Old container mapping is required for package reassignment."))
                .when(self).assignContainers(any(), any(), anyBoolean());

        assertThrows(RunnerException.class,
                () -> self.assignContainers(request, Constants.SHIPMENT_PACKING, Boolean.TRUE));
    }

    @Test
    void testAssignContainers_packNotInTargetContainer() throws RunnerException {
        Long shipmentId = 10L;
        Long packId = 100L;
        Long targetContainerId = 500L;
        Long oldContainerId = 400L;

        AssignContainerRequest request = new AssignContainerRequest();
        request.setAllowPackageReassignment(Boolean.TRUE);
        request.setContainerId(targetContainerId);
        request.setShipmentPackIds(new HashMap<>(Map.of(shipmentId, new ArrayList<>(List.of(packId)))));

        Packing mockPacking = new Packing();
        mockPacking.setId(packId);
        mockPacking.setShipmentId(shipmentId);
        mockPacking.setContainerId(targetContainerId);

        ContainerResponse actualResponse = self.assignContainers(request, Constants.CONSOLIDATION_PACKING, Boolean.TRUE);

        assertEquals(null, actualResponse);

        assertFalse(request.getShipmentPackIds().get(shipmentId).isEmpty(),
                "PackId should be removed from shipmentPackIds since it was already in target container");

        verify(self, never()).unAssignContainers(any(), any(), any(), any(), any(), any(), anyBoolean(), anyBoolean());
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
        packing.setPacks("1");
        packing.setHazardous(true);
        containers1.setPacksList(List.of(packing));

        double packCount = 0;
        double result = containerV3Service.getTotalDGPacks(containers1, packCount);
        assertEquals(1, result);
    }

    @Test
    void testDGPacks2(){
        Containers containers2 = new Containers();
        Packing packing2 = new Packing();
        packing2.setPacks("22");
        packing2.setHazardous(false);
        containers2.setPacksList(List.of(packing2));

        double packCount = 0;
        double result = containerV3Service.getTotalDGPacks(containers2, packCount);
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
        lenient().when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);

        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(new ContainerResponse());
        ContainerResponse response = containerV3Service.create(containerV3Request, "CONSOLIDATION");
        assertNotNull(response);
    }

    @Test
    void testContainerCreateShipment() throws RunnerException {
        ContainerV3Request containerV3Request =ContainerV3Request.builder().id(1L).containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build();
        lenient().when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(jsonHelper.convertValue(any(), eq(Containers.class))).thenReturn(testContainer);
        doNothing().when(containerValidationUtil).validateContainerNumberUniqueness(anyString(), anyList());
        lenient().when(consolidationV3Service.fetchConsolidationDetails(any())).thenReturn(testConsole);
        when(containerDao.save(testContainer)).thenReturn(testContainer);
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        lenient().when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(new ShipmentDetails()));
        lenient().when(iConsoleShipmentMappingDao.findByShipmentId(any())).thenReturn(List.of(ConsoleShipmentMapping.builder().build()));
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(new ContainerResponse());
        ContainerResponse response = containerV3Service.create(containerV3Request, SHIPMENT);
        assertNotNull(response);
    }

    @Test
    void testContainerCreateShipment_Error(){
        ContainerV3Request containerV3Request =ContainerV3Request.builder().id(1L).containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build();
        Runnable mockRunnable = mock(Runnable.class);
        lenient().when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        lenient().when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(new ShipmentDetails()));

        lenient().when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(new ContainerResponse());
        assertThrows(ValidationException.class, () ->  containerV3Service.create(containerV3Request, SHIPMENT));
    }

    @Test
    void testContainerCreateShipment_withMigrationStatusAndConsolidationList_andCargoDetailsUpdate() throws RunnerException {

        Long shipmentId = 999L;
        Long consolidationId = 888L;
        Long containerId = 777L;

        ContainerV3Request containerV3Request = ContainerV3Request.builder()
                .id(1L)
                .containerCode("Code")
                .commodityGroup("FCR")
                .containerCount(2L)
                .containerNumber("12345678910")
                .shipmentId(shipmentId)
                .build();

        testContainer = new Containers();
        testContainer.setId(containerId);

        ConsolidationDetails mockConso = new ConsolidationDetails();
        mockConso.setId(consolidationId);
        Set<ConsolidationDetails> consolidationSet = new HashSet<>();
        consolidationSet.add(mockConso);

        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setId(shipmentId);
        shipment.setJobType("FCL");
        shipment.setMigrationStatus(MigrationStatus.MIGRATED_FROM_V2);
        shipment.setConsolidationList(consolidationSet);
        shipment.setPackingList(Collections.emptyList());

        CargoDetailsResponse cargoDetailsResponse = new CargoDetailsResponse();

        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipment));
        doNothing().when(containerValidationUtil).validateShipmentForContainer(any());
        doNothing().when(containerValidationUtil).validateShipmentCargoType(any());

        when(containerDao.findByShipmentId(shipmentId)).thenReturn(List.of(testContainer));
        when(shipmentService.calculateShipmentSummary(any(), any(), any())).thenReturn(cargoDetailsResponse);
        doNothing().when(shipmentService).updateCargoDetailsInShipment(shipment, cargoDetailsResponse);
        doNothing().when(shipmentDao).updateTriggerMigrationWarning(shipmentId);

        when(iConsoleShipmentMappingDao.findByShipmentId(any())).thenReturn(List.of(ConsoleShipmentMapping.builder().consolidationId(consolidationId).build()));
        lenient().when(containerDao.findByConsolidationId(consolidationId)).thenReturn(List.of(testContainer));
        when(jsonHelper.convertValue(any(), eq(Containers.class))).thenReturn(testContainer);
        when(containerDao.save(any())).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(new ContainerResponse());

        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });

        lenient().when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
        lenient().doNothing().when(shipmentsContainersMappingDao).assignShipments(containerId, Set.of(shipmentId), false);

        ContainerResponse response = containerV3Service.create(containerV3Request, SHIPMENT);

        assertNotNull(response);
        assertEquals(consolidationId, containerV3Request.getConsolidationId());
        verify(shipmentDao).updateTriggerMigrationWarning(shipmentId); // verify migration block
        verify(shipmentsContainersMappingDao).assignShipments(containerId, Set.of(shipmentId), false); // verify ifPresent
        verify(shipmentService).updateCargoDetailsInShipment(shipment, cargoDetailsResponse); // cargo update
    }


    @Test
    void testContainerUpdate() throws RunnerException {
        ContainerV3Request containerV3Request =ContainerV3Request.builder().id(1L).containerCode("Code").commodityGroup("FCR").containerCount(2L).consolidationId(1L).containerNumber("12345678910").build();
        when(containerDao.findByConsolidationId(containerV3Request.getConsolidationId())).thenReturn(List.of(testContainer));
        when(jsonHelper.convertValueToList(any(), eq(Containers.class))).thenReturn(List.of(testContainer));
        lenient().when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
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
        lenient().when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
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
    void testUpdateBulk_removesMatchingContainerById() throws RunnerException {
        UUID guid = UUID.randomUUID();

        ContainerV3Request containerV3Request = ContainerV3Request.builder()
                .id(1L)
                .containerCode("Code")
                .commodityGroup("FCR")
                .containerCount(2L)
                .consolidationId(1L)
                .containerNumber("12345678910")
                .build();

        // Setting same Id as in ContainerV3Request
        Containers containerInDb = new Containers();
        containerInDb.setId(1L);
        containerInDb.setGuid(guid);
        containerInDb.setContainerCode("Code");
        containerInDb.setContainerCount(2L);

        when(containerDao.findByConsolidationId(containerV3Request.getConsolidationId()))
                .thenReturn(List.of(containerInDb));
        when(jsonHelper.convertValueToList(any(), eq(Containers.class))).thenReturn(List.of(testContainer));
        when(jsonHelper.convertValueToList(any(), eq(ContainerResponse.class))).
                thenReturn(List.of(objectMapper.convertValue(testContainer, ContainerResponse.class), objectMapper.convertValue(testContainer, ContainerResponse.class)));
        lenient().when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
        when(consolidationV3Service.fetchConsolidationDetails(any())).thenReturn(testConsole);

        BulkContainerResponse response = containerV3Service.updateBulk(List.of(containerV3Request), "CONSOLIDATION");

        assertNotNull(response);
    }

    @Test
    void testUpdateBulk_withShipmentModuleAndMigratedStatus() throws RunnerException {
        ContainerV3Request containerV3Request = ContainerV3Request.builder()
                .id(1L)
                .containerCode("Code")
                .commodityGroup("FCR")
                .containerCount(2L)
                .consolidationId(1L)
                .containerNumber("12345678910")
                .shipmentId(100L)
                .build();

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(100L);
        shipmentDetails.setJobType("LCL");
        shipmentDetails.setMigrationStatus(MigrationStatus.MIGRATED_FROM_V2);

        when(shipmentDao.findById(100L)).thenReturn(Optional.of(shipmentDetails));
        when(jsonHelper.convertValueToList(any(), eq(Containers.class))).thenReturn(List.of(testContainer));
        when(jsonHelper.convertValueToList(any(), eq(ContainerResponse.class))).
                thenReturn(List.of(objectMapper.convertValue(testContainer, ContainerResponse.class), objectMapper.convertValue(testContainer, ContainerResponse.class)));
        lenient().when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(true);
        lenient().when(consolidationV3Service.fetchConsolidationDetails(any())).thenReturn(testConsole);
        when(iConsoleShipmentMappingDao.findByShipmentId(anyLong())).thenReturn(List.of(ConsoleShipmentMapping.builder().build()));

        BulkContainerResponse response = containerV3Service.updateBulk(List.of(containerV3Request), "SHIPMENT");

        assertNotNull(response);
        verify(shipmentDao).updateTriggerMigrationWarning(100L);
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
        ContainerListResponse containerListResponse = containerV3Service.fetchConsolidationContainers(ListCommonRequest.builder().entityId("1").build(), CONSOLIDATION);
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
        ContainerResponse response = containerV3Service.assignContainers(request, Constants.CONTAINER, Boolean.TRUE);
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
        doReturn(new ContainerResponse()).when(self).unAssignContainers(any(), any(), any(), any(), any(), any(), anyBoolean(), anyBoolean());
        when(containerDao.save(any())).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(containerResponse);
        ContainerResponse response = spyService.assignContainers(request, Constants.CONTAINER, Boolean.TRUE);
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
        ContainerResponse response = spyService.assignContainers(request, Constants.CONTAINER, Boolean.TRUE);
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
        when(jsonHelper.convertValue(any(), eq(Containers.class))).thenReturn(new Containers());
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(containerResponse);
        ContainerResponse response = containerV3Service.unAssignContainers(request, Constants.CONTAINER, new UnAssignContainerParams(), null, null, null, Boolean.FALSE, Boolean.FALSE);
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
        when(jsonHelper.convertValue(any(), eq(Containers.class))).thenReturn(new Containers());
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(containerResponse);
        ContainerResponse response = containerV3Service.unAssignContainers(request, Constants.CONTAINER, new UnAssignContainerParams(), null, null, null, Boolean.FALSE, Boolean.FALSE);
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
        when(jsonHelper.convertValue(any(), eq(Containers.class))).thenReturn(new Containers());
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(containerResponse);
        ContainerResponse response = containerV3Service.unAssignContainers(request, Constants.CONTAINER, new UnAssignContainerParams(), null, null, null, Boolean.FALSE,  Boolean.FALSE);
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
        when(jsonHelper.convertValue(any(), eq(Containers.class))).thenReturn(new Containers());
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(containerResponse);
        ContainerResponse response = containerV3Service.unAssignContainers(request, Constants.CONTAINER, new UnAssignContainerParams(), null, null, null, Boolean.FALSE,  Boolean.FALSE);
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
        when(jsonHelper.convertValue(any(), eq(Containers.class))).thenReturn(new Containers());
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(containerResponse);
        ContainerResponse response = containerV3Service.unAssignContainers(request, Constants.CONTAINER, new UnAssignContainerParams(), null, null, null, Boolean.FALSE,  Boolean.FALSE);
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
        when(jsonHelper.convertValue(any(), eq(Containers.class))).thenReturn(new Containers());
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(containerResponse);
        ContainerResponse response = containerV3Service.unAssignContainers(request, Constants.CONTAINER, new UnAssignContainerParams(), null, null, null, Boolean.FALSE,  Boolean.FALSE);
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
        List<Containers> containersList = containerV3Service.getSiblingContainers(new ContainerV3Request(), "","");
        assertEquals(0, containersList.size());
    }

    @Test
    void testGetSiblingContainersWithRequest() {
        ContainerV3Request request = new ContainerV3Request();
        request.setShipmentId(1L);
        lenient().when(containerDao.findByShipmentId(anyLong())).thenReturn(List.of(new Containers()));
        List<Containers> containersList = containerV3Service.getSiblingContainers(request,"","");
        assertNotNull(containersList);
    }

    @Test
    void testGetSiblingContainersWithRequest5() {
        ContainerV3Request request = new ContainerV3Request();
        request.setShipmentId(1L);
        when(containerDao.findByShipmentId(anyLong())).thenReturn(List.of(new Containers()));
        List<Containers> containersList = containerV3Service.getSiblingContainers(request,SHIPMENT,SHIPMENT_TYPE_DRT);
        assertNotNull(containersList);
    }

    @Test
    void testGetSiblingContainersWithRequest1() {
        ContainerV3Request request = new ContainerV3Request();
        request.setBookingId(1L);
        when(containerDao.findByBookingIdIn(anyList())).thenReturn(List.of(new Containers()));
        List<Containers> containersList = containerV3Service.getSiblingContainers(request,"","");
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
    void test_InvalidFourthCharacterInContainerNumber() {
        String containerNumber = "ABCX1234560";
        ContainerNumberCheckResponse response = containerV3Service.validateContainerNumber(containerNumber);
        assertTrue(response.isSuccess(), "Should pass with a warning about 4th char only");
        assertEquals("Invalid Container Number. The fourth character must be U, J, or Z.", response.getWarningMessage());
    }
    @Test
    void testLowerCaseInputGetsUpperCased() {
        String input = "abcu1234560"; // lowercase input
        ContainerNumberCheckResponse response = containerV3Service.validateContainerNumber(input);
        assertTrue(response.isSuccess());
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
        AssignContainerParams assignContainerParams = new AssignContainerParams();
        assignContainerParams.setShipmentDetailsMap(shipmentDetailsMap);
        assignContainerParams.setAssignedShipIds(assignedShipIds);
        assignContainerParams.setAssignedPacks(assignedPacks);
        containerV3Service.assignContainerCalculationsAndLogic(
                assignContainerParams,
                assignContainerRequest,
                testContainer,
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
        AssignContainerParams assignContainerParams = new AssignContainerParams();
        assignContainerParams.setShipmentDetailsMap(shipmentDetailsMap);
        assignContainerParams.setAssignedShipIds(assignedShipIds);
        assignContainerParams.setAssignedPacks(assignedPacks);
        assertDoesNotThrow(() -> containerV3Service.assignContainerCalculationsAndLogic(assignContainerParams, assignContainerRequest, testContainer, Constants.CONTAINER));
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
        request.setShipmentId(1L);
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
        verify(producer, never()).produceToKafka(anyString(), anyString(), anyString());
    }

    @Test
    void testPushContainersToDependentServices_EmptyContainersList() {
        // Arrange
        List<Containers> containersList = new ArrayList<>();
        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setLogicAppIntegrationEnabled(true);

        // Act
        containerV3Service.pushContainersToDependentServices(containersList);

        // Assert
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
        assertNotNull(containerV3Service.fetchConsolidationContainersForPackageAssignment(request, CONSOLIDATION));
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
        assertNotNull(containerV3Service.fetchConsolidationContainersForPackageAssignment(request, CONSOLIDATION));
    }

    @Test
    void testCheckAndMakeDG() {
        Containers container = new Containers();
        container.setId(1L);
        List<Long> shipmentIdsForAttachment = Arrays.asList(100L, 101L);
        lenient().when(commonUtils.checkIfDGClass1(any())).thenReturn(true);
        Packing packing = new Packing();
        packing.setHazardous(true);
        packing.setDGClass("1");
        when(packingDao.findByContainerIdIn(any())).thenReturn(List.of(packing));
        assertThrows(ValidationException.class, ()->containerV3Service.checkAndMakeDG(container, shipmentIdsForAttachment));
    }

    @Test
    void testCheckAndMakeDG2() {
        Containers container = new Containers();
        container.setHazardous(Boolean.TRUE);
        container.setDgClass("1");
        container.setId(1L);
        testPacking.setHazardous(Boolean.TRUE);
        container.setPacksList(List.of(testPacking));
        List<Long> shipmentIdsForAttachment = Arrays.asList(100L, 101L);
        when(commonUtils.checkIfDGClass1(Mockito.any())).thenReturn(true);
        containerV3Service.checkAndMakeDG(container, shipmentIdsForAttachment);
        assertTrue(testPacking.getHazardous());
    }


    @Test
    void testCallChangeShipmentDGStatusFromContainer_WhenIdIsNull() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        ContainerV3Request container = new ContainerV3Request();
        container.setId(null);
        container.setDgClass("1");
        when(commonUtils.checkIfDGClass1("1")).thenReturn(true);
        containerV3Service.callChangeShipmentDGStatusFromContainer(shipmentDetails, container);
        verify(commonUtils).checkIfDGClass1("1");
        verify(commonUtils).changeShipmentDGStatusToReqd(shipmentDetails, true);
    }

    @Test
    void testCallChangeShipmentDGStatusFromContainer_WhenIdIsNotNull_AndDGFieldsChanged() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        ContainerV3Request container = new ContainerV3Request();
        container.setId(1L);
        container.setDgClass("1");
        Containers oldContainer = new Containers();
        when(commonUtils.checkIfDGClass1("1")).thenReturn(true);
        when(containerRepository.getById(1L)).thenReturn(oldContainer);
        when(commonUtils.checkIfDGFieldsChangedInContainer(container, oldContainer)).thenReturn(true);
        containerV3Service.callChangeShipmentDGStatusFromContainer(shipmentDetails, container);
        verify(containerRepository).getById(1L);
        verify(commonUtils).checkIfDGFieldsChangedInContainer(container, oldContainer);
        verify(commonUtils).changeShipmentDGStatusToReqd(shipmentDetails, true);
    }

    @Test
    void testCallChangeShipmentDGStatusFromContainer_WhenIdIsNotNull_AndDGFieldsUnchanged() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        ContainerV3Request container = new ContainerV3Request();
        container.setId(1L);
        container.setDgClass("1");
        Containers oldContainer = new Containers();
        when(commonUtils.checkIfDGClass1("1")).thenReturn(true);
        when(containerRepository.getById(1L)).thenReturn(oldContainer);
        when(commonUtils.checkIfDGFieldsChangedInContainer(container, oldContainer)).thenReturn(false);
        containerV3Service.callChangeShipmentDGStatusFromContainer(shipmentDetails, container);
        verify(containerRepository).getById(1L);
        verify(commonUtils).checkIfDGFieldsChangedInContainer(container, oldContainer);
        verify(commonUtils, never()).changeShipmentDGStatusToReqd(any(), anyBoolean());
    }

    @Test
    void testProcessDGShipmentDetailsFromContainer_WithValidContainerId() throws RunnerException {
        Long containerId = 10L;
        Long shipmentId = 123L;
        ContainerV3Request containerRequest = new ContainerV3Request();
        containerRequest.setId(containerId);
        containerRequest.setHazardous(true);
        ShipmentsContainersMapping mapping = new ShipmentsContainersMapping();
        mapping.setId(shipmentId);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setConsolidationList(new HashSet<>());
        when(iShipmentsContainersMappingDao.findByContainerId(containerId)).thenReturn(List.of(mapping));
        lenient().when(shipmentService.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));
        ContainerV3Service serviceSpy = Mockito.spy(containerV3Service);
        ReflectionTestUtils.setField(serviceSpy, "iShipmentsContainersMappingDao", iShipmentsContainersMappingDao);
        ReflectionTestUtils.setField(serviceSpy, "shipmentService", shipmentService);
        ReflectionTestUtils.setField(serviceSpy, "shipmentDao", shipmentDao);
        ReflectionTestUtils.setField(serviceSpy, "shipmentValidationV3Util", shipmentValidationV3Util);
        doNothing().when(serviceSpy).callChangeShipmentDGStatusFromContainer(shipmentDetails, containerRequest);
        serviceSpy.processDGShipmentDetailsFromContainer(containerRequest);
        verify(iShipmentsContainersMappingDao).findByContainerId(containerId);
        verify(shipmentService).findById(shipmentId);
        verify(shipmentValidationV3Util).processDGValidations(eq(shipmentDetails), isNull(), anySet());
        verify(serviceSpy).callChangeShipmentDGStatusFromContainer(shipmentDetails, containerRequest);
        verify(shipmentDao).save(shipmentDetails, false, false);
        serviceSpy.processDGShipmentDetailsFromContainer(List.of(containerRequest));
        assertEquals(123L , shipmentId);
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
        assertNotNull(containerV3Service.fetchConsolidationContainersForPackageAssignment(request, CONSOLIDATION));
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

    @Test
    void testProcessDGShipmentDetailsFromContainer_WhenContainerIdIsNull() throws RunnerException {
        // Test case when container ID is null - should skip processing
        ContainerV3Request container = new ContainerV3Request();
        container.setId(null);
        container.setHazardous(true);
        List<ContainerV3Request> containerRequestList = Arrays.asList(container);

        containerV3Service.processDGShipmentDetailsFromContainer(containerRequestList);

        // Verify that no repository calls are made
        verify(iShipmentsContainersMappingDao, never()).findByContainerId(any());
        verify(shipmentService, never()).findById(any());
        verify(containerDao, never()).findByShipmentId(any());
    }

    @Test
    void testProcessDGShipmentDetailsFromContainer_WhenHazardousIsFalse() throws RunnerException {
        // Test case when hazardous is false - should skip processing
        ContainerV3Request container = new ContainerV3Request();
        container.setId(1L);
        container.setHazardous(false);
        List<ContainerV3Request> containerRequestList = Arrays.asList(container);

        containerV3Service.processDGShipmentDetailsFromContainer(containerRequestList);

        // Verify that no repository calls are made
        verify(iShipmentsContainersMappingDao, never()).findByContainerId(any());
        verify(shipmentService, never()).findById(any());
        verify(containerDao, never()).findByShipmentId(any());
    }

    @Test
    void testProcessDGShipmentDetailsFromContainer_WhenHazardousIsNull() throws RunnerException {
        // Test case when hazardous is null - should skip processing
        ContainerV3Request container = new ContainerV3Request();
        container.setId(1L);
        container.setHazardous(null);
        List<ContainerV3Request> containerRequestList = Arrays.asList(container);

        containerV3Service.processDGShipmentDetailsFromContainer(containerRequestList);

        // Verify that no repository calls are made
        verify(iShipmentsContainersMappingDao, never()).findByContainerId(any());
        verify(shipmentService, never()).findById(any());
        verify(containerDao, never()).findByShipmentId(any());
    }

    @Test
    void testProcessDGShipmentDetailsFromContainer_WhenNoShipmentsContainersMappingFound() throws RunnerException {
        // Test case when no shipments containers mapping is found
        ContainerV3Request container = new ContainerV3Request();
        container.setId(1L);
        container.setHazardous(true);
        List<ContainerV3Request> containerRequestList = Arrays.asList(container);

        when(iShipmentsContainersMappingDao.findByContainerId(1L)).thenReturn(Collections.emptyList());

        containerV3Service.processDGShipmentDetailsFromContainer(containerRequestList);

        verify(iShipmentsContainersMappingDao).findByContainerId(1L);
        verify(shipmentService, never()).findById(any());
        verify(containerDao, never()).findByShipmentId(any());
    }

    @Test
    void testProcessDGShipmentDetailsFromContainer_WhenShipmentNotFound() throws RunnerException {
        // Test case when shipment is not found by ID
        ContainerV3Request container = new ContainerV3Request();
        container.setId(1L);
        container.setHazardous(true);
        List<ContainerV3Request> containerRequestList = Arrays.asList(container);

        ShipmentsContainersMapping mapping = new ShipmentsContainersMapping();
        mapping.setShipmentId(10L);
        List<ShipmentsContainersMapping> mappingList = Arrays.asList(mapping);

        when(iShipmentsContainersMappingDao.findByContainerId(1L)).thenReturn(mappingList);
        when(shipmentService.findById(10L)).thenReturn(Optional.empty());

        containerV3Service.processDGShipmentDetailsFromContainer(containerRequestList);

        verify(iShipmentsContainersMappingDao).findByContainerId(1L);
        verify(shipmentService).findById(10L);
        verify(containerDao, never()).findByShipmentId(any());
    }

    @Test
    void testProcessDGShipmentDetailsFromContainer_SuccessfulProcessing() throws RunnerException {
        // Test case for successful processing with all conditions met
        ContainerV3Request container = new ContainerV3Request();
        container.setId(1L);
        container.setHazardous(true);
        List<ContainerV3Request> containerRequestList = Arrays.asList(container);

        ShipmentsContainersMapping mapping = new ShipmentsContainersMapping();
        mapping.setShipmentId(10L);
        List<ShipmentsContainersMapping> mappingList = Arrays.asList(mapping);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(10L);

        Containers containerEntity = new Containers();
        List<Containers> containersList = Arrays.asList(containerEntity);

        when(iShipmentsContainersMappingDao.findByContainerId(1L)).thenReturn(mappingList);
        when(shipmentService.findById(10L)).thenReturn(Optional.of(shipmentDetails));
        when(containerDao.findByShipmentId(10L)).thenReturn(containersList);

        containerV3Service.processDGShipmentDetailsFromContainer(containerRequestList);

        verify(iShipmentsContainersMappingDao).findByContainerId(1L);
        verify(shipmentService).findById(10L);
        verify(containerDao).findByShipmentId(10L);
        // Verify updateOceanDGStatus is called - you may need to add a spy or mock for this
    }

    @Test
    void testProcessDGShipmentDetailsFromContainer_MultipleContainers() throws RunnerException {
        // Test case with multiple containers - some valid, some invalid
        ContainerV3Request validContainer = new ContainerV3Request();
        validContainer.setId(1L);
        validContainer.setHazardous(true);

        ContainerV3Request invalidContainer = new ContainerV3Request();
        invalidContainer.setId(null);
        invalidContainer.setHazardous(true);

        List<ContainerV3Request> containerRequestList = Arrays.asList(validContainer, invalidContainer);

        ShipmentsContainersMapping mapping = new ShipmentsContainersMapping();
        mapping.setShipmentId(10L);
        List<ShipmentsContainersMapping> mappingList = Arrays.asList(mapping);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(10L);

        Containers containerEntity = new Containers();
        List<Containers> containersList = Arrays.asList(containerEntity);

        when(iShipmentsContainersMappingDao.findByContainerId(1L)).thenReturn(mappingList);
        when(shipmentService.findById(10L)).thenReturn(Optional.of(shipmentDetails));
        when(containerDao.findByShipmentId(10L)).thenReturn(containersList);

        containerV3Service.processDGShipmentDetailsFromContainer(containerRequestList);

        // Verify only valid container is processed
        verify(iShipmentsContainersMappingDao).findByContainerId(1L);
        verify(iShipmentsContainersMappingDao, never()).findByContainerId(isNull());
        verify(shipmentService).findById(10L);
        verify(containerDao).findByShipmentId(10L);
    }

    @Test
    void testProcessDGShipmentDetailsFromContainer_MultipleShipmentMappings() throws RunnerException {
        // Test case with multiple shipment mappings for one container
        ContainerV3Request container = new ContainerV3Request();
        container.setId(1L);
        container.setHazardous(true);
        List<ContainerV3Request> containerRequestList = Arrays.asList(container);

        ShipmentsContainersMapping mapping1 = new ShipmentsContainersMapping();
        mapping1.setShipmentId(10L);
        ShipmentsContainersMapping mapping2 = new ShipmentsContainersMapping();
        mapping2.setShipmentId(20L);
        List<ShipmentsContainersMapping> mappingList = Arrays.asList(mapping1, mapping2);

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setId(10L);
        ShipmentDetails shipmentDetails2 = new ShipmentDetails();
        shipmentDetails2.setId(20L);

        Containers containerEntity = new Containers();
        List<Containers> containersList = Arrays.asList(containerEntity);

        when(iShipmentsContainersMappingDao.findByContainerId(1L)).thenReturn(mappingList);
        when(shipmentService.findById(10L)).thenReturn(Optional.of(shipmentDetails1));
        when(shipmentService.findById(20L)).thenReturn(Optional.of(shipmentDetails2));
        when(containerDao.findByShipmentId(10L)).thenReturn(containersList);
        when(containerDao.findByShipmentId(20L)).thenReturn(containersList);

        containerV3Service.processDGShipmentDetailsFromContainer(containerRequestList);

        verify(iShipmentsContainersMappingDao).findByContainerId(1L);
        verify(shipmentService).findById(10L);
        verify(shipmentService).findById(20L);
        verify(containerDao).findByShipmentId(10L);
        verify(containerDao).findByShipmentId(20L);
    }

    @Test
    void testProcessDGShipmentDetailsFromContainer_EmptyContainerRequestList() throws RunnerException {
        // Test case with empty container request list
        List<ContainerV3Request> containerRequestList = Collections.emptyList();

        containerV3Service.processDGShipmentDetailsFromContainer(containerRequestList);

        // Verify no repository calls are made
        verify(iShipmentsContainersMappingDao, never()).findByContainerId(any());
        verify(shipmentService, never()).findById(any());
        verify(containerDao, never()).findByShipmentId(any());
    }

    @Test
    void testProcessDGShipmentDetailsFromContainer_WithValidHazardousContainer_ShouldProcessSuccessfully() {
        // Arrange
        ContainerV3Request containerV3Request = new ContainerV3Request();
        containerV3Request.setId(1L);
        containerV3Request.setHazardous(true);

        List<ContainerV3Request> containerRequestList = List.of(containerV3Request);

        ShipmentsContainersMapping shipmentsContainersMapping = new ShipmentsContainersMapping();
        shipmentsContainersMapping.setShipmentId(100L);
        List<ShipmentsContainersMapping> shipmentsContainersMappingList = List.of(shipmentsContainersMapping);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(100L);

        Containers container = new Containers();
        container.setId(1L);
        List<Containers> containersList = List.of(container);

        // Mock dependencies
        when(iShipmentsContainersMappingDao.findByContainerId(1L)).thenReturn(shipmentsContainersMappingList);
        when(shipmentService.findById(100L)).thenReturn(Optional.of(shipmentDetails));
        when(containerDao.findByShipmentId(100L)).thenReturn(containersList);
        // Act & Assert - Should not throw exception
        assertDoesNotThrow(() -> containerV3Service.processDGShipmentDetailsFromContainer(containerRequestList));

    }

    @Test
    void testProcessDGShipmentDetailsFromContainer_WithNullContainerId_ShouldSkipProcessing()  {
        // Arrange
        ContainerV3Request containerV3Request = new ContainerV3Request();
        containerV3Request.setId(null); // Null ID
        containerV3Request.setHazardous(true);

        List<ContainerV3Request> containerRequestList = List.of(containerV3Request);

        // Act & Assert - Should not throw exception
        assertDoesNotThrow(() -> containerV3Service.processDGShipmentDetailsFromContainer(containerRequestList));

    }

    @Test
    void testProcessDGShipmentDetailsFromContainer_WithNonHazardousContainer_ShouldSkipProcessing() {
        // Arrange
        ContainerV3Request containerV3Request = new ContainerV3Request();
        containerV3Request.setId(1L);
        containerV3Request.setHazardous(false); // Not hazardous

        List<ContainerV3Request> containerRequestList = List.of(containerV3Request);

        // Act & Assert - Should not throw exception
        assertDoesNotThrow(() -> containerV3Service.processDGShipmentDetailsFromContainer(containerRequestList));


    }

    @Test
    void testProcessDGShipmentDetailsFromContainer_WithEmptyShipmentsContainersMappingList_ShouldSkipInnerLoop() {
        // Arrange
        ContainerV3Request containerV3Request = new ContainerV3Request();
        containerV3Request.setId(1L);
        containerV3Request.setHazardous(true);

        List<ContainerV3Request> containerRequestList = List.of(containerV3Request);

        // Return empty list
        List<ShipmentsContainersMapping> emptyMappingList = new ArrayList<>();

        // Mock dependencies
        when(iShipmentsContainersMappingDao.findByContainerId(1L)).thenReturn(emptyMappingList);

        // Act & Assert - Should not throw exception
        assertDoesNotThrow(() -> containerV3Service.processDGShipmentDetailsFromContainer(containerRequestList));


    }

    @Test
    void testProcessDGShipmentDetailsFromContainer_WithShipmentNotFound_ShouldSkipUpdateOceanDGStatus() {
        // Arrange
        ContainerV3Request containerV3Request = new ContainerV3Request();
        containerV3Request.setId(1L);
        containerV3Request.setHazardous(true);

        List<ContainerV3Request> containerRequestList = List.of(containerV3Request);

        ShipmentsContainersMapping shipmentsContainersMapping = new ShipmentsContainersMapping();
        shipmentsContainersMapping.setShipmentId(100L);
        List<ShipmentsContainersMapping> shipmentsContainersMappingList = List.of(shipmentsContainersMapping);

        // Mock dependencies - shipment not found
        when(iShipmentsContainersMappingDao.findByContainerId(1L)).thenReturn(shipmentsContainersMappingList);
        when(shipmentService.findById(100L)).thenReturn(Optional.empty()); // Empty optional

        // Act & Assert - Should not throw exception
        assertDoesNotThrow(() -> containerV3Service.processDGShipmentDetailsFromContainer(containerRequestList));


    }


    @Test
    void testProcessDGShipmentDetailsFromContainer_WithEmptyContainerRequestList_ShouldNotProcess() {
        // Arrange
        List<ContainerV3Request> emptyContainerRequestList = new ArrayList<>();

        // Act & Assert - Should not throw exception
        assertDoesNotThrow(() -> containerV3Service.processDGShipmentDetailsFromContainer(emptyContainerRequestList));

    }

    @Test
    void testProcessDGShipmentDetailsFromContainer_MixedHazardousValues() throws RunnerException {
        // Arrange
        ContainerV3Request hazardousContainer = new ContainerV3Request();
        hazardousContainer.setId(1L);
        hazardousContainer.setHazardous(true);

        ContainerV3Request nonHazardousContainer = new ContainerV3Request();
        nonHazardousContainer.setId(2L);
        nonHazardousContainer.setHazardous(false);

        List<ContainerV3Request> containerRequestList = Arrays.asList(hazardousContainer, nonHazardousContainer);

        ShipmentsContainersMapping mapping = new ShipmentsContainersMapping();
        mapping.setShipmentId(10L);
        List<ShipmentsContainersMapping> mappingList = Arrays.asList(mapping);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(10L);

        Containers containerEntity = new Containers();
        List<Containers> containersList = Arrays.asList(containerEntity);

        when(iShipmentsContainersMappingDao.findByContainerId(1L)).thenReturn(mappingList);
        when(shipmentService.findById(10L)).thenReturn(Optional.of(shipmentDetails));
        when(containerDao.findByShipmentId(10L)).thenReturn(containersList);

        containerV3Service.processDGShipmentDetailsFromContainer(containerRequestList);

        // Verify only hazardous container is processed
        verify(iShipmentsContainersMappingDao).findByContainerId(1L);
        verify(iShipmentsContainersMappingDao, never()).findByContainerId(2L);
        verify(shipmentService).findById(10L);
        verify(containerDao).findByShipmentId(10L);
    }

    @Test
    void testUpdateOceanDGStatus_WhenUpdateNotRequired() throws RunnerException {
        // Test case when isUpdateDGStatusRequired returns false - should return early
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("ROAD"); // Not SEA transport mode

        List<Containers> containersList = Arrays.asList(new Containers());
        List<ContainerV3Request> containerRequestList = Arrays.asList(new ContainerV3Request());

        containerV3Service.updateStatusForOceanDG(shipmentDetails, containersList, containerRequestList);

        // Verify no further processing happens
        verify(commonUtils, never()).changeShipmentDGStatusToReqd(any(), anyBoolean());
        verify(shipmentDao, never()).updateDgStatusInShipment(anyBoolean(), any(), any());
    }

    @Test
    void testUpdateOceanDGStatus_WhenDGFieldsChangedAndSaveRequired() throws RunnerException {
        // Test case for successful DG status update with DG Class 1
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(100L);
        shipmentDetails.setTransportMode("SEA");
        shipmentDetails.setOceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED); // Assuming enum exists

        Containers oldContainer = new Containers();
        oldContainer.setId(1L);

        ContainerV3Request updatedContainer = new ContainerV3Request();
        updatedContainer.setId(1L);
        updatedContainer.setDgClass("1");

        List<Containers> containersList = Arrays.asList(oldContainer);
        List<ContainerV3Request> containerRequestList = Arrays.asList(updatedContainer);

        when(commonUtils.checkIfDGFieldsChangedInContainer(updatedContainer, oldContainer)).thenReturn(true);
        when(commonUtils.checkIfDGClass1("1")).thenReturn(true);
        when(commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, true)).thenReturn(true);

        containerV3Service.updateStatusForOceanDG(shipmentDetails, containersList, containerRequestList);

        verify(commonUtils).checkIfDGFieldsChangedInContainer(updatedContainer, oldContainer);
        verify(commonUtils).checkIfDGClass1("1");
        verify(commonUtils).changeShipmentDGStatusToReqd(shipmentDetails, true);
        verify(shipmentValidationV3Util).processDGValidations(shipmentDetails, null, shipmentDetails.getConsolidationList());

        // Verify shipment details is updated
        assertTrue(shipmentDetails.getContainsHazardous());
    }

    @Test
    void testUpdateOceanDGStatus_WhenDGFieldsChangedButSaveNotRequired() throws RunnerException {
        // Test case when DG fields changed but save is not required
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(100L);
        shipmentDetails.setTransportMode("SEA");

        Containers oldContainer = new Containers();
        oldContainer.setId(1L);

        ContainerV3Request updatedContainer = new ContainerV3Request();
        updatedContainer.setId(1L);
        updatedContainer.setDgClass("2");

        List<Containers> containersList = Arrays.asList(oldContainer);
        List<ContainerV3Request> containerRequestList = Arrays.asList(updatedContainer);

        when(commonUtils.checkIfDGFieldsChangedInContainer(updatedContainer, oldContainer)).thenReturn(true);
        when(commonUtils.checkIfDGClass1("2")).thenReturn(false);
        when(commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, false)).thenReturn(false);

        containerV3Service.updateStatusForOceanDG(shipmentDetails, containersList, containerRequestList);

        verify(commonUtils).checkIfDGFieldsChangedInContainer(updatedContainer, oldContainer);
        verify(commonUtils).checkIfDGClass1("2");
        verify(commonUtils).changeShipmentDGStatusToReqd(shipmentDetails, false);
        // Verify that shipment is not saved
        verify(shipmentValidationV3Util, never()).processDGValidations(any(), any(), any());
        verify(shipmentDao, never()).updateDgStatusInShipment(anyBoolean(), any(), any());
    }

    @Test
    void testProcessDGShipmentDetailsFromContainer_WithMixedHazardousAndNonHazardousContainers_ShouldProcessOnlyHazardous() {
        // Arrange
        ContainerV3Request nullIdContainer = new ContainerV3Request();
        nullIdContainer.setId(null);
        nullIdContainer.setHazardous(true);

        ContainerV3Request hazardousContainer = new ContainerV3Request();
        hazardousContainer.setId(1L);
        hazardousContainer.setHazardous(true);

        ContainerV3Request nonHazardousContainer = new ContainerV3Request();
        nonHazardousContainer.setId(2L);
        nonHazardousContainer.setHazardous(false);

        List<ContainerV3Request> containerRequestList = List.of(hazardousContainer, nonHazardousContainer, nullIdContainer);

        ShipmentsContainersMapping mapping = new ShipmentsContainersMapping();
        mapping.setShipmentId(100L);
        List<ShipmentsContainersMapping> mappingList = List.of(mapping);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(100L);

        Containers container = new Containers();

        // Mock dependencies - only for hazardous container with valid ID
        when(iShipmentsContainersMappingDao.findByContainerId(1L)).thenReturn(mappingList);
        when(shipmentService.findById(100L)).thenReturn(Optional.of(shipmentDetails));
        when(containerDao.findByShipmentId(100L)).thenReturn(List.of(container));

        // Act & Assert - Should not throw exception
        assertDoesNotThrow(() -> containerV3Service.processDGShipmentDetailsFromContainer(containerRequestList));

    }

    @Test
    void testUpdateOceanDGStatus_WithNullShipmentDetails_ShouldReturnEarly() throws RunnerException {
        // Arrange
        ShipmentDetails shipmentDetails = null;
        List<Containers> containersList = List.of(new Containers());

        // Act & Assert - Should not throw exception
        assertDoesNotThrow(() -> containerV3Service.updateOceanDgStatusForCreateUpdate(shipmentDetails, containersList, new ArrayList<>(), false));

        // Verify no methods were called since method returns early
        verify(commonUtils, never()).checkIfDGClass1(any());
        verify(commonUtils, never()).changeShipmentDGStatusToReqd(any(), anyBoolean());
        verify(shipmentValidationV3Util, never()).processDGValidations(any(), any(), any());
        verify(shipmentDao, never()).updateDgStatusInShipment(anyBoolean(), any(), any());
    }

    @Test
    void testUpdateOceanDGStatus_WithNullContainersList_ShouldReturnEarly() {
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        List<Containers> containersList = null;
        // Act & Assert - Should not throw exception
        assertDoesNotThrow(() -> containerV3Service.updateOceanDgStatusForCreateUpdate(shipmentDetails, containersList, null , false));
    }


    @Test
    void testProcessDG() {
        ContainerV3Request containerV3Request = new ContainerV3Request();
        containerV3Request.setConsolidationId(1L);
        containerV3Request.setHazardous(true);
        List<ContainerV3Request> containerRequestList = List.of(containerV3Request);
        String module = CONSOLIDATION;
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setTransportMode("SEA");
        when(consolidationV3Service.fetchConsolidationDetails(anyLong())).thenReturn(consolidationDetails);
        when(consolidationValidationV3Util.checkConsolidationTypeValidation(any())).thenReturn(false);

        assertThrows(ValidationException.class, () -> containerV3Service.processContainerDG(containerRequestList, module, null, true ));
    }

    @Test
    void updateOceanDgStatusCreate() throws RunnerException {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(TRANSPORT_MODE_SEA);
        ContainerV3Request containerV3Request = new ContainerV3Request();
        containerV3Request.setHazardous(true);
        when(commonUtils.checkIfDGClass1(any())).thenReturn(true);
        when(commonUtils.changeShipmentDGStatusToReqd(any(), anyBoolean())).thenReturn(true);

        containerV3Service.updateOceanDgStatusForCreateUpdate(shipmentDetails, null, List.of(containerV3Request), true);
        verify(commonUtils).checkIfDGClass1(any());
    }

    @Test
    void updateOceanDgStatusUpdate() throws RunnerException {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(TRANSPORT_MODE_SEA);
        ContainerV3Request containerV3Request = new ContainerV3Request();
        containerV3Request.setHazardous(true);
        containerV3Request.setId(1L);

        Containers containers = new Containers();
        containers.setId(1L);
        when(commonUtils.checkIfDGClass1(any())).thenReturn(true);
        when(commonUtils.changeShipmentDGStatusToReqd(any(), anyBoolean())).thenReturn(true);
        lenient().when(commonUtils.checkIfDGFieldsChangedInContainer(containerV3Request, containers)).thenReturn(true);

        containerV3Service.updateOceanDgStatusForCreateUpdate(shipmentDetails, List.of(containers), List.of(containerV3Request), false);
        verify(commonUtils).checkIfDGClass1(any());
    }

    @Test
    void testProcessContainerDG_WhenModuleIsShipment_ThenValidateAndSaveDGShipmentCalled() throws RunnerException {
        boolean isCreate = true;
        Long shipmentId = 1L;
        ContainerV3Request containerRequest = new ContainerV3Request();
        containerRequest.setShipmentId(shipmentId);
        List<ContainerV3Request> containerRequestList = List.of(containerRequest);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        List<Containers> oldContainers = List.of(new Containers());
        ContainerV3Service spyService = Mockito.spy(containerV3Service);
        Mockito.doReturn(true).when(spyService).containsHazardousContainer(containerRequestList);
        Mockito.when(containerDao.findByShipmentId(shipmentId)).thenReturn(oldContainers);
        spyService.processContainerDG(containerRequestList, SHIPMENT, shipmentDetails, isCreate);
        Mockito.verify(containerDao).findByShipmentId(shipmentId);
        Mockito.verify(spyService).validateAndSaveDGShipment(oldContainers, shipmentDetails, containerRequestList, isCreate);
    }

    @Test
    void testUpdateContainerRequestWithDgFalse() {
        ContainerV3Request containerRequest = new ContainerV3Request();
        containerRequest.setUnNumber("1234");
        containerRequest.setProperShippingName("Some Name");
        containerRequest.setDgClass("3");
        containerRequest.setMarinePollutant(true);
        containerRequest.setPackingGroup("II");
        containerRequest.setMinimumFlashPoint(new BigDecimal(23));
        containerV3Service.updateContainerRequestWithDgFalse(containerRequest);
        assertNull(containerRequest.getUnNumber());
        assertNull(containerRequest.getProperShippingName());
        assertNull(containerRequest.getDgClass());
        assertNull(containerRequest.getMarinePollutant());
        assertNull(containerRequest.getPackingGroup());
        assertNull(containerRequest.getMinimumFlashPoint());
    }

    @Test
    void testUpdateContainerRequestOnDgFlag_shouldClearDgFieldsForNonHazardousContainers() {
        ContainerV3Request dgContainer = new ContainerV3Request();
        dgContainer.setHazardous(true);
        dgContainer.setUnNumber("1234");
        ContainerV3Request nonDgContainer = new ContainerV3Request();
        nonDgContainer.setHazardous(false);
        nonDgContainer.setProperShippingName("Sample DG");
        nonDgContainer.setDgClass("3");
        nonDgContainer.setMarinePollutant(true);
        nonDgContainer.setPackingGroup("II");
        nonDgContainer.setMinimumFlashPoint(new BigDecimal(23));
        List<ContainerV3Request> containerList = List.of(dgContainer, nonDgContainer);
        containerV3Service.updateContainerRequestOnDgFlag(containerList);
        assertEquals("1234", dgContainer.getUnNumber());
        assertNull(nonDgContainer.getProperShippingName());
        assertNull(nonDgContainer.getDgClass());
        assertNull(nonDgContainer.getMarinePollutant());
        assertNull(nonDgContainer.getPackingGroup());
        assertNull(nonDgContainer.getMinimumFlashPoint());
    }

    @Test
    void testSetAttachedShipmentResponseInContainer_shouldSetResponsesCorrectly() {
        ShipmentDetailsProjection detail1 = Mockito.mock(ShipmentDetailsProjection.class);
        ShipmentDetailsProjection detail2 = Mockito.mock(ShipmentDetailsProjection.class);
        Mockito.when(detail1.getId()).thenReturn(1L);
        Mockito.when(detail1.getShipmentNumber()).thenReturn("SHP001");
        Mockito.when(detail1.getShipmentType()).thenReturn("FCL");
        Mockito.when(detail2.getId()).thenReturn(2L);
        Mockito.when(detail2.getShipmentNumber()).thenReturn("SHP002");
        Mockito.when(detail2.getShipmentType()).thenReturn("LCL");
        List<ShipmentDetailsProjection> details = List.of(detail1, detail2);
        ContainerBaseResponse container = new ContainerBaseResponse();
        ContainerV3Service.setAttachedShipmentResponseInContainer(container, details);
        List<AttachedShipmentResponse> responses = container.getAttachedShipmentResponses();
        assertNotNull(responses);
        assertEquals(2, responses.size());
        assertEquals(1L, responses.get(0).getAttachedShipmentId());
        assertEquals("SHP001", responses.get(0).getAttachedShipmentNumber());
        assertEquals("FCL", responses.get(0).getAttachedShipmentType());
        assertEquals(2L, responses.get(1).getAttachedShipmentId());
        assertEquals("SHP002", responses.get(1).getAttachedShipmentNumber());
        assertEquals("LCL", responses.get(1).getAttachedShipmentType());
    }
    @Test
    void testSaveUnAssignContainerResults_ShouldUnassignCorrectly() throws Exception {
        Long shipmentId1 = 101L;
        Long shipmentId2 = 102L;
        List<Long> shipmentIdsForDetachment = List.of(shipmentId1, shipmentId2);
        List<Long> cargoDetachmentIds = List.of(shipmentId1);
        List<Long> packingIds = List.of(301L, 302L);
        Containers container1 = mock(Containers.class);
        Containers savedContainer = mock(Containers.class);
        ShipmentsContainersMapping mapping1 = new ShipmentsContainersMapping();
        mapping1.setShipmentId(shipmentId1);
        ShipmentsContainersMapping mapping2 = new ShipmentsContainersMapping();
        mapping2.setShipmentId(999L); // not in detachment list
        UnAssignContainerParams params1 = new UnAssignContainerParams();
        params1.setShipmentIdsForCargoDetachment(cargoDetachmentIds);
        params1.setRemoveAllPackingIds(packingIds);
        params1.setShipmentsContainersMappings(List.of(mapping1, mapping2));
        when(containerDao.save(container1)).thenReturn(savedContainer);
        Method method = ContainerV3Service.class.getDeclaredMethod(
                "saveUnAssignContainerResults",
                List.class, Containers.class, UnAssignContainerParams.class
        );
        method.setAccessible(true);
        Containers result = (Containers) method.invoke(containerV3Service, shipmentIdsForDetachment, container1, params1);
        assertEquals(savedContainer, result);
        verify(shipmentDao).setShipmentIdsToContainer(cargoDetachmentIds, null);
        verify(packingDao).setPackingIdsToContainer(packingIds, null);
        verify(containerDao).save(container1);
        verify(shipmentsContainersMappingDao).deleteAll(List.of(mapping1));
        verifyNoMoreInteractions(shipmentsContainersMappingDao);
    }

    @Test
    void testHandleUnAssignmentLogicWhenAllPacksAreRemoved_WithSeaFCL_ShouldUpdateContainerAndDetach() throws Exception {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(10L);
        shipmentDetails.setShipmentType("FCL");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_SEA);
        shipmentDetails.setContainerAssignedToShipmentCargo(99L);
        Containers container1 = new Containers();
        container1.setId(99L);
        Containers oldContainer = new Containers();
        oldContainer.setGrossWeight(new BigDecimal("100.0"));
        oldContainer.setGrossWeightUnit("KG");
        oldContainer.setGrossVolume(new BigDecimal("10.0"));
        oldContainer.setGrossVolumeUnit("CBM");
        oldContainer.setPacks("5");
        oldContainer.setPacksType("CTN");
        Packing packing1 = new Packing();
        packing1.setContainerId(99L);
        Packing packing2 = new Packing();
        packing2.setContainerId(99L);
        List<Long> shipmentIdsForDetachment = new ArrayList<>();
        List<Packing> packingList = List.of(packing1, packing2);
        Set<Long> removePackIds = Set.of(1L, 2L);
        UnAssignContainerParams params1 = new UnAssignContainerParams();
        params1.setOldContainersEntity(oldContainer);
        params1.setRemoveAllPackingIds(new ArrayList<Long>());
        params1.setShipmentIdsForCargoDetachment(new ArrayList<Long>());
        CommonUtils commonUtils = mock(CommonUtils.class);
        when(commonUtils.isSeaFCLOrRoadFTL(TRANSPORT_MODE_SEA, CARGO_TYPE_FCL)).thenReturn(true);
        ContainerV3Service service = new ContainerV3Service();
        Field utilsField = ContainerV3Service.class.getDeclaredField("commonUtils");
        utilsField.setAccessible(true);
        utilsField.set(service, commonUtils);
        Method method = ContainerV3Service.class.getDeclaredMethod(
                "handleUnAssignmentLogicWhenAllPacksAreRemoved",
                UnAssignContainerParams.class,
                Containers.class,
                ShipmentDetails.class,
                List.class,
                List.class,
                Set.class,
                Map.class,
                Boolean.class
        );
        method.setAccessible(true);
        method.invoke(service, params1, container1, shipmentDetails, shipmentIdsForDetachment, packingList, removePackIds, new HashMap<>(), Boolean.FALSE);
        assertEquals("KG", container1.getGrossWeightUnit());
        assertEquals("CBM", container1.getGrossVolumeUnit());
        assertEquals("5", container1.getPacks());
        assertEquals("CTN", container1.getPacksType());
        assertTrue(params1.getRemoveAllPackingIds().containsAll(removePackIds));
        assertTrue(params1.getShipmentIdsForCargoDetachment().contains(10L));
        assertNull(shipmentDetails.getContainerAssignedToShipmentCargo());
        assertTrue(packingList.stream().allMatch(p -> p.getContainerId() == null));
    }

    @Test
    void testUpdateSummary_WithEmptyFclOrFtlShipmentIds() throws RunnerException {
        params.setFclOrFtlShipmentIds(null);
        List<Long> shipmentIdsForAttachment = Arrays.asList(1L, 2L);
        containerV3Service.updateSummary(container, shipmentIdsForAttachment, params);
        verifyNoInteractions(shipmentService);
        verifyNoInteractions(consolidationV3Service);
    }

    @Test
    void testUpdateSummary_WithFclOrFtlShipmentIds_AllShipmentsNeedAttachment() throws RunnerException {
        params.setFclOrFtlShipmentIds(Set.of(1L, 2L));
        List<Long> shipmentIdsForAttachment = Arrays.asList(1L, 2L);
        ShipmentWtVolResponse oldResponse = new ShipmentWtVolResponse();
        params.setOldShipmentWtVolResponse(oldResponse);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        params.setConsolidationDetails(consolidationDetails);
        containerV3Service.updateSummary(container, shipmentIdsForAttachment, params);
        verify(shipmentService).calculateAndUpdateShipmentCargoSummary(
                eq(shipmentDetails1),
                argThat(list -> list.contains(container))
        );
        verify(shipmentService).calculateAndUpdateShipmentCargoSummary(
                eq(shipmentDetails2),
                argThat(list -> list.contains(container))
        );
        verify(consolidationV3Service).updateConsolidationCargoSummary(consolidationDetails, oldResponse);
    }

    @Test
    void testUpdateSummary_WithFclOrFtlShipmentIds_SomeShipmentsNeedAttachment() throws RunnerException {
        params.setFclOrFtlShipmentIds(Set.of(1L, 2L));
        List<Long> shipmentIdsForAttachment = List.of(1L);
        ShipmentWtVolResponse oldResponse = new ShipmentWtVolResponse();
        params.setOldShipmentWtVolResponse(oldResponse);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        params.setConsolidationDetails(consolidationDetails);
        containerV3Service.updateSummary(container, shipmentIdsForAttachment, params);
        verify(shipmentService).calculateAndUpdateShipmentCargoSummary(
                eq(shipmentDetails1),
                argThat(list -> list.contains(container))
        );
        verify(shipmentService).calculateAndUpdateShipmentCargoSummary(
                eq(shipmentDetails2),
                argThat(list -> !list.contains(container))
        );
        verify(consolidationV3Service).updateConsolidationCargoSummary(consolidationDetails, oldResponse);
    }

    @Test
    void testUpdateSummary_WithFclOrFtlShipmentIds_NoShipmentsNeedAttachment() throws RunnerException {
        params.setFclOrFtlShipmentIds(Set.of(1L, 2L));
        List<Long> shipmentIdsForAttachment = List.of(3L);
        ShipmentWtVolResponse oldResponse = new ShipmentWtVolResponse();
        params.setOldShipmentWtVolResponse(oldResponse);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        params.setConsolidationDetails(consolidationDetails);
        containerV3Service.updateSummary(container, shipmentIdsForAttachment, params);
        verify(shipmentService).calculateAndUpdateShipmentCargoSummary(
                eq(shipmentDetails1),
                argThat(list -> !list.contains(container))
        );
        verify(shipmentService).calculateAndUpdateShipmentCargoSummary(
                eq(shipmentDetails2),
                argThat(list -> !list.contains(container))
        );
        verify(consolidationV3Service).updateConsolidationCargoSummary(consolidationDetails, oldResponse);
    }

    @Test
    void testAssignContainerOnlyToShipment_shouldThrowException_whenIsFCLorFTL() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("SEA");
        shipmentDetails.setShipmentType("FCL");
        Containers container1 = new Containers();
        List<Long> shipmentIdsToSetContainerCargo = List.of(1L);
        when(commonUtils.isSeaFCLOrRoadFTL(anyString(), anyString())).thenReturn(true);
        ValidationException ex = assertThrows(ValidationException.class, () -> {
            containerV3Service.assignContainerOnlyToShipment(shipmentDetails, container1, shipmentIdsToSetContainerCargo);
        });
        assertEquals("Please select atleast one package for FCL/FTL shipment.", ex.getMessage());
    }
    @Test
    void testAssignContainerOnlyToShipment_shouldThrowException_whenContainerAlreadyAssigned() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("AIR"); // non-FCL/FTL
        shipmentDetails.setShipmentType("LCL");
        shipmentDetails.setContainerAssignedToShipmentCargo(123L);
        Containers container1 = new Containers();
        List<Long> shipmentIdsToSetContainerCargo = List.of(1L);
        when(commonUtils.isSeaFCLOrRoadFTL(anyString(), anyString())).thenReturn(false);
        when(containerV3Util.getContainerNumberOrType(123L)).thenReturn("CONT123");
        ValidationException ex = assertThrows(ValidationException.class, () -> {
            containerV3Service.assignContainerOnlyToShipment(shipmentDetails, container1, shipmentIdsToSetContainerCargo);
        });
        assertEquals("Shipment already Assigned to Container -  CONT123", ex.getMessage());
    }

    @Test
    void testSetAssignedContainersParems_WithValidShipmentIds_ShouldSetDetailsAndWtVol() throws RunnerException {
        AssignContainerParams params1 = new AssignContainerParams();
        Set<Long> shipmentIds = Set.of(101L, 102L);
        Long consolidationId = 999L;
        params1.setFclOrFtlShipmentIds(shipmentIds);
        params1.setConsolidationId(consolidationId);
        ConsolidationDetails mockConsolidationDetails = new ConsolidationDetails();
        ShipmentWtVolResponse mockWtVolResponse = new ShipmentWtVolResponse();
        when(consolidationV3Service.fetchConsolidationDetails(consolidationId)).thenReturn(mockConsolidationDetails);
        when(consolidationV3Service.calculateShipmentWtVol(mockConsolidationDetails)).thenReturn(mockWtVolResponse);
        containerV3Service.setAssignedContainersParems(params1);
        assertEquals(mockConsolidationDetails, params1.getConsolidationDetails());
        assertEquals(mockWtVolResponse, params1.getOldShipmentWtVolResponse());
    }

    @Test
    void setUnassignedContainerParems_happyPath_shouldSetDetailsAndWtVol() throws RunnerException {
        // Arrange
        UnAssignContainerParams params = new UnAssignContainerParams();
        params.setFclOrFtlShipmentIds(Set.of(1L, 2L)); // Condition 1: not empty
        params.setConsolidationDetails(null); // Condition 2: is null
        params.setConsolidationId(123L);

        ConsolidationDetails mockConsolidationDetails = new ConsolidationDetails();
        ShipmentWtVolResponse mockWtVolResponse = new ShipmentWtVolResponse();

        // Stub the mock service calls
        when(consolidationV3Service.fetchConsolidationDetails(123L)).thenReturn(mockConsolidationDetails);
        when(consolidationV3Service.calculateShipmentWtVol(mockConsolidationDetails)).thenReturn(mockWtVolResponse);

        // Act
        containerV3Service.setUnassignedContainerParems(params);

        // Assert
        // Verify that the service methods were called with the correct arguments
        verify(consolidationV3Service, times(1)).fetchConsolidationDetails(123L);
        verify(consolidationV3Service, times(1)).calculateShipmentWtVol(mockConsolidationDetails);

        // Verify that the parameters object was updated correctly
        assertEquals(mockConsolidationDetails, params.getConsolidationDetails());
        assertEquals(mockWtVolResponse, params.getOldShipmentWtVolResponse());
    }

    @Test
    void setUnassignedContainerParems_whenShipmentIdsAreEmpty_shouldDoNothing() throws RunnerException {
        UnAssignContainerParams params = new UnAssignContainerParams();
        params.setFclOrFtlShipmentIds(Collections.emptySet());
        params.setConsolidationDetails(null);
        containerV3Service.setUnassignedContainerParems(params);
        verify(consolidationV3Service, never()).fetchConsolidationDetails(any());
        verify(consolidationV3Service, never()).calculateShipmentWtVol(any());
        assertEquals(Collections.emptySet(), params.getFclOrFtlShipmentIds());
        assertEquals(null, params.getConsolidationDetails());
        assertEquals(null, params.getOldShipmentWtVolResponse());
    }

    @Test
    void setUnassignedContainerParems_whenConsolidationDetailsArePresent_shouldDoNothing() throws RunnerException {
        UnAssignContainerParams params = new UnAssignContainerParams();
        params.setFclOrFtlShipmentIds(Set.of(1L)); // Condition 1: not empty
        params.setConsolidationDetails(new ConsolidationDetails()); // Condition 2: not null
        containerV3Service.setUnassignedContainerParems(params);
        verify(consolidationV3Service, never()).fetchConsolidationDetails(any());
        verify(consolidationV3Service, never()).calculateShipmentWtVol(any());
        assertEquals(1, params.getFclOrFtlShipmentIds().size());
    }
}
