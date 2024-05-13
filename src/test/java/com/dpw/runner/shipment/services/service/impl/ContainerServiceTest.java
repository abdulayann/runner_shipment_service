package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.Kafka.Dto.KafkaResponse;
import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.BulkContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.impl.SyncEntityConversionService;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class ContainerServiceTest {

    @Mock
    private IContainerDao containerDao;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private SyncEntityConversionService syncEntityConversionService;

    @Mock
    private KafkaProducer producer;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IV1Service v1Service;

    @Mock
    private IPackingDao packingDao;

    @Mock
    private IEventDao eventDao;

    @Mock
    private IAuditLogService auditLogService;

    @Mock
    private IPackingsSync packingsADSync;

    @Mock
    IShipmentsContainersMappingDao shipmentsContainersMappingDao;
    private static JsonTestUtility jsonTestUtility;
    private static Containers testContainer;
    private static Packing testPacking;
    private static ShipmentDetails testShipment;
    private static ObjectMapper objectMapper;

    @InjectMocks
    private ContainerService containerService;

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
    public void setUp() {
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
    public void testCreate_Success() {
        ResponseEntity<IRunnerResponse> responseEntity = containerService.create(CommonRequestModel.buildRequest());

        Assertions.assertNull(responseEntity);
    }



    @Test
    public void testAttachPacks_Success() {
        long containerId = 1L;
        List<Long> packsId = new ArrayList<>();
        packsId.add(100L);
        packsId.add(101L);

        Containers containers = testContainer;
        Packing packing1 = new Packing();
        Packing packing2 = new Packing();
        containers.setPacksList(new ArrayList<>());
        containers.getPacksList().add(packing1);
        containers.getPacksList().add(packing2);

        when(containerDao.findById(containerId)).thenReturn(Optional.of(containers));
        when(packingDao.findById(100L)).thenReturn(Optional.of(new Packing()));
        when(packingDao.findById(101L)).thenReturn(Optional.of(new Packing()));
        when(containerDao.save(containers)).thenReturn(containers);


        ResponseEntity<IRunnerResponse> responseEntity = containerService.attachPacks(containerId, packsId);

        Assertions.assertNotNull(responseEntity);
        assertEquals(ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(containers, ContainerResponse.class)), responseEntity);
    }

    @Test
    @Disabled
    public void testAttachPacks_ContainerNotFound() {
        long containerId = 1L;
        List<Long> packsId = new ArrayList<>();
        packsId.add(100L);
        packsId.add(101L);

        when(containerDao.findById(containerId)).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> responseEntity = containerService.attachPacks(containerId, packsId);

        Assertions.assertNull(responseEntity);
    }

    @Test
    public void testAttachPacks_PackingNotFound() {
        long containerId = 1L;
        List<Long> packsId = new ArrayList<>();
        packsId.add(100L);
        packsId.add(101L);

        Containers containers = testContainer;
        when(containerDao.findById(containerId)).thenReturn(Optional.of(containers));
        when(packingDao.findById(100L)).thenReturn(Optional.empty());
        when(packingDao.findById(101L)).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> responseEntity = containerService.attachPacks(containerId, packsId);

        Assertions.assertNotNull(responseEntity);
    }

    @Test
    void testUpdate() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ResponseEntity<IRunnerResponse> responseEntity = containerService.update(CommonRequestModel.buildRequest());

        Assertions.assertNull(responseEntity);
    }

    @Test
    void testList() {
        testContainer.setId(1L);
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);
        IRunnerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerResponse.class);
        Page<Containers> page = new PageImpl<>(List.of(testContainer) , PageRequest.of(0 , 10) , 1);

        when(containerDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(Containers.class), eq(ContainerResponse.class))).thenReturn((ContainerResponse) containerResponse);

        ResponseEntity<IRunnerResponse> responseEntity = containerService.list(commonRequestModel);

        assertNotNull(responseEntity);
        assertEquals(ResponseHelper.buildListSuccessResponse(List.of(containerResponse), page.getTotalPages(), page.getTotalElements()), responseEntity);
    }

    @Test
    void testListAsync() throws ExecutionException, InterruptedException {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().build();
        CommonRequestModel request = CommonRequestModel.buildRequest(listCommonRequest);
        IRunnerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerResponse.class);
        Page<Containers> page = new PageImpl<>(List.of(testContainer) , PageRequest.of(0 , 10) , 1);

        when(containerDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(Containers.class), eq(ContainerResponse.class))).thenReturn((ContainerResponse) containerResponse);

        CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntity = containerService.listAsync(request);

        assertNotNull(responseEntity);
        assertEquals(ResponseHelper.buildListSuccessResponse(List.of(containerResponse), page.getTotalPages(), page.getTotalElements()),
                responseEntity.get());

    }

    @Test
    void testDelete() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Long id = 1L;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(id);

        when(containerDao.findById(id)).thenReturn(Optional.of(testContainer));

        ResponseEntity<IRunnerResponse> responseEntity = containerService.delete(commonRequestModel);

        verify(packingDao, times(1)).deleteEntityFromContainer(id);
        verify(containerDao, times(1)).delete(testContainer);
        verify(auditLogService, times(1)).addAuditLog(any(AuditLogMetaData.class));
        Assertions.assertEquals(responseEntity.getStatusCodeValue() , HttpStatus.OK.value());
    }

    @Test
    void testRetrieveById() {
        testContainer.setId(1L);
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);
        ContainerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerResponse.class);

        when(containerDao.findById(anyLong())).thenReturn(Optional.of(testContainer));
        when(jsonHelper.convertValue(any(Containers.class), eq(ContainerResponse.class))).thenReturn(containerResponse);

        ResponseEntity<IRunnerResponse> responseEntity = containerService.retrieveById(commonRequestModel);

        assertNotNull(responseEntity);
        assertEquals(responseEntity, ResponseHelper.buildSuccessResponse(containerResponse));
    }

    @Test
    public void testCalculateUtilization_WithNonNullContainer() {
        Containers container = new Containers();
        container.setAchievedVolume(BigDecimal.valueOf(50));
        container.setAllocatedVolume(BigDecimal.valueOf(100));
        container.setAchievedWeight(BigDecimal.valueOf(500));
        container.setAllocatedWeight(BigDecimal.valueOf(1000));

        Containers resultContainer = containerService.calculateUtilization(container);

        Assertions.assertEquals("50.0", resultContainer.getVolumeUtilization());
        Assertions.assertEquals("50.0", resultContainer.getWeightUtilization());
    }

    @Test
    public void testCalculateUtilization_WithNullContainer() {
        Containers resultContainer = containerService.calculateUtilization(null);
        Assertions.assertNull(resultContainer);
    }

    @Test
    public void testCalculateUtilization_WithZeroAllocatedWeight() {
        Containers container = new Containers();
        container.setAchievedVolume(BigDecimal.valueOf(50));
        container.setAllocatedVolume(BigDecimal.valueOf(100));
        container.setAchievedWeight(BigDecimal.valueOf(500));
        container.setAllocatedWeight(BigDecimal.ZERO);

        Containers resultContainer = containerService.calculateUtilization(container);

        Assertions.assertEquals("50.0", resultContainer.getVolumeUtilization());
        Assertions.assertEquals("100", resultContainer.getWeightUtilization());
    }

    @Test
    public void testCalculateUtilization_WithZeroAllocatedVolume() {
        Containers container = new Containers();
        container.setAchievedVolume(BigDecimal.valueOf(50));
        container.setAllocatedVolume(BigDecimal.ZERO);
        container.setAchievedWeight(BigDecimal.valueOf(500));
        container.setAllocatedWeight(BigDecimal.valueOf(1000));

        Containers resultContainer = containerService.calculateUtilization(container);

        Assertions.assertEquals("100", resultContainer.getVolumeUtilization());
        Assertions.assertEquals("50.0", resultContainer.getWeightUtilization());
    }

    @Test
    public void testCalculateUtilization_WithNullAchievedVolumeAndWeight() {
        Containers container = new Containers();
        container.setAllocatedVolume(BigDecimal.valueOf(100));
        container.setAllocatedWeight(BigDecimal.valueOf(1000));

        Containers resultContainer = containerService.calculateUtilization(container);

        Assertions.assertEquals("0.0", resultContainer.getVolumeUtilization());
        Assertions.assertEquals("0.0", resultContainer.getWeightUtilization());
    }

    @Test
    public void testCalculateAchieved_AllocatedForSameUnit_Success() {
        testContainer.setId(1L);
        ContainerRequest containerRequest = objectMapper.convertValue(testContainer, ContainerRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(containerRequest);
        ContainerResponse containerResponse = objectMapper.convertValue(containerRequest, ContainerResponse.class);

        when(jsonHelper.convertValue(any(ContainerRequest.class), eq(Containers.class))).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(Containers.class), eq(ContainerResponse.class))).thenReturn(containerResponse);

        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchieved_AllocatedForSameUnit(commonRequestModel);

        Assertions.assertNotNull(responseEntity);
        assertEquals(responseEntity, ResponseHelper.buildSuccessResponse(containerResponse));
    }

    @Test
    void calculateAllocatedData() {
        testContainer.setId(1L);
        CheckAllocatedDataChangesRequest request = CheckAllocatedDataChangesRequest.builder().containerCode("TEST").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        EntityTransferContainerType entityTransferContainerType = EntityTransferContainerType.builder().build();
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities(entityTransferContainerType).build();

        when(v1Service.fetchContainerTypeData(any(CommonV1ListRequest.class))).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(any() , eq(EntityTransferContainerType.class))).thenReturn(List.of(entityTransferContainerType));

        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAllocatedData(commonRequestModel);

        Assertions.assertNotNull(responseEntity);
        assertEquals(responseEntity.getStatusCodeValue() , HttpStatus.OK.value());
    }


    @Test
    void detachContainer() {
        ContainerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerResponse.class);

        when(containerDao.save(any(Containers.class))).thenReturn(testContainer);
        when(producer.getKafkaResponse(any() , anyBoolean())).thenReturn(new KafkaResponse());
        when(jsonHelper.convertToJson(any(KafkaResponse.class))).thenReturn(StringUtils.EMPTY);
        when(jsonHelper.convertValue(any(Containers.class), eq(ContainerResponse.class))).thenReturn(containerResponse);

        Packing packing = jsonTestUtility.getTestPacking();
        List<Packing> packingList = List.of(packing);

        testContainer.setId(12L);
        var response = containerService.detachContainer(packingList , testContainer , 12L , true);

        verify(shipmentsContainersMappingDao, times(1)).detachShipments(anyLong(), anyList(), anyBoolean());
        verify(packingDao, times(1)).saveAll(anyList());
        verify(packingsADSync, times(1)).sync(anyList(), anyString());
        verify(producer, times(1)).produceToKafka(anyString(), any(), any());
        assertNotNull(response);
    }

    @Test
    public void testValidateContainerNumber_Success() {
        String validContainerNumber = "ABCD123456";
        ResponseEntity<IRunnerResponse> responseEntity = containerService.validateContainerNumber(validContainerNumber);
        Assertions.assertNotNull(responseEntity);
        assertTrue(((ContainerNumberCheckResponse)((RunnerResponse)responseEntity.getBody()).getData()).isSuccess());
    }

    @Test
    public void testValidateContainerNumber_InvalidLength() {
        String invalidLengthContainerNumber = "ABC123";
        ResponseEntity<IRunnerResponse> responseEntity = containerService.validateContainerNumber(invalidLengthContainerNumber);
        Assertions.assertNotNull(responseEntity);
        assertFalse(((ContainerNumberCheckResponse)((RunnerResponse)responseEntity.getBody()).getData()).isSuccess());
    }

    @Test
    public void testValidateContainerNumber_InvalidCharacters() {
        String invalidCharactersContainerNumber = "1234ABCD56";
        ResponseEntity<IRunnerResponse> responseEntity = containerService.validateContainerNumber(invalidCharactersContainerNumber);
        Assertions.assertNotNull(responseEntity);
        assertFalse(((ContainerNumberCheckResponse)((RunnerResponse)responseEntity.getBody()).getData()).isSuccess());
    }

    @Test
    void getContainers() {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(listCommonRequest);
        Page<Containers> page = new PageImpl<>(List.of(testContainer) , PageRequest.of(0 , 10) , 1);
        ContainerResponse containerResponse = objectMapper.convertValue(testContainer, ContainerResponse.class);

        when(containerDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(Containers.class), eq(ContainerResponse.class))).thenReturn(containerResponse);

        var response = containerService.getContainers(requestModel);
        assertNotNull(response);
    }

    @Test
    void getContainers_Failure() {
        CommonRequestModel requestModel = CommonRequestModel.buildRequest();
        var response = containerService.getContainers(requestModel);
        assertNotNull(response);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void calculateAchievedQuantity_onPackDetach_FailureOnRetrieve() {
        ContainerPackADInShipmentRequest request = new ContainerPackADInShipmentRequest();
        request.setContainerId(1L);
        request.setPacksId(List.of(2L));
        request.setShipmentId(3L);
        when(containerDao.findById(anyLong())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantity_onPackDetach(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculateAchievedQuantity_onPackDetach_EmptyOnRetrieve() {
        ContainerPackADInShipmentRequest request = new ContainerPackADInShipmentRequest();
        request.setContainerId(1L);
        request.setPacksId(List.of(2L));
        request.setShipmentId(3L);
        when(containerDao.findById(anyLong())).thenReturn(Optional.empty());
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantity_onPackDetach(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculateAchievedQuantity_onPackDetach_PacksIdNull() {
        ContainerPackADInShipmentRequest request = new ContainerPackADInShipmentRequest();
        request.setContainerId(1L);
        request.setShipmentId(3L);
        when(containerDao.findById(anyLong())).thenReturn(Optional.of(testContainer));
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantity_onPackDetach(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculateAchievedQuantity_onPackDetach_PacksIdEmpty() {
        ContainerPackADInShipmentRequest request = new ContainerPackADInShipmentRequest();
        request.setContainerId(1L);
        request.setShipmentId(3L);
        request.setPacksId(new ArrayList<>());
        when(containerDao.findById(anyLong())).thenReturn(Optional.of(testContainer));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantity_onPackDetach(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculateAchievedQuantity_onPackDetach_FCL() {
        ContainerPackADInShipmentRequest request = new ContainerPackADInShipmentRequest();
        request.setContainerId(1L);
        request.setPacksId(List.of(2L));
        request.setShipmentId(3L);
        when(containerDao.findById(anyLong())).thenReturn(Optional.of(testContainer));
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        testPacking.setId(2L);
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testPacking)));
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantity_onPackDetach(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
    }

    @Test
    void calculateAchievedQuantity_onPackDetach() {
        ContainerPackADInShipmentRequest request = new ContainerPackADInShipmentRequest();
        request.setContainerId(1L);
        request.setPacksId(List.of(2L));
        request.setShipmentId(3L);
        when(containerDao.findById(anyLong())).thenReturn(Optional.of(testContainer));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        testPacking.setId(2L);
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testPacking)));
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantity_onPackDetach(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
    }

    @Test
    void calculateAchievedQuantity_onPackDetach_EmptyPackOnRetrieve() {
        ContainerPackADInShipmentRequest request = new ContainerPackADInShipmentRequest();
        request.setContainerId(1L);
        request.setPacksId(List.of(2L));
        request.setShipmentId(3L);
        when(containerDao.findById(anyLong())).thenReturn(Optional.of(testContainer));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        testPacking.setId(2L);
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>()));
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantity_onPackDetach(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
    }

    @Test
    void calculateAchievedQuantity_onPackDetach_NullPackOnRetrieve() {
        ContainerPackADInShipmentRequest request = new ContainerPackADInShipmentRequest();
        request.setContainerId(1L);
        request.setPacksId(List.of(2L));
        request.setShipmentId(3L);
        when(containerDao.findById(anyLong())).thenReturn(Optional.of(testContainer));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        testPacking.setId(2L);
        when(packingDao.findAll(any(), any())).thenReturn(null);
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantity_onPackDetach(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
    }

    @Test
    void getContainersForSelection() {
        ContainerAssignListRequest containerAssignListRequest = new ContainerAssignListRequest();
        containerAssignListRequest.setShipmentId(1L);
        containerAssignListRequest.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        containerAssignListRequest.setConsolidationId(2L);
        containerAssignListRequest.setTake(20);
        testContainer.setAchievedWeightUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setAchievedVolumeUnit(Constants.VOLUME_UNIT_M3);
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testContainer)));
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(objectMapper.convertValue(testContainer, ContainerResponse.class));
        when(masterDataUtils.createInBulkCommodityTypeRequest(any(), any(), any(), any())).thenReturn(new ArrayList<>());
        ResponseEntity<IRunnerResponse> responseEntity = containerService.getContainersForSelection(CommonRequestModel.buildRequest(containerAssignListRequest));
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getContainersForSelection_NonSeaCase() {
        ContainerAssignListRequest containerAssignListRequest = new ContainerAssignListRequest();
        containerAssignListRequest.setShipmentId(1L);
        containerAssignListRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        containerAssignListRequest.setConsolidationId(2L);
        testContainer.setAchievedWeightUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setAchievedVolumeUnit(Constants.VOLUME_UNIT_M3);
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        testContainer.setShipmentsList(List.of(testShipment));
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testContainer)));
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(objectMapper.convertValue(testContainer, ContainerResponse.class));
        when(masterDataUtils.createInBulkCommodityTypeRequest(any(), any(), any(), any())).thenReturn(new ArrayList<>());
        ResponseEntity<IRunnerResponse> responseEntity = containerService.getContainersForSelection(CommonRequestModel.buildRequest(containerAssignListRequest));
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getContainersForSelection_WithShipmentsList() {
        ContainerAssignListRequest containerAssignListRequest = new ContainerAssignListRequest();
        containerAssignListRequest.setShipmentId(1L);
        containerAssignListRequest.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        containerAssignListRequest.setConsolidationId(2L);
        containerAssignListRequest.setTake(20);
        testContainer.setAchievedWeightUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setAchievedVolumeUnit(Constants.VOLUME_UNIT_M3);
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        testContainer.setShipmentsList(List.of(testShipment));
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testContainer)));
        ResponseEntity<IRunnerResponse> responseEntity = containerService.getContainersForSelection(CommonRequestModel.buildRequest(containerAssignListRequest));
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getContainersForSelection_() {
        ContainerAssignListRequest containerAssignListRequest = new ContainerAssignListRequest();
        containerAssignListRequest.setShipmentId(1L);
        containerAssignListRequest.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        containerAssignListRequest.setConsolidationId(2L);
        containerAssignListRequest.setTake(20);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setMultipleShipmentEnabled(false);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsConsolidator(true);
        testContainer.setAchievedWeightUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setAchievedVolumeUnit(Constants.VOLUME_UNIT_M3);
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        testContainer.setShipmentsList(List.of(testShipment));
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testContainer)));
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(objectMapper.convertValue(testContainer, ContainerResponse.class));
        when(masterDataUtils.createInBulkCommodityTypeRequest(any(), any(), any(), any())).thenReturn(new ArrayList<>());
        ResponseEntity<IRunnerResponse> responseEntity = containerService.getContainersForSelection(CommonRequestModel.buildRequest(containerAssignListRequest));
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getContainersForSelection_Failure() {
        ContainerAssignListRequest containerAssignListRequest = new ContainerAssignListRequest();
        containerAssignListRequest.setShipmentId(1L);
        containerAssignListRequest.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        containerAssignListRequest.setConsolidationId(2L);
        containerAssignListRequest.setTake(20);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setMultipleShipmentEnabled(false);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsConsolidator(true);
        testContainer.setAchievedWeightUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setAchievedVolumeUnit(Constants.VOLUME_UNIT_M3);
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        testContainer.setShipmentsList(List.of(testShipment));
        when(containerDao.findAll(any(), any())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = containerService.getContainersForSelection(CommonRequestModel.buildRequest(containerAssignListRequest));
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void checkForDelete() {
        List<ShipmentsContainersMapping> shipmentsContainersMappingList = new ArrayList<>();
        shipmentsContainersMappingList.add(new ShipmentsContainersMapping());
        shipmentsContainersMappingList.add(new ShipmentsContainersMapping());
        when(shipmentsContainersMappingDao.findByContainerId(anyLong())).thenReturn(shipmentsContainersMappingList);
        ResponseEntity<IRunnerResponse> responseEntity = containerService.checkForDelete(CommonRequestModel.buildRequest(1L));
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void checkForDelete_null() {
        when(shipmentsContainersMappingDao.findByContainerId(anyLong())).thenReturn(null);
        ResponseEntity<IRunnerResponse> responseEntity = containerService.checkForDelete(CommonRequestModel.buildRequest(1L));
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void checkForDelete_empty() {
        when(shipmentsContainersMappingDao.findByContainerId(anyLong())).thenReturn(new ArrayList<>());
        ResponseEntity<IRunnerResponse> responseEntity = containerService.checkForDelete(CommonRequestModel.buildRequest(1L));
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void checkForDelete_Failure() {
        when(shipmentsContainersMappingDao.findByContainerId(anyLong())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = containerService.checkForDelete(CommonRequestModel.buildRequest(1L));
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculateContainerSummary() throws RunnerException{
        List<Containers> containersList = List.of(testContainer);
        ContainerSummaryResponse containerSummaryResponse = containerService.calculateContainerSummary(containersList, Constants.TRANSPORT_MODE_SEA, Constants.SHIPMENT_TYPE_LCL);
        assertNotNull(containerSummaryResponse);
    }

    @Test
    void calculateContainerSummary_Branches() throws RunnerException{
        testContainer.setNoOfPackages(null);
        testContainer.setPacks(null);
        testContainer.setContainerCount(null);
        List<Containers> containersList = List.of(testContainer);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setWeightChargeableUnit(null);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setVolumeChargeableUnit(null);
        ContainerSummaryResponse containerSummaryResponse = containerService.calculateContainerSummary(containersList, Constants.TRANSPORT_MODE_SEA, Constants.SHIPMENT_TYPE_LCL);
        assertNotNull(containerSummaryResponse);
    }

    @Test
    void calculateContainerSummary_() {
        List<Containers> containersList = new ArrayList<>();
        containersList.add(testContainer);
        String summary = containerService.calculateContainerSummary(containersList);
        assertNotNull(summary);
    }

    @Test
    void calculateContainerSummary_ContCount() {
        List<Containers> containersList = new ArrayList<>();
        testContainer.setContainerCount(4L);
        testContainer.setIsPart(true);
        containersList.add(testContainer);
        String summary = containerService.calculateContainerSummary(containersList);
        assertNotNull(summary);
    }

    @Test
    void calculateContainerSummary_ContCount_MultipleConts() {
        List<Containers> containersList = new ArrayList<>();
        testContainer.setContainerCount(4L);
        testContainer.setIsPart(false);
        containersList.add(testContainer);
        containersList.add(testContainer);
        String summary = containerService.calculateContainerSummary(containersList);
        assertNotNull(summary);
    }

    @Test
    void calculateContainerSummary_ContCount_MultipleConts_IsPart() {
        List<Containers> containersList = new ArrayList<>();
        testContainer.setContainerCount(4L);
        testContainer.setIsPart(true);
        containersList.add(testContainer);
        containersList.add(testContainer);
        String summary = containerService.calculateContainerSummary(containersList);
        assertNotNull(summary);
    }

    @Test
    void calculateContainerSummary_NullRequest() {
        String summary = containerService.calculateContainerSummary(null);
        assertNull(summary);
    }

    @Test
    void calculateContainerSummary_EmptyRequest() {
        String summary = containerService.calculateContainerSummary(new ArrayList<>());
        assertNull(summary);
    }

    @Test
    void V1ContainerCreateAndUpdate() throws RunnerException{
        when(syncEntityConversionService.containerV1ToV2(any())).thenReturn(testContainer);
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(jsonTestUtility.getTestConsolidation()));
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testShipment)));
        when(containerDao.save(any())).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(objectMapper.convertValue(testContainer, ContainerResponse.class));
        ResponseEntity<IRunnerResponse> responseEntity = containerService.V1ContainerCreateAndUpdate(CommonRequestModel.buildRequest(jsonTestUtility.getTestContainerRequestV2()), false);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void V1ContainerCreateAndUpdate_ExistingCont() throws RunnerException{
        when(syncEntityConversionService.containerV1ToV2(any())).thenReturn(testContainer);
        when(containerDao.findByGuid(any())).thenReturn(List.of(testContainer));
        when(containerDao.save(any())).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(objectMapper.convertValue(testContainer, ContainerResponse.class));
        ResponseEntity<IRunnerResponse> responseEntity = containerService.V1ContainerCreateAndUpdate(CommonRequestModel.buildRequest(jsonTestUtility.getTestContainerRequestV2()), false);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void V1ContainerCreateAndUpdate_Failure() throws RunnerException{
        when(containerDao.findByGuid(any())).thenThrow(new RuntimeException());
        assertThrows(RuntimeException.class, () -> containerService.V1ContainerCreateAndUpdate(CommonRequestModel.buildRequest(jsonTestUtility.getTestContainerRequestV2()), false));
    }

    @Test
    void V1BulkContainerCreateAndUpdate() throws RunnerException{
        BulkContainerRequestV2 bulkContainerRequestV2 = BulkContainerRequestV2.builder().bulkContainers(List.of(jsonTestUtility.getTestContainerRequestV2())).build();
        ContainerService spyService = spy(containerService);
        doReturn(new ResponseEntity<>(HttpStatus.OK)).when(spyService).V1ContainerCreateAndUpdate(any(), anyBoolean());
        ResponseEntity<IRunnerResponse> responseEntity = spyService.V1BulkContainerCreateAndUpdate(CommonRequestModel.buildRequest(bulkContainerRequestV2));
        assertNotNull(responseEntity);
    }

    @Test
    void V1BulkContainerCreateAndUpdate_Failure() throws RunnerException{
        BulkContainerRequestV2 bulkContainerRequestV2 = BulkContainerRequestV2.builder().bulkContainers(List.of(jsonTestUtility.getTestContainerRequestV2())).build();
        ContainerService spyService = spy(containerService);
        doThrow(new RunnerException()).when(spyService).V1ContainerCreateAndUpdate(any(), anyBoolean());
        assertThrows(RuntimeException.class, () -> spyService.V1BulkContainerCreateAndUpdate(CommonRequestModel.buildRequest(bulkContainerRequestV2)));
    }

}