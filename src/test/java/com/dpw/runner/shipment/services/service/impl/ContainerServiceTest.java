package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.Kafka.Dto.KafkaResponse;
import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CheckAllocatedDataChangesRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
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
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
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

}