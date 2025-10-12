package com.dpw.runner.shipment.services.service.impl;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ExportContainerListRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CheckAllocatedDataChangesRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerAssignListRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerPackADInShipmentRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerEventExcelModel;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.ContainersExcelModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.service_bus.model.ContainerBoomiUniversalJson;
import com.dpw.runner.shipment.services.service_bus.model.EventMessage;
import com.dpw.runner.shipment.services.syncing.Entity.BulkContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.impl.SyncEntityConversionService;
import com.dpw.runner.shipment.services.syncing.interfaces.IContainerSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IContainersSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
import com.dpw.runner.shipment.services.utils.CSVParsingUtil;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class ContainerServiceTest extends CommonMocks {

    @Mock
    private ConsolidationService consolidationService;

    @Mock
    private IContainersSync containersSync;

    @Mock
    private IContainerSync containerSync;

    @Mock
    private CSVParsingUtil parser;

    @Mock
    private CSVParsingUtil newParser;

    @Mock
    private ICustomerBookingDao customerBookingDao;

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

    @Mock
    private ISBUtils sbUtils;

    @Mock
    private ISBProperties isbProperties;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private V1ServiceUtil v1ServiceUtil;

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
    void setUp() {
        testContainer = jsonTestUtility.getTestContainer();
        testShipment = jsonTestUtility.getTestShipment();
        testPacking = jsonTestUtility.getTestPacking();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        TenantContext.setCurrentTenant(1);
        mockUser.setUsername("user");
        TenantContext.setCurrentTenant(1);
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").multipleShipmentEnabled(true).build());
        MockitoAnnotations.initMocks(this);
    }


    @Test
    void testCreate_Success() {
        ResponseEntity<IRunnerResponse> responseEntity = containerService.create(CommonRequestModel.buildRequest());

        Assertions.assertNull(responseEntity);
    }



    @Test
    void testAttachPacks_Success() {
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
    @Disabled("Skipped")
    void testAttachPacks_ContainerNotFound() {
        long containerId = 1L;
        List<Long> packsId = new ArrayList<>();
        packsId.add(100L);
        packsId.add(101L);

        when(containerDao.findById(containerId)).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> responseEntity = containerService.attachPacks(containerId, packsId);

        Assertions.assertNull(responseEntity);
    }

    @Test
    void testAttachPacks_PackingNotFound() {
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
    void testUpdate() throws RunnerException{
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
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(),any())).thenReturn(containerResponse);
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
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(),any())).thenReturn(containerResponse);

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
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(),any())).thenReturn(containerResponse);

        ResponseEntity<IRunnerResponse> responseEntity = containerService.retrieveById(commonRequestModel);

        assertNotNull(responseEntity);
        assertEquals(responseEntity, ResponseHelper.buildSuccessResponse(containerResponse));
    }

    @Test
    void testCalculateUtilization_WithNonNullContainer() {
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
    void testCalculateUtilization_WithNullContainer() {
        Containers resultContainer = containerService.calculateUtilization(null);
        Assertions.assertNull(resultContainer);
    }

    @Test
    void testCalculateUtilization_WithZeroAllocatedWeight() {
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
    void testCalculateUtilization_WithZeroAllocatedVolume() {
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
    void testCalculateUtilization_WithNullAchievedVolumeAndWeight() {
        Containers container = new Containers();
        container.setAllocatedVolume(BigDecimal.valueOf(100));
        container.setAllocatedWeight(BigDecimal.valueOf(1000));

        Containers resultContainer = containerService.calculateUtilization(container);

        Assertions.assertEquals("0.0", resultContainer.getVolumeUtilization());
        Assertions.assertEquals("0.0", resultContainer.getWeightUtilization());
    }

    @Test
    void testCalculateAchieved_AllocatedForSameUnit_Success() {
        testContainer.setId(1L);
        ContainerRequest containerRequest = objectMapper.convertValue(testContainer, ContainerRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(containerRequest);
        ContainerResponse containerResponse = objectMapper.convertValue(containerRequest, ContainerResponse.class);

        when(jsonHelper.convertValue(any(ContainerRequest.class), eq(Containers.class))).thenReturn(testContainer);
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(),any())).thenReturn(containerResponse);

        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedAllocatedForSameUnit(commonRequestModel);

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
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(),any())).thenReturn(containerResponse);

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
    void testValidateContainerNumber_Success() {
        String validContainerNumber = "ABCD123456";
        ResponseEntity<IRunnerResponse> responseEntity = containerService.validateContainerNumber(validContainerNumber);
        Assertions.assertNotNull(responseEntity);
        assertTrue(((ContainerNumberCheckResponse)((RunnerResponse)responseEntity.getBody()).getData()).isSuccess());
    }

    @Test
    void testValidateContainerNumber_InvalidLength() {
        String invalidLengthContainerNumber = "ABC123";
        ResponseEntity<IRunnerResponse> responseEntity = containerService.validateContainerNumber(invalidLengthContainerNumber);
        Assertions.assertNotNull(responseEntity);
        assertFalse(((ContainerNumberCheckResponse)((RunnerResponse)responseEntity.getBody()).getData()).isSuccess());
    }

    @Test
    void getContainers() {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(listCommonRequest);
        Page<Containers> page = new PageImpl<>(List.of(testContainer) , PageRequest.of(0 , 10) , 1);

        when(containerDao.findAll(any(), any())).thenReturn(page);
        var response = containerService.getContainers(requestModel);
        assertNotNull(response);
    }
    @Test
    void getContainersWithIncludeColumns() {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().includeColumns(List.of("id", "guid", "tenantId", "containerNumber")).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(listCommonRequest);
        Page<Containers> page = new PageImpl<>(List.of(testContainer) , PageRequest.of(0 , 10) , 1);

        when(containerDao.findAll(any(), any())).thenReturn(page);
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
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantityOnPackDetach(CommonRequestModel.buildRequest(request));
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
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantityOnPackDetach(CommonRequestModel.buildRequest(request));
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
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantityOnPackDetach(CommonRequestModel.buildRequest(request));
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
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantityOnPackDetach(CommonRequestModel.buildRequest(request));
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
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantityOnPackDetach(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
    }

    @Test
    void calculateAchievedQuantityOnPackDetach() {
        ContainerPackADInShipmentRequest request = new ContainerPackADInShipmentRequest();
        request.setContainerId(1L);
        request.setPacksId(List.of(2L));
        request.setShipmentId(3L);
        when(containerDao.findById(anyLong())).thenReturn(Optional.of(testContainer));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        testPacking.setId(2L);
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testPacking)));
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantityOnPackDetach(CommonRequestModel.buildRequest(request));
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
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantityOnPackDetach(CommonRequestModel.buildRequest(request));
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
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantityOnPackDetach(CommonRequestModel.buildRequest(request));
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
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(),any())).thenReturn(objectMapper.convertValue(testContainer, ContainerResponse.class));
        when(masterDataUtils.createInBulkCommodityTypeRequest(any(), any(), any(), any(), any())).thenReturn(new ArrayList<>());
        mockShipmentSettings();
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
        testContainer.setShipmentsList(Set.of(testShipment));
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testContainer)));
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(),any())).thenReturn(objectMapper.convertValue(testContainer, ContainerResponse.class));
        when(masterDataUtils.createInBulkCommodityTypeRequest(any(), any(), any(), any(), any())).thenReturn(new ArrayList<>());
        mockShipmentSettings();
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
        testContainer.setShipmentsList(Set.of(testShipment));
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testContainer)));
        mockShipmentSettings();
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
        testContainer.setShipmentsList(Set.of(testShipment));
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testContainer)));
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(),any())).thenReturn(objectMapper.convertValue(testContainer, ContainerResponse.class));
        when(masterDataUtils.createInBulkCommodityTypeRequest(any(), any(), any(), any(), any())).thenReturn(new ArrayList<>());
        mockShipmentSettings();
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
        testContainer.setShipmentsList(Set.of(testShipment));
        when(containerDao.findAll(any(), any())).thenThrow(new RuntimeException());
        mockShipmentSettings();
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
        mockShipmentSettings();
        mockTenantSettings();
        ContainerSummaryResponse containerSummaryResponse = containerService.calculateContainerSummary(containersList, Constants.TRANSPORT_MODE_SEA, Constants.SHIPMENT_TYPE_LCL);
        assertNotNull(containerSummaryResponse);
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
        ResponseEntity<IRunnerResponse> responseEntity = containerService.v1ContainerCreateAndUpdate(CommonRequestModel.buildRequest(jsonTestUtility.getTestContainerRequestV2()), false);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void V1ContainerCreateAndUpdate_ExistingCont() throws RunnerException{
        when(syncEntityConversionService.containerV1ToV2(any())).thenReturn(testContainer);
        when(containerDao.findByGuid(any())).thenReturn(List.of(testContainer));
        when(containerDao.save(any())).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(), eq(ContainerResponse.class))).thenReturn(objectMapper.convertValue(testContainer, ContainerResponse.class));
        ResponseEntity<IRunnerResponse> responseEntity = containerService.v1ContainerCreateAndUpdate(CommonRequestModel.buildRequest(jsonTestUtility.getTestContainerRequestV2()), false);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void V1ContainerCreateAndUpdate_Failure(){
        when(containerDao.findByGuid(any())).thenThrow(new RuntimeException());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(jsonTestUtility.getTestContainerRequestV2());
        assertThrows(RuntimeException.class, () -> containerService.v1ContainerCreateAndUpdate(commonRequestModel, false));
    }

    @Test
    void V1BulkContainerCreateAndUpdate() throws RunnerException{
        BulkContainerRequestV2 bulkContainerRequestV2 = BulkContainerRequestV2.builder().bulkContainers(List.of(jsonTestUtility.getTestContainerRequestV2())).build();
        ContainerService spyService = spy(containerService);
        doReturn(new ResponseEntity<>(HttpStatus.OK)).when(spyService).v1ContainerCreateAndUpdate(any(), anyBoolean());
        ResponseEntity<IRunnerResponse> responseEntity = spyService.v1BulkContainerCreateAndUpdate(CommonRequestModel.buildRequest(bulkContainerRequestV2));
        assertNotNull(responseEntity);
    }

    @Test
    void V1BulkContainerCreateAndUpdate_Failure() throws RunnerException{
        BulkContainerRequestV2 bulkContainerRequestV2 = BulkContainerRequestV2.builder().bulkContainers(List.of(jsonTestUtility.getTestContainerRequestV2())).build();
        ContainerService spyService = spy(containerService);
        doThrow(new RunnerException()).when(spyService).v1ContainerCreateAndUpdate(any(), anyBoolean());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(bulkContainerRequestV2);
        assertThrows(RuntimeException.class, () -> spyService.v1BulkContainerCreateAndUpdate(commonRequestModel));
    }

    @Test
    void testExportContainers_Success() {
        HttpServletResponse response = new MockHttpServletResponse();
        ExportContainerListRequest request = new ExportContainerListRequest();
        request.setConsolidationId("1");
        request.setFreeTimeNoOfDaysDetention(3L);
        request.setFreeTimeNoOfDaysStorage(4L);

        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        List<Containers> containers = new ArrayList<>();
        testContainer.setBookingId(6L);
        containers.add(testContainer);
        consolidationDetails.setContainersList(containers);

        Optional<ConsolidationDetails> consol = Optional.of(consolidationDetails);
        when(consolidationDetailsDao.findById(1L)).thenReturn(consol);
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(),any())).thenReturn(objectMapper.convertValue(testContainer, ContainerResponse.class));

        List<String> contHeaders = new ArrayList<>();
        contHeaders.add("containerNumber");
        when(parser.getHeadersForContainer()).thenReturn(contHeaders);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(jsonTestUtility.getCustomerBooking()));
        assertDoesNotThrow(() -> containerService.exportContainers(response, request));
    }

    @Test
    void testExportContainers_Failure_ConsoleIdNull(){
        HttpServletResponse response = new MockHttpServletResponse();
        ExportContainerListRequest request = new ExportContainerListRequest();
        request.setFreeTimeNoOfDaysDetention(3L);
        request.setFreeTimeNoOfDaysStorage(4L);
        assertThrows(RuntimeException.class, () -> containerService.exportContainers(response, request));
    }

    @Test
    void testExportContainers_Failure_ConsoleNotFound() {
        HttpServletResponse response = new MockHttpServletResponse();
        ExportContainerListRequest request = new ExportContainerListRequest();
        request.setConsolidationId("1");
        request.setFreeTimeNoOfDaysDetention(3L);
        request.setFreeTimeNoOfDaysStorage(4L);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.empty());
        assertThrows(RuntimeException.class, () -> containerService.exportContainers(response, request));
    }

    @Test
    void testExportContainers_Failure_EmptyContainers() {
        HttpServletResponse response = new MockHttpServletResponse();
        ExportContainerListRequest request = new ExportContainerListRequest();
        request.setConsolidationId("1");
        request.setFreeTimeNoOfDaysDetention(3L);
        request.setFreeTimeNoOfDaysStorage(4L);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        List<Containers> containers = new ArrayList<>();
        consolidationDetails.setContainersList(containers);

        Optional<ConsolidationDetails> consol = Optional.of(consolidationDetails);
        when(consolidationDetailsDao.findById(1L)).thenReturn(consol);
        assertThrows(RuntimeException.class, () -> containerService.exportContainers(response, request));
    }

    @Test
    void testExportContainers_Failure_NullContainers() {
        HttpServletResponse response = new MockHttpServletResponse();
        ExportContainerListRequest request = new ExportContainerListRequest();
        request.setConsolidationId("1");
        request.setFreeTimeNoOfDaysDetention(3L);
        request.setFreeTimeNoOfDaysStorage(4L);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setContainersList(null);

        Optional<ConsolidationDetails> consol = Optional.of(consolidationDetails);
        when(consolidationDetailsDao.findById(1L)).thenReturn(consol);
        assertThrows(RuntimeException.class, () -> containerService.exportContainers(response, request));
    }

    @Test
    void afterSaveList() {
        List<Containers> containersList = new ArrayList<>();
        testContainer.setTenantId(66);
        containersList.add(testContainer);
        assertDoesNotThrow(() -> containerService.afterSaveList(containersList, true));
    }

    @Test
    void afterSaveList_Empty() {
        List<Containers> containersList = new ArrayList<>();
        assertDoesNotThrow(() -> containerService.afterSaveList(containersList, true));
    }

    @Test
    void afterSaveList_Null() {
        assertDoesNotThrow(() -> containerService.afterSaveList(null, true));
    }

    @Test
    void detachContainer_SyncFailure() {
        List<Packing> packingList = new ArrayList<>();
        packingList.add(testPacking);
        when(containerDao.save(any())).thenReturn(testContainer);
        when(packingsADSync.sync(any(), any())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = containerService.detachContainer(packingList, testContainer, 4L, false);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void detachContainer_Failure() {
        List<Packing> packingList = new ArrayList<>();
        packingList.add(testPacking);
        when(containerDao.save(any())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = containerService.detachContainer(packingList, testContainer, 4L, false);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculateAllocatedData_Changes() {
        testContainer.setId(1L);
        CheckAllocatedDataChangesRequest request = CheckAllocatedDataChangesRequest.builder()
                .containerCode("20GP")
                .allocatedVolume(new BigDecimal(37893))
                .allocatedVolumeUnit(Constants.VOLUME_UNIT_M3)
                .allocatedWeight(new BigDecimal(36288))
                .allocatedWeightUnit(Constants.WEIGHT_UNIT_KG)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        EntityTransferContainerType entityTransferContainerType = EntityTransferContainerType.builder()
                .CubicCapacityUnit(Constants.VOLUME_UNIT_M3)
                .CubicCapacity(43.0)
                .MaxCargoGrossWeight(434.9)
                .MaxCargoGrossWeightUnit(Constants.WEIGHT_UNIT_KG)
                .build();
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities(entityTransferContainerType).build();

        when(v1Service.fetchContainerTypeData(any(CommonV1ListRequest.class))).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(any() , eq(EntityTransferContainerType.class))).thenReturn(List.of(entityTransferContainerType));

        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAllocatedData(commonRequestModel);

        Assertions.assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK.value(), responseEntity.getStatusCodeValue());
    }

    @Test
    void calculateAllocatedData_Failure() {
        testContainer.setId(1L);
        CheckAllocatedDataChangesRequest request = CheckAllocatedDataChangesRequest.builder()
                .containerCode("20GP")
                .allocatedVolume(new BigDecimal(37893))
                .allocatedVolumeUnit(Constants.VOLUME_UNIT_M3)
                .allocatedWeight(new BigDecimal(36288))
                .allocatedWeightUnit(Constants.WEIGHT_UNIT_KG)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        when(v1Service.fetchContainerTypeData(any(CommonV1ListRequest.class))).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAllocatedData(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST.value(), responseEntity.getStatusCodeValue());
    }

    @Test
    void calculateAchieved_AllocatedForSameUnit_failure() {
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedAllocatedForSameUnit(null);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveById() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        ResponseEntity<IRunnerResponse> responseEntity = containerService.retrieveById(commonRequestModel);
        assertNotNull(responseEntity);
    }

    @Test
    void retrieveById_IdNull() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder()
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);
        ResponseEntity<IRunnerResponse> responseEntity = containerService.retrieveById(commonRequestModel);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveById_ContainerEmpty() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder()
                .id(1L)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);
        when(containerDao.findById(any())).thenReturn(Optional.empty());
        ResponseEntity<IRunnerResponse> responseEntity = containerService.retrieveById(commonRequestModel);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void delete() {
        assertThrows(NullPointerException.class, () -> containerService.delete(null));
    }

    @Test
    void delete_IdNull() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        when(containerDao.findById(any())).thenReturn(Optional.empty());
        ResponseEntity<IRunnerResponse> responseEntity = containerService.delete(commonRequestModel);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void delete_Failure() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        when(containerDao.findById(any())).thenReturn(Optional.of(testContainer));
        when(jsonHelper.convertToJson(any())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = containerService.delete(commonRequestModel);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listAsync_Failure() throws Exception{
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntityCompletableFuture = containerService.listAsync(commonRequestModel);
        assertNotNull(responseEntityCompletableFuture);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntityCompletableFuture.get().getStatusCode());
    }

    @Test
    void list_Failure(){
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        ResponseEntity<IRunnerResponse> responseEntity = containerService.list(commonRequestModel);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void uploadContainers_NullReq() {
        assertThrows(ValidationException.class, () -> containerService.uploadContainers(null));
    }

    @Test
    void uploadContainers_NullConsoleId(){
        BulkUploadRequest request = new BulkUploadRequest();
        assertThrows(ValidationException.class, () -> containerService.uploadContainers(request));
    }

    @Test
    void uploadContainers_OwnContAndShipperOwnTrue() throws Exception{
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(true);
        testContainer.setHazardous(true);
        testContainer.setIsPart(true);
        testContainer.setContainerStuffingLocation("unloc");
        testContainer.setHazardousUn("hzUn");
        testContainer.setCommodityCode("680510");
        testContainer.setHandlingInfo("handlingInfo");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        testContainer.setDgClass("dgClass");
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    return List.of(testContainer);
                });
        assertThrows(ValidationException.class, () -> containerService.uploadContainers(request));
    }

    @Test
    void uploadContainers_DgClassInvalid() throws Exception{
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(false);
        testContainer.setHazardous(true);
        testContainer.setHazardousUn("hzUn");
        testContainer.setCommodityCode("680510");
        testContainer.setHandlingInfo("handlingInfo");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        testContainer.setIsPart(false);
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    testContainer.setContainerNumber("MSKU00000");
                    return List.of(testContainer);
                });
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(jsonTestUtility.getVolumeWeightChargeable());
        assertThrows(ValidationException.class, () -> containerService.uploadContainers(request));
    }

    @Test
    void uploadContainers_ContNumAndCountInvalid() throws Exception{
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(false);
        testContainer.setHazardous(true);
        testContainer.setIsPart(true);
        testContainer.setContainerStuffingLocation("unloc");
        testContainer.setHazardousUn("hzUn");
        testContainer.setCommodityCode("680510");
        testContainer.setHandlingInfo("handlingInfo");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        testContainer.setDgClass("dgClass");
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    return List.of(testContainer);
                });
        assertThrows(ValidationException.class, () -> containerService.uploadContainers(request));
    }

    @Test
    void uploadContainers_ContainerNumberAlreadyExists() throws Exception{
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setContainerCount(1L);
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(false);
        testContainer.setHazardous(false);
        testContainer.setIsPart(false);
        testContainer.setContainerStuffingLocation("unloc");
        testContainer.setHandlingInfo(null);
        Containers containers = new Containers();
        containers.setGuid(UUID.randomUUID());
        containers.setContainerNumber("CONT0000006");
        containers.setContainerCount(1L);
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(containers));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(containers));
        ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    return List.of(testContainer);
                });
        assertThrows(ValidationException.class, () -> containerService.uploadContainers(request));
    }

    @Test
    void uploadContainers_MultipleContainerOwnership() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setContainerCount(1L);
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(true);
        testContainer.setContainerStuffingLocation("unloc");
        testContainer.setCommodityCode("680510");
        testContainer.setHandlingInfo("handlingInfo");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    return List.of(testContainer);
                });
        assertThrows(ValidationException.class, () -> containerService.uploadContainers(request));
    }

    @Test
    void uploadContainers_CommodityTypeException() throws IOException {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setContainerCount(1L);
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(false);
        testContainer.setContainerStuffingLocation("unloc");
        testContainer.setCommodityCode("46321");
        testContainer.setHandlingInfo("handlingInfo");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    return List.of(testContainer);
                });
        assertThrows(ValidationException.class, () -> containerService.uploadContainers(request));
    }

    @Test
    void uploadContainers_SEA() throws Exception{
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(false);
        testContainer.setHazardous(true);
        testContainer.setIsPart(true);
        testContainer.setContainerStuffingLocation("unloc");
        testContainer.setHazardousUn("hzUn");
        testContainer.setCommodityCode("680510");
        testContainer.setHandlingInfo("handlingInfo");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        testContainer.setDgClass("dgClass");
        testContainer.setContainerCount(1L);
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    return List.of(testContainer);
                });
        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
        when(containerDao.saveAll(any())).thenReturn(List.of(testContainer));
        assertDoesNotThrow(() -> containerService.uploadContainers(request));
    }

    @Test
    void uploadContainers_SEA2() throws Exception{
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setContainerCount(1L);
        testContainer.setIsOwnContainer(false);
        testContainer.setIsShipperOwned(true);
        testContainer.setHazardous(false);
        testContainer.setIsPart(true);
        testContainer.setContainerStuffingLocation("unloc");
        testContainer.setHazardousUn("hzUn");
        testContainer.setCommodityCode(null);
        testContainer.setHandlingInfo("handlingInfo");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        testContainer.setDgClass("dgClass");
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    return List.of(testContainer);
                });
        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
        when(containerDao.saveAll(any())).thenReturn(List.of(testContainer));
        assertDoesNotThrow(() -> containerService.uploadContainers(request));
    }

    @Test
    void uploadContainers_AIR() throws Exception{
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(false);
        testContainer.setHazardous(null);
        testContainer.setIsPart(true);
        testContainer.setContainerStuffingLocation("unloc");
        testContainer.setHazardousUn("hzUn");
        testContainer.setCommodityCode("680510");
        testContainer.setHandlingInfo("handlingInfo");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        testContainer.setDgClass("dgClass");
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                   return List.of(testContainer);
                });
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(jsonTestUtility.getVolumeWeightChargeable());
        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
        when(containerDao.saveAll(any())).thenReturn(List.of(testContainer));
        assertDoesNotThrow(() -> containerService.uploadContainers(request));
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
        assertDoesNotThrow(() -> containerService.downloadContainers(response, request));
    }

    @Test
    void downloadContainers2(){
        HttpServletResponse response = new MockHttpServletResponse();
        BulkDownloadRequest request = new BulkDownloadRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setConsolidationId("3");
        request.setShipmentId("6");
        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
        assertDoesNotThrow(() -> containerService.downloadContainers(response, request));
    }

    @Test
    void downloadContainers3(){
        HttpServletResponse response = new MockHttpServletResponse();
        BulkDownloadRequest request = new BulkDownloadRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setConsolidationId("3");
        request.setShipmentId("6");
        ShipmentDetails shipmentDetails1 = testShipment;
        shipmentDetails1.setDirection(null);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidation();
        consolidationDetails.setShipmentType(null);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails1));
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.convertToList(anyList(), eq(ContainersExcelModel.class))).thenReturn(List.of(objectMapper.convertValue(testContainer, ContainersExcelModel.class)));
        assertDoesNotThrow(() -> containerService.downloadContainers(response, request));
    }

    @Test
    void downloadContainerEvents(){
        HttpServletResponse response = new MockHttpServletResponse();
        BulkDownloadRequest request = new BulkDownloadRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setConsolidationId("3");
        request.setShipmentId("6");
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testContainer)));
        when(commonUtils.convertToList(any(), eq(ContainerEventExcelModel.class))).thenReturn(List.of(objectMapper.convertValue(jsonTestUtility.getTestEventData(), ContainerEventExcelModel.class)));
        assertDoesNotThrow(() -> containerService.downloadContainerEvents(response, request));
    }

    @Test
    void uploadContainerEvents() throws Exception {
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(4L);
        when(newParser.parseExcelFile(any(), any(), any(), any(), any(), any(), any(), any(), any())).thenReturn(List.of(jsonTestUtility.getTestEventData()));
        assertDoesNotThrow(() -> containerService.uploadContainerEvents(request));
    }

    @Test
    void uploadContainerEvents_Failure() {
        assertThrows(ValidationException.class, () -> containerService.uploadContainerEvents(null));
    }

    @Test
    void uploadContainerEvents_Failure_NullId() {
        BulkUploadRequest request = new BulkUploadRequest();
        assertThrows(ValidationException.class, () -> containerService.uploadContainerEvents(request));
    }

    @Test
    void testPushContainersToDependentServices() {
        // Arrange
        int tenantId = 1;
        TenantContext.setCurrentTenant(tenantId);

        Containers c1 = new Containers();
        c1.setId(1L);
        c1.setConsolidationId(1L);
        c1.setContainerNumber("C123");
        c1.setShipmentsList(new HashSet<>(Collections.singletonList(ShipmentDetails.builder().bookingReference("DBFC-4317515-934863").build())));
        Containers c2 = new Containers();
        c2.setId(2L);
        c2.setContainerNumber("C456");

        List<Containers> containersList = Arrays.asList(c1,c2);
        List<Containers> oldContainers = List.of(c1);

        V1TenantSettingsResponse v1TenantSettingsResponse = new V1TenantSettingsResponse();
        v1TenantSettingsResponse.setLogicAppIntegrationEnabled(true);
        v1TenantSettingsResponse.setTransportOrchestratorEnabled(true);
        Map<Integer, V1TenantSettingsResponse> tenantSettingsResponseMap = new HashMap<>();
        tenantSettingsResponseMap.put(tenantId, v1TenantSettingsResponse);
        when(v1ServiceUtil.getTenantSettingsMap(any())).thenReturn(tenantSettingsResponseMap);

        ContainerBoomiUniversalJson containerBoomiUniversalJson = new ContainerBoomiUniversalJson();
        containerBoomiUniversalJson.setHazardous(true);

        when(jsonHelper.convertToJson(any(EventMessage.class))).thenReturn("jsonBody");
        when(modelMapper.map(any(), eq(ContainerBoomiUniversalJson.class))).thenReturn(containerBoomiUniversalJson);

        // Act
        containerService.pushContainersToDependentServices(containersList, oldContainers, null);

        // Assert
        verify(producer, times(1)).produceToKafka(eq("jsonBody"), any(), anyString());
        verify(sbUtils, times(1)).sendMessagesToTopic(eq(isbProperties), any(), anyList());
    }

    @Test
    void testPushContainersToDependentServicesForNewContainers() {
        // Arrange
        TenantContext.setCurrentTenant(1);
        Containers c1 = new Containers();
        c1.setId(1L);
        c1.setGuid(UUID.randomUUID());
        c1.setConsolidationId(1L);
        c1.setContainerNumber("C123");
        c1.setShipmentsList(new HashSet<>(Collections.singletonList(ShipmentDetails.builder().bookingReference("DBFC-4317515-934863").build())));
        Containers c2 = new Containers();
        c2.setGuid(UUID.randomUUID());
        c2.setId(2L);
        c2.setContainerNumber("C456");

        List<Containers> containersList = Arrays.asList(c1,c2);
        List<Containers> oldContainers = List.of(c1);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Set<Containers> containers = new HashSet<>();
        containers.add(c1);
        shipmentDetails.setContainersList(containers);
        shipmentDetails.setBookingReference("DBFC-4353158-409107");

        V1TenantSettingsResponse v1TenantSettingsResponse = new V1TenantSettingsResponse();
        v1TenantSettingsResponse.setLogicAppIntegrationEnabled(true);
        v1TenantSettingsResponse.setTransportOrchestratorEnabled(true);
        Map<Integer, V1TenantSettingsResponse> tenantSettingsResponseMap = new HashMap<>();
        tenantSettingsResponseMap.put(1, v1TenantSettingsResponse);
        when(v1ServiceUtil.getTenantSettingsMap(any())).thenReturn(tenantSettingsResponseMap);

        ContainerBoomiUniversalJson containerBoomiUniversalJson = new ContainerBoomiUniversalJson();
        containerBoomiUniversalJson.setHazardous(true);

        when(jsonHelper.convertToJson(any(EventMessage.class))).thenReturn("jsonBody");
        when(modelMapper.map(any(), eq(ContainerBoomiUniversalJson.class))).thenReturn(containerBoomiUniversalJson);

        // Act
        containerService.pushContainersToDependentServices(containersList, oldContainers, shipmentDetails);

        // Assert
        verify(producer, times(1)).produceToKafka(eq("jsonBody"), any(), anyString());
        verify(sbUtils, times(1)).sendMessagesToTopic(eq(isbProperties), any(), anyList());
    }

    @Test
    void testPushContainersToDependentServicesWithEmptyBookingRef() {
        // Arrange
        TenantContext.setCurrentTenant(1);
        Containers c1 = new Containers();
        c1.setId(1L);
        c1.setConsolidationId(1L);
        c1.setContainerNumber("C123");
        c1.setShipmentsList(new HashSet<>(Collections.singletonList(ShipmentDetails.builder().bookingReference("").build())));
        Containers c2 = new Containers();
        c2.setId(2L);
        c2.setContainerNumber("C456");

        List<Containers> containersList = Arrays.asList(c1,c2);
        List<Containers> oldContainers = Arrays.asList(c1);

        V1TenantSettingsResponse v1TenantSettingsResponse = new V1TenantSettingsResponse();
        v1TenantSettingsResponse.setLogicAppIntegrationEnabled(true);
        v1TenantSettingsResponse.setTransportOrchestratorEnabled(true);
        Map<Integer, V1TenantSettingsResponse> tenantSettingsResponseMap = new HashMap<>();
        tenantSettingsResponseMap.put(1, v1TenantSettingsResponse);
        when(v1ServiceUtil.getTenantSettingsMap(any())).thenReturn(tenantSettingsResponseMap);
        // Act
        containerService.pushContainersToDependentServices(containersList, oldContainers, null);
        verify(producer, times(0)).produceToKafka(eq("jsonBody"), any(), anyString());
        verify(sbUtils, times(0)).sendMessagesToTopic(eq(isbProperties), any(), anyList());
    }

    @Test
    void getByModuleGuidAndModuleType_ValidShipmentGuid_ShouldReturnSuccessResponse() {
        UUID validGuid = UUID.randomUUID();
        // Given
        String moduleGuid = validGuid.toString();
        String moduleType = Constants.SHIPMENT;

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setContainersList(Set.of(new Containers()));

       ContainerResponse containerResponse = new ContainerResponse();

        when(shipmentDao.findByGuid(validGuid)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(), any())).thenReturn(containerResponse);

        // When
        ResponseEntity<IRunnerResponse> response = containerService.getByModuleGuidAndModuleType(moduleGuid, moduleType);

        // Then
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
    }

    @Test
    void getByModuleGuidAndModuleType_ValidConsolidationGuid_ShouldReturnSuccessResponse() {
        UUID validGuid = UUID.randomUUID();
        // Given
        String moduleGuid = validGuid.toString();
        String moduleType = Constants.CONSOLIDATION;

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setContainersList(List.of(new Containers()));

        ContainerResponse containerResponse = new ContainerResponse();

        when(consolidationDetailsDao.findByGuid(validGuid)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(), any())).thenReturn(containerResponse);

        // When
        ResponseEntity<IRunnerResponse> response = containerService.getByModuleGuidAndModuleType(moduleGuid, moduleType);

        // Then
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
    }
    @Test
    void uploadContainers_SEA4() throws Exception{
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(false);
        testContainer.setHazardous(true);
        testContainer.setIsPart(false);
        testContainer.setContainerStuffingLocation("unloc");
        testContainer.setHazardousUn("hzUn");
        testContainer.setCommodityCode("680510");
        testContainer.setHandlingInfo("handlingInfo");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        testContainer.setDgClass("dgClass");
        testContainer.setContainerCount(1L);
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor<Map> captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    return List.of(testContainer);
                });
        when(containerDao.saveAll(any())).thenReturn(List.of(testContainer));
        assertDoesNotThrow(() -> containerService.uploadContainers(request));
    }

    @Test
    void uploadContainers_SEA5() throws Exception{
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(false);
        testContainer.setHazardous(true);
        testContainer.setContainerStuffingLocation("unloc");
        testContainer.setHazardousUn("hzUn");
        testContainer.setCommodityCode("680510");
        testContainer.setHandlingInfo("handlingInfo");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        testContainer.setDgClass("dgClass");
        testContainer.setContainerCount(1L);
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor<Map> captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    return List.of(testContainer);
                });
        when(containerDao.saveAll(any())).thenReturn(List.of(testContainer));
        assertDoesNotThrow(() -> containerService.uploadContainers(request));
    }

    @Test
    void uploadContainers_SEA6() throws Exception{
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(false);
        testContainer.setHazardous(true);
        testContainer.setIsPart(true);
        testContainer.setContainerStuffingLocation("unloc");
        testContainer.setHazardousUn("hzUn");
        testContainer.setCommodityCode("680510");
        testContainer.setHandlingInfo("handlingInfo");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        testContainer.setDgClass("dgClass");
        testContainer.setContainerCount(1L);
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor<Map> captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    return List.of(testContainer);
                });
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
        assertThrows(RuntimeException.class, () -> containerService.uploadContainers(request));
    }

    @Test
    void uploadContainers_SEA7() throws Exception{
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(false);
        testContainer.setHazardous(true);
        testContainer.setIsPart(true);
        testContainer.setContainerStuffingLocation("");
        testContainer.setHazardousUn("hzUn");
        testContainer.setCommodityCode("680510");
        testContainer.setHandlingInfo("handlingInfo");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        testContainer.setDgClass("dgClass");
        testContainer.setContainerCount(1L);
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor<Map> captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    return List.of(testContainer);
                });
        when(shipmentDao.findById(any())).thenReturn(Optional.ofNullable(testShipment));
        when(containerDao.saveAll(any())).thenReturn(List.of(testContainer));
        assertDoesNotThrow(() -> containerService.uploadContainers(request));
    }

    @Test
    void uploadContainers_SEA8() throws Exception{
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(false);
        testContainer.setHazardous(true);
        testContainer.setIsPart(true);
        testContainer.setContainerStuffingLocation("abcd");
        testContainer.setHazardousUn("hzUn");
        testContainer.setCommodityCode("680510");
        testContainer.setHandlingInfo("handlingInfo");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        testContainer.setContainerCount(1L);
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor<Map> captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    return List.of(testContainer);
                });
        assertThrows(RuntimeException.class, () -> containerService.uploadContainers(request));
    }

    @Test
    void uploadContainers_AIR2() throws Exception{
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(false);
        testContainer.setHazardous(null);
        testContainer.setIsPart(true);
        testContainer.setContainerStuffingLocation("unloc");
        testContainer.setHazardousUn("hzUn");
        testContainer.setCommodityCode("680510");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        testContainer.setDgClass("dgClass");
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    return List.of(testContainer);
                });
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(jsonTestUtility.getVolumeWeightChargeable());
        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
        when(containerDao.saveAll(any())).thenReturn(List.of(testContainer));
        assertDoesNotThrow(() -> containerService.uploadContainers(request));
    }

    @Test
    void uploadContainers_AIR3() throws Exception{
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(false);
        testContainer.setHazardous(null);
        testContainer.setIsPart(true);
        testContainer.setContainerStuffingLocation("unloc");
        testContainer.setHazardousUn("hzUn");
        testContainer.setCommodityCode("680510");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        testContainer.setDgClass("dgClass");
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    return List.of(testContainer);
                });
        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
        when(containerDao.saveAll(any())).thenReturn(List.of(testContainer));
        assertDoesNotThrow(() -> containerService.uploadContainers(request));
    }

    @Test
    void calculateAchievedQuantityOnPackDetach2() {
        ContainerPackADInShipmentRequest request = new ContainerPackADInShipmentRequest();
        request.setContainerId(1L);
        request.setPacksId(List.of(2L));
        request.setShipmentId(3L);
        testContainer.setAchievedWeightUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setAchievedVolumeUnit(Constants.VOLUME_UNIT_M3);
        testContainer.setAllocatedVolumeUnit(Constants.VOLUME_UNIT_LITRE);
        testContainer.setAllocatedWeightUnit(Constants.WEIGHT_UNIT_MG);
        when(containerDao.findById(anyLong())).thenReturn(Optional.of(testContainer));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        testPacking.setId(2L);
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testPacking)));
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantityOnPackDetach(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
    }

    @Test
    void calculateAchievedQuantityOnPackDetach3() {
        ContainerPackADInShipmentRequest request = new ContainerPackADInShipmentRequest();
        request.setContainerId(1L);
        request.setPacksId(List.of(2L));
        request.setShipmentId(3L);
        testContainer.setAchievedWeightUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setAchievedVolumeUnit(Constants.VOLUME_UNIT_M3);
        when(containerDao.findById(anyLong())).thenReturn(Optional.of(testContainer));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        testPacking.setId(2L);
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testPacking)));
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantityOnPackDetach(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
    }

    @Test
    void calculateAchievedQuantityOnPackDetach4() {
        ContainerPackADInShipmentRequest request = new ContainerPackADInShipmentRequest();
        request.setContainerId(1L);
        request.setPacksId(List.of(2L));
        request.setShipmentId(3L);
        testContainer.setAchievedWeightUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setAchievedVolumeUnit(Constants.VOLUME_UNIT_M3);
        testContainer.setAllocatedVolumeUnit(Constants.VOLUME_UNIT_M3);
        testContainer.setAllocatedWeightUnit(Constants.WEIGHT_UNIT_KG);
        when(containerDao.findById(anyLong())).thenReturn(Optional.of(testContainer));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        testPacking.setId(2L);
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testPacking)));
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantityOnPackDetach(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
    }

    @Test
    void calculateAchievedQuantityOnPackDetach5() {
        ContainerPackADInShipmentRequest request = new ContainerPackADInShipmentRequest();
        request.setContainerId(1L);
        request.setPacksId(List.of(2L));
        request.setShipmentId(3L);
        testContainer.setAllocatedVolumeUnit(Constants.VOLUME_UNIT_LITRE);
        testContainer.setAllocatedWeightUnit(Constants.WEIGHT_UNIT_MG);
        when(containerDao.findById(anyLong())).thenReturn(Optional.of(testContainer));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        testPacking.setId(2L);
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testPacking)));
        ResponseEntity<IRunnerResponse> responseEntity = containerService.calculateAchievedQuantityOnPackDetach(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
    }

    @ParameterizedTest
    @ValueSource(strings = {"1234ABCD56", "ABCD12345A", "ABCD12345%", "aBCD12345%", "ABCD123456%", "ABCD123456A", "ABCD1234561"})  // Runs test for both true and false cases
    void testValidateContainerNumber_InvalidCharacters(String invalidCharactersContainerNumber) {
        ResponseEntity<IRunnerResponse> responseEntity = containerService.validateContainerNumber(invalidCharactersContainerNumber);
        Assertions.assertNotNull(responseEntity);
        assertFalse(((ContainerNumberCheckResponse)((RunnerResponse)responseEntity.getBody()).getData()).isSuccess());
    }

    @Test
    void uploadContainers_SEA9() throws Exception{
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(false);
        testContainer.setHazardous(true);
        testContainer.setIsPart(false);
        testContainer.setContainerStuffingLocation("unloc");
        testContainer.setHazardousUn("hzUn");
        testContainer.setCommodityCode("680510");
        testContainer.setHandlingInfo("handlingInfo");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        testContainer.setDgClass("dgClass1");
        testContainer.setContainerCount(1L);
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor<Map> captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    masterDataMap.putAll(jsonTestUtility.getMasterDataMapWithSameCommodity());

                    return List.of(testContainer);
                });
        assertThrows(RuntimeException.class ,() -> containerService.uploadContainers(request));
    }

    @Test
    void uploadContainers_SEA10() throws Exception{
        BulkUploadRequest request = new BulkUploadRequest();
        request.setConsolidationId(1L);
        request.setShipmentId(3L);
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testContainer.setGuid(UUID.randomUUID());
        testContainer.setContainerNumber("CONT0000006");
        testContainer.setIsOwnContainer(true);
        testContainer.setIsShipperOwned(false);
        testContainer.setHazardous(true);
        testContainer.setIsPart(false);
        testContainer.setContainerStuffingLocation("unloc");
        testContainer.setHazardousUn("hzUn");
        testContainer.setCommodityCode("680510");
        testContainer.setHandlingInfo("handlingInfo");
        testContainer.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
        testContainer.setChargeable(new BigDecimal(3453));
        testContainer.setGrossVolume(new BigDecimal(432));
        testContainer.setDgClass("dgClass1");
        testContainer.setContainerCount(1L);
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        when(containerDao.findByConsolidationId(any())).thenReturn(List.of(testContainer));
        ArgumentCaptor<Map> captor = ArgumentCaptor.forClass(Map.class);
        when(parser.parseExcelFile(any(), any(), any(), (Map<String, Set<String>>) captor.capture(), any(), any(), any(), any(), any()))
                .thenAnswer(invocation -> {

                    Map<String, Set<String>> masterDataMap = (Map<String, Set<String>>) captor.getValue();
                    masterDataMap.clear();
                    var a = jsonTestUtility.getMasterDataMapWithSameCommodity();
                    a.remove(MasterDataType.DG_CLASS.getDescription());
                    masterDataMap.putAll(a);

                    return List.of(testContainer);
                });
        when(containerDao.saveAll(any())).thenReturn(List.of(testContainer));
        assertDoesNotThrow(() -> containerService.uploadContainers(request));
    }

}