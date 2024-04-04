package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackContainerNumberChangeRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.PackingExcelModel;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.ISyncQueueService;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingSync;
import com.dpw.runner.shipment.services.utils.CSVParsingUtil;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class PackingServiceTest {

    @Mock
    IPackingDao packingDao;

    @Mock
    @Lazy
    private IConsolidationService consolidationService;

    @Mock
    IContainerDao containersDao;
    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IAuditLogService auditLogService;

    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IConsolidationDetailsDao consolidationDao;

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;

    @Mock
    private IPackingSync packingSync;

    @Mock
    private ISyncQueueService syncQueueService;
    @Mock
    private SyncConfig syncConfig;

    @Mock
    private IContainerService containerService;

    @Mock
    private CSVParsingUtil<Packing> parser;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private CommonUtils commonUtils;

    @Mock
    private HttpServletResponse response;
    @InjectMocks
    private PackingService packingService;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;

    private static PackingResponse packingResponse;
    private static Containers testContainer;
    private static Packing testPacking;
    private static PackingRequest packingRequest;
    private static ShipmentDetails testShipment;

    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
            objectMapperTest = JsonTestUtility.getMapper();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUp() {
        testPacking = jsonTestUtility.getTestPacking();
        testContainer = jsonTestUtility.getTestContainer();
        testShipment = jsonTestUtility.getTestShipment();
        packingRequest = objectMapperTest.convertValue(testPacking, PackingRequest.class);
        packingResponse = objectMapperTest.convertValue(testPacking, PackingResponse.class);
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(false).mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void create() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {

        when(jsonHelper.convertValue(any(PackingRequest.class), eq(Packing.class))).thenReturn(testPacking);
        when(jsonHelper.convertValue(any(Packing.class) , eq(PackingResponse.class))).thenReturn(packingResponse);
        when(packingDao.save(any(Packing.class))).thenReturn(testPacking);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(packingRequest);

        ResponseEntity<IRunnerResponse> httpResponse = packingService.create(commonRequestModel);


        verify(auditLogService, times(1)).addAuditLog(any(AuditLogMetaData.class));
        assertEquals(ResponseHelper.buildSuccessResponse(packingResponse), httpResponse);

    }

    @Test
    void downloadPacking() {
        BulkDownloadRequest request = BulkDownloadRequest.builder().consolidationId("12").shipmentId("12").build();
        Page<Packing> page = new PageImpl<>(List.of(testPacking) , PageRequest.of(0 , 10) , 1);

        when(packingDao.findAll(any(), any())).thenReturn(page);
        when(commonUtils.convertToList(any(), eq(PackingExcelModel.class))).thenReturn(List.of(PackingExcelModel.builder().build()));

        Assertions.assertThrows(RunnerException.class, () -> packingService.downloadPacking(response, request));
    }

    @Test
    void update() throws RunnerException {
        packingRequest.setId(12L);
        testPacking.setId(12L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(packingRequest);

        when(packingDao.findById(anyLong())).thenReturn(Optional.of(testPacking));
        when(packingDao.save(any(Packing.class))).thenReturn(testPacking);
        when(jsonHelper.convertValue(any(PackingRequest.class), eq(Packing.class))).thenReturn(testPacking);
        when(jsonHelper.convertValue(any(Packing.class), eq(PackingResponse.class))).thenReturn(packingResponse);

        ResponseEntity<IRunnerResponse> responseEntity = packingService.update(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(packingResponse), responseEntity);
    }

    @Test
    void list() {
        testPacking.setId(1L);
        ListCommonRequest getRequest = ListCommonRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);
        Page<Packing> page = new PageImpl<>(List.of(testPacking) , PageRequest.of(0 , 10) , 1);

        when(packingDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(Packing.class), eq(PackingResponse.class))).thenReturn((PackingResponse) packingResponse);

        ResponseEntity<IRunnerResponse> responseEntity = packingService.list(commonRequestModel);

        assertNotNull(responseEntity);
        assertEquals(ResponseHelper.buildListSuccessResponse(List.of(packingResponse), page.getTotalPages(), page.getTotalElements()), responseEntity);
    }

    @Test
    void listAsync() throws ExecutionException, InterruptedException {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().build();
        CommonRequestModel request = CommonRequestModel.buildRequest(listCommonRequest);
        Page<Packing> page = new PageImpl<>(List.of(testPacking) , PageRequest.of(0 , 10) , 1);

        when(packingDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(Packing.class), eq(PackingResponse.class))).thenReturn((PackingResponse) packingResponse);

        CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntity = packingService.listAsync(request);

        assertNotNull(responseEntity);
        assertEquals(ResponseHelper.buildListSuccessResponse(List.of(packingResponse), page.getTotalPages(), page.getTotalElements()),
                responseEntity.get());
    }

    @Test
    void delete() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Long id = 1L;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(id);

        when(packingDao.findById(id)).thenReturn(Optional.of(testPacking));

        ResponseEntity<IRunnerResponse> responseEntity = packingService.delete(commonRequestModel);

        verify(packingDao, times(1)).delete(testPacking);
        verify(auditLogService, times(1)).addAuditLog(any(AuditLogMetaData.class));
        Assertions.assertEquals(responseEntity.getStatusCodeValue() , HttpStatus.OK.value());
    }

    @Test
    void retrieveById() {
        testPacking.setId(1L);
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);

        when(packingDao.findById(anyLong())).thenReturn(Optional.of(testPacking));
        when(jsonHelper.convertValue(any(Packing.class), eq(PackingResponse.class))).thenReturn(packingResponse);

        ResponseEntity<IRunnerResponse> responseEntity = packingService.retrieveById(commonRequestModel);

        assertNotNull(responseEntity);
        assertEquals(responseEntity, ResponseHelper.buildSuccessResponse(packingResponse));
    }

    @Test
    void calculateWeightVolumne() throws RunnerException {
        ContainerRequest containerRequest = objectMapperTest.convertValue(testContainer, ContainerRequest.class);
        packingRequest.setShipmentId(1L);
        PackContainerNumberChangeRequest request = PackContainerNumberChangeRequest.builder()
                .newContainer(containerRequest)
                .oldPack(null)
                .newPack(packingRequest)
                .oldContainer(null).build();

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        when(jsonHelper.convertValue(any(ContainerRequest.class) , eq(Containers.class))).thenReturn(testContainer);

        packingService.calculateWeightVolumne(CommonRequestModel.builder().data(request).build());

    }

}
