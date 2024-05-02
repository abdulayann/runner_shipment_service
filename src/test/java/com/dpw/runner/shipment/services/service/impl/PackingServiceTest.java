package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.DetachPacksListDto;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackContainerNumberChangeRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.AutoCalculatePackingResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.ISyncQueueService;
import com.dpw.runner.shipment.services.syncing.Entity.BulkPackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
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
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static org.junit.jupiter.api.Assertions.*;
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
    private static List<Packing> testPackingList;
    private static AutoCalculatePackingRequest testAutoCalculatePackingRequest;
    private static AutoCalculatePackingResponse testAutoCalculatePackingResponse;
    private static PackingRequestV2 testPackingRequestV2;
    private static ConsolidationDetails testConsolidation;

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
        testAutoCalculatePackingRequest = new AutoCalculatePackingRequest();
        testAutoCalculatePackingResponse = new AutoCalculatePackingResponse();
        testAutoCalculatePackingRequest = objectMapperTest.convertValue(testPacking, AutoCalculatePackingRequest.class);
        testAutoCalculatePackingResponse = objectMapperTest.convertValue(testPacking, AutoCalculatePackingResponse.class);
        testPackingList = jsonTestUtility.getTestPackingList();
        testConsolidation = jsonTestUtility.getTestConsolidation();
        testPacking = jsonTestUtility.getTestPacking();
        testContainer = jsonTestUtility.getTestContainer();
        testShipment = jsonTestUtility.getTestShipment();
        testPackingRequestV2 = jsonTestUtility.getTestPackingRequestV2();
        packingRequest = objectMapperTest.convertValue(testPacking, PackingRequest.class);
        packingResponse = objectMapperTest.convertValue(testPacking, PackingResponse.class);
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void create() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(packingRequest);

        ResponseEntity<IRunnerResponse> httpResponse = packingService.create(commonRequestModel);

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

        ResponseEntity<IRunnerResponse> responseEntity = packingService.update(commonRequestModel);
        assertNull(responseEntity);
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
    void testList_Failure() {
        ResponseEntity<IRunnerResponse> httpResponse = packingService.list(CommonRequestModel.buildRequest());
        assertNotNull(httpResponse);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void listAsync() throws ExecutionException, InterruptedException {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().build();
        CommonRequestModel request = CommonRequestModel.buildRequest(listCommonRequest);

        CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntity = packingService.listAsync(request);

        assertNull(responseEntity);
    }

    @Test
    void delete() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Long id = 1L;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(id);

        packingService.delete(commonRequestModel);
    }

    @Test
    void retrieveById() {
        testPacking.setId(1L);
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);

        ResponseEntity<IRunnerResponse> responseEntity = packingService.retrieveById(commonRequestModel);

        assertNull(responseEntity);
    }

    @Test
    void calculateWeightVolumne() throws RunnerException {
        ContainerRequest containerRequest = objectMapperTest.convertValue(testContainer, ContainerRequest.class);
        packingRequest.setShipmentId(1L);
        PackContainerNumberChangeRequest request = PackContainerNumberChangeRequest.builder()
                .newContainer(containerRequest)
                .oldPack(null)
                .newPack(packingRequest)
                .oldContainer(containerRequest).build();

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        when(jsonHelper.convertValue(any(ContainerRequest.class) , eq(Containers.class))).thenReturn(testContainer);
        when(jsonHelper.convertValue(any(), eq(Packing.class))).thenReturn(testPacking);

        packingService.calculateWeightVolumne(CommonRequestModel.builder().data(request).build());

    }

    @Test
    void calculateWeightVolumne_NewPackNull() throws RunnerException {
        ContainerRequest containerRequest = objectMapperTest.convertValue(testContainer, ContainerRequest.class);
        packingRequest.setShipmentId(1L);
        PackContainerNumberChangeRequest request = PackContainerNumberChangeRequest.builder()
                .newContainer(null)
                .oldPack(packingRequest)
                .newPack(null)
                .oldContainer(containerRequest).build();

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        when(jsonHelper.convertValue(any(ContainerRequest.class) , eq(Containers.class))).thenReturn(testContainer);

        packingService.calculateWeightVolumne(CommonRequestModel.builder().data(request).build());

    }

    @Test
    void calculateWeightVolumne_NullOldAndNewCont() throws RunnerException {
        PackContainerNumberChangeRequest request = PackContainerNumberChangeRequest.builder()
                .newContainer(null)
                .oldPack(null)
                .newPack(null)
                .oldContainer(null).build();
        ResponseEntity<IRunnerResponse> responseEntity = packingService.calculateWeightVolumne(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
    }

    @Test
    void testCalculatePackSummary_Success() throws RunnerException {
        List<Packing> packingList = testPackingList;
        PackSummaryResponse packSummaryResponse = packingService.calculatePackSummary(packingList, Constants.TRANSPORT_MODE_SEA, Constants.SHIPMENT_TYPE_LCL, ShipmentMeasurementDetailsDto.builder().build());
        assertNotNull(packSummaryResponse);
        assertEquals(packSummaryResponse, jsonTestUtility.getTestPackSummaryResponse());
    }

    @Test
    void testCalculatePackSummary_AIR_Success() throws RunnerException {
        List<Packing> packingList = testPackingList;
        PackSummaryResponse packSummaryResponse = packingService.calculatePackSummary(packingList, Constants.TRANSPORT_MODE_AIR, Constants.SHIPMENT_TYPE_LCL, ShipmentMeasurementDetailsDto.builder().build());
        assertNotNull(packSummaryResponse);
        assertEquals(packSummaryResponse, jsonTestUtility.getTestPackSummaryAirResponse());
    }

    @Test
    void testCalculateVolumetricWeightForAir_Success() throws RunnerException {
        VolumeWeightChargeable vwObj = packingService.calculateVolumetricWeightForAir(new BigDecimal(1), new BigDecimal(1), Constants.TRANSPORT_MODE_SEA, Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3);
        assertNotNull(vwObj);
        assertEquals(vwObj.getChargeable(), jsonTestUtility.getTestVolWtChargeable().getChargeable());
    }

    @Test
    void testCalculateVolumetricWeightForAirAndChargeable_Success() throws RunnerException {
        testAutoCalculatePackingRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        testAutoCalculatePackingRequest.setContainerCategory(Constants.SHIPMENT_TYPE_LCL);
        CommonRequestModel commonRequest = CommonRequestModel.buildRequest(testAutoCalculatePackingRequest);
        ResponseEntity<IRunnerResponse> response = packingService.calculateVolumetricWeightForAirAndChargeable(commonRequest);
        assertNotNull(response);
    }

    @Test
    void testCalculateVolumetricWeightForAirAndChargeable_SEA_Success() throws RunnerException {
        testAutoCalculatePackingRequest.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testAutoCalculatePackingRequest.setContainerCategory(Constants.SHIPMENT_TYPE_LCL);
        CommonRequestModel commonRequest = CommonRequestModel.buildRequest(testAutoCalculatePackingRequest);
        ResponseEntity<IRunnerResponse> response = packingService.calculateVolumetricWeightForAirAndChargeable(commonRequest);
        assertNotNull(response);
    }

    @Test
    void testCalculateVolume_Success() throws RunnerException {
        packingService.calculateVolume(Constants.M, Constants.M, Constants.M, testAutoCalculatePackingResponse, testAutoCalculatePackingRequest);
        assertNotNull(testAutoCalculatePackingResponse);
    }

    @Test
    void testAutoCalculateVolumetricWeight_Success() {
        CommonRequestModel commonRequest = CommonRequestModel.buildRequest(testAutoCalculatePackingRequest);
        ResponseEntity<IRunnerResponse> response = packingService.autoCalculateVolumetricWeight(commonRequest);
        assertNotNull(response);
    }

    @Test
    void testAutoCalculateChargable_Success() throws RunnerException {
        testAutoCalculatePackingRequest.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testAutoCalculatePackingRequest.setContainerCategory(Constants.SHIPMENT_TYPE_LCL);
        CommonRequestModel commonRequest = CommonRequestModel.buildRequest(testAutoCalculatePackingRequest);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(jsonTestUtility.getTestVolWtChargeable());
        ResponseEntity<IRunnerResponse> response = packingService.autoCalculateChargable(commonRequest);
        assertNotNull(response);
    }

    @Test
    void testAutoCalculateChargable_AIR_Success() throws RunnerException {
        testAutoCalculatePackingRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        testAutoCalculatePackingRequest.setContainerCategory(Constants.SHIPMENT_TYPE_LCL);
        CommonRequestModel commonRequest = CommonRequestModel.buildRequest(testAutoCalculatePackingRequest);
        ResponseEntity<IRunnerResponse> response = packingService.autoCalculateChargable(commonRequest);
        assertNotNull(response);
    }

    @Test
    void testAutoCalculateVolume_Success() throws RunnerException {
        CommonRequestModel commonRequest = CommonRequestModel.buildRequest(testAutoCalculatePackingRequest);
        ResponseEntity<IRunnerResponse> response = packingService.autoCalculateVolume(commonRequest);
        assertNotNull(response);
    }

    @Test
    void testCalculateChargeableForAir_Success() throws RunnerException {
        packingService.calculateChargeableForAir(testAutoCalculatePackingResponse, testAutoCalculatePackingRequest);
        assertNotNull(testAutoCalculatePackingResponse.getChargeable());
    }

    @Test
    void testCalculateChargeableForAir_ChWt_Success() throws RunnerException {
        testAutoCalculatePackingRequest.setWeight(new BigDecimal(9));
        testAutoCalculatePackingRequest.setVolume(new BigDecimal(99999));
        packingService.calculateChargeableForAir(testAutoCalculatePackingResponse, testAutoCalculatePackingRequest);
        assertNotNull(testAutoCalculatePackingResponse.getChargeable());
    }

    @Test
    void testCalculateChargeableForSEA_LCL_Success() throws RunnerException {
        packingService.calculateChargeableForSEA_LCL(testAutoCalculatePackingResponse, testAutoCalculatePackingRequest);
        assertNotNull(testAutoCalculatePackingResponse.getChargeable());
    }

    @Test
    void testListPacksToDetach_Success() throws RunnerException {
        DetachPacksListDto request = DetachPacksListDto.builder().containerId(1L).pageSize(1).shipmentId(1L).pageNo(1).build();
        Page<Packing> page = new PageImpl<>(List.of(testPacking) , PageRequest.of(1 , 1) , 1);
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testPacking)));
        ResponseEntity<IRunnerResponse> responseEntity = packingService.listPacksToDetach(CommonRequestModel.buildRequest(request));
        assertNotNull(responseEntity);
//        assertEquals(ResponseHelper.buildListSuccessResponse(List.of(packingResponse), page.getTotalPages(), page.getTotalElements()), responseEntity);
    }

    @Test
    void testV1PackingCreateAndUpdate_Success() throws RunnerException {
        when(packingDao.findByGuid(any())).thenReturn(Optional.of(testPacking));
        when(modelMapper.map(any(), any())).thenReturn(testPacking);

        ResponseEntity<IRunnerResponse> responseEntity = packingService.V1PackingCreateAndUpdate(CommonRequestModel.buildRequest(testPackingRequestV2), false);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testV1PackingCreateAndUpdate_Success_SyncQueue() throws RunnerException, NoSuchFieldException, IllegalAccessException {
        Field field = SyncConfig.class.getField("IS_REVERSE_SYNC_ACTIVE");
        field.setAccessible(true);
        field.set(syncConfig, false);
        ResponseEntity<IRunnerResponse> responseEntity = new ResponseEntity<>(HttpStatus.OK);
        when(syncQueueService.saveSyncRequest(any(), any(), any())).thenReturn(responseEntity);
        ResponseEntity<IRunnerResponse> response = packingService.V1PackingCreateAndUpdate(CommonRequestModel.buildRequest(testPackingRequestV2), true);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testV1PackingCreateAndUpdate_NewPack_Success() throws RunnerException {
        when(packingDao.findByGuid(any())).thenReturn(Optional.empty());
        when(modelMapper.map(any(), any())).thenReturn(testPacking);
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(testShipment));
        when(consolidationDao.findByGuid(any())).thenReturn(Optional.of(testConsolidation));
        when(packingDao.save(any())).thenReturn(testPacking);
        when(objectMapper.convertValue(any(), eq(PackingResponse.class))).thenReturn(packingResponse);
        ResponseEntity<IRunnerResponse> responseEntity = packingService.V1PackingCreateAndUpdate(CommonRequestModel.buildRequest(testPackingRequestV2), false);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testV1PackingCreateAndUpdate_Failure() throws RunnerException {
        when(packingDao.findByGuid(any())).thenReturn(Optional.empty());
        when(modelMapper.map(any(), any())).thenReturn(testPacking);
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(testShipment));
        when(consolidationDao.findByGuid(any())).thenReturn(Optional.of(testConsolidation));
        when(packingDao.save(any())).thenThrow(new RuntimeException());
        var e  = assertThrows(RuntimeException.class, () -> packingService.V1PackingCreateAndUpdate(CommonRequestModel.buildRequest(testPackingRequestV2), false));
        assertNotNull(e);
    }

    @Test
    void testV1BulkPackingCreateAndUpdate() {
        BulkPackingRequestV2 bulkPackingRequestV2 = BulkPackingRequestV2.builder()
                .bulkPacking(List.of(testPackingRequestV2))
                .ConsolidationId(1L)
                .ShipmentId(1L)
                .build();
        ResponseEntity<IRunnerResponse> responseEntity = packingService.V1BulkPackingCreateAndUpdate(CommonRequestModel.buildRequest(bulkPackingRequestV2));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testV1BulkPackingCreateAndUpdate_Failure() throws RunnerException{
        BulkPackingRequestV2 bulkPackingRequestV2 = BulkPackingRequestV2.builder()
                .bulkPacking(List.of(testPackingRequestV2))
                .ConsolidationId(1L)
                .ShipmentId(1L)
                .build();
        PackingService spyService = spy(packingService);
        doThrow(new RuntimeException()).when(spyService).V1PackingCreateAndUpdate(any(), anyBoolean());
        var e = assertThrows(RuntimeException.class, () -> spyService.V1BulkPackingCreateAndUpdate(CommonRequestModel.buildRequest(bulkPackingRequestV2)));
        assertNotNull(e);
    }

}
