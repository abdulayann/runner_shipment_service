package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.BulkRoutingResponse;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.RoutingValidationUtil;
import com.dpw.runner.shipment.services.utils.v3.RoutingV3Util;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.Executors;

import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.any;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class RoutingsV3ServiceTest extends CommonMocks {

    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IRoutingsDao routingsDao;
    @Mock
    private AuditLogService auditLogService;
    @Mock
    private RoutingValidationUtil routingValidationUtil;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private RoutingV3Util routingV3Util;
    @Mock
    private IShipmentServiceV3 shipmentServiceV3;
    @Mock
    private ICarrierDetailsDao carrierDetailsDao;
    @InjectMocks
    private RoutingsV3Service routingsService;

    private Routings routings;
    private RoutingsRequest routingsRequest;

    private static JsonTestUtility jsonTestUtility;

    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @AfterEach
    void tearDown() {
        routingsService.executorServiceMasterData.shutdown();
    }
    @BeforeEach
    void setUp() {
        routingsRequest = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .leg(1L)
                .mode(Constants.TRANSPORT_MODE_AIR)
                .build();
        routings = jsonTestUtility.getTestRouting();
        routingsService.executorServiceMasterData = Executors.newFixedThreadPool(2);
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().enableRouteMaster(true).build());
    }



    @Test
    void testCreate_Success() throws RunnerException {
        routingsRequest.setShipmentId(1L);
        RoutingsResponse response = RoutingsResponse.builder()
                .id(2L)
                .build();
        routings.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(routingsRequest);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(new CarrierDetails())
                .build();
        mockShipmentSettings();
        when(jsonHelper.convertValue(any(), eq(Routings.class))).thenReturn(routings);
        when(routingsDao.save(routings)).thenReturn(routings);
        when(jsonHelper.convertValue(any(), eq(RoutingsResponse.class))).thenReturn(response);
        when(shipmentServiceV3.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(routingsDao.findByShipmentIdAndCarriage(any(), any())).thenReturn(List.of(new Routings()));
        var resp = routingsService.create(commonRequestModel, Constants.SHIPMENT);
        assertEquals(response.getId(), resp.getId());
    }

    @Test
    void testCreate_NullRequestError(){
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        assertThrows(RunnerException.class, () -> routingsService.create(commonRequestModel, Constants.SHIPMENT));
    }

    @Test
    void testCreate_DaoSaveError(){
        routingsRequest.setShipmentId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(routingsRequest);
        when(jsonHelper.convertValue(any(), eq(Routings.class))).thenReturn(routings);
        when(routingsDao.save(routings)).thenThrow(new RuntimeException("Error"));
        assertThrows(RunnerException.class, () -> routingsService.create(commonRequestModel, Constants.SHIPMENT));
    }

    @Test
    void testUpdate_Success() throws RunnerException {
        routingsRequest.setShipmentId(1L);
        routingsRequest.setId(1L);
        routingsRequest.setGuid(UUID.randomUUID());
        RoutingsResponse response = RoutingsResponse.builder()
                .id(1L)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(routingsRequest);
        Routings oldEntity = Routings.builder().build();
        routings.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(new CarrierDetails())
                .build();
        mockShipmentSettings();

        when(routingsDao.findById(anyLong())).thenReturn(Optional.of(oldEntity));
        when(jsonHelper.convertValue(any(), eq(Routings.class))).thenReturn(oldEntity);
        when(jsonHelper.convertValue(routingsRequest, Routings.class)).thenReturn(routings);
        when(routingsDao.save(routings)).thenReturn(routings);
        when(jsonHelper.convertValue(any(), eq(RoutingsResponse.class))).thenReturn(response);
        when(shipmentServiceV3.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(routingsDao.findByShipmentIdAndCarriage(any(), any())).thenReturn(List.of(new Routings()));
        var resp = routingsService.update(commonRequestModel, Constants.SHIPMENT);
        assertEquals(response.getId(), resp.getId());
    }

    @Test
    void testUpdate_DaoError(){
        routingsRequest.setShipmentId(1L);
        routingsRequest.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(routingsRequest);
        when(routingsDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> routingsService.update(commonRequestModel, Constants.SHIPMENT));
    }

    @Test
    void testUpdate_DaoSaveError(){
        routingsRequest.setShipmentId(1L);
        routingsRequest.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(routingsRequest);
        when(jsonHelper.convertValue(any(), eq(Routings.class))).thenReturn(routings);
        when(routingsDao.findById(anyLong())).thenReturn(Optional.of(Routings.builder().build()));
        when(routingsDao.save(routings)).thenThrow(new RuntimeException("Error"));
        assertThrows(RunnerException.class, () -> routingsService.update(commonRequestModel, Constants.SHIPMENT));
    }

    @Test
    void testDelete_Success() throws RunnerException {
        routings.setId(1L);
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        when(routingsDao.findById(anyLong())).thenReturn(Optional.of(routings));
        when(jsonHelper.convertValue(any(), eq(Routings.class))).thenReturn(routings);
        routingsService.delete(commonRequestModel, Constants.SHIPMENT);
        verify(routingsDao).delete(routings);
    }

    @Test
    void testDelete_DaoError() {
        routings.setId(1L);
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        when(routingsDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> routingsService.delete(commonRequestModel, Constants.SHIPMENT));
    }

    @Test
    void testDelete_DaoSaveError() {
        routings.setId(1L);
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        when(routingsDao.findById(anyLong())).thenReturn(Optional.of(routings));
        when(jsonHelper.convertValue(any(), eq(Routings.class))).thenReturn(routings);
        Mockito.doThrow(new RuntimeException("Error")).when(routingsDao).delete(routings);
        assertThrows(RunnerException.class, () -> routingsService.delete(commonRequestModel, Constants.SHIPMENT));
    }

    @Test
    void testRetrieveById_Success() throws RunnerException {
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        RoutingsResponse response = RoutingsResponse.builder()
                .id(1L)
                .build();

        when(routingsDao.findById(anyLong())).thenReturn(Optional.of(routings));
        when(jsonHelper.convertValue(any(), eq(RoutingsResponse.class))).thenReturn(response);
        var resp = routingsService.retrieveById(commonRequestModel, Constants.SHIPMENT);
        assertEquals(response.getId(), resp.getId());
    }

    @Test
    void testRetrieveById_IdNotFound() {
        CommonGetRequest request = CommonGetRequest.builder().id(null).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        assertThrows(RunnerException.class, () -> routingsService.retrieveById(commonRequestModel, Constants.SHIPMENT));
    }

    @Test
    void testRetrieveById_DaoError() {
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(routingsDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> routingsService.retrieveById(commonRequestModel, Constants.SHIPMENT));
    }

    @Test
    void testUpdateBulk_success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        RoutingsRequest newRequest = new RoutingsRequest();
        routingsRequest.setId(2L);
        routings.setId(2L);
        List<RoutingsRequest> requestList = List.of(routingsRequest, newRequest);
        RoutingsResponse response = RoutingsResponse.builder().id(2L).build();

        when(routingsDao.findByIdIn(anyList())).thenReturn(List.of(routings));
        when(jsonHelper.convertValueToList(anyList(), eq(Routings.class))).thenReturn(List.of(routings));
        when(routingsDao.saveAll(anyList())).thenReturn(List.of(routings));
        when(jsonHelper.convertValueToList(anyList(), eq(RoutingsResponse.class))).thenReturn(List.of(response));
        doNothing().when(auditLogService).addAuditLog(any());

        BulkRoutingResponse result = routingsService.updateBulk(requestList, Constants.SHIPMENT);

        assertNotNull(result.getRoutingsResponseList());
        assertEquals(1, result.getRoutingsResponseList().size());
        verify(auditLogService, atLeastOnce()).addAuditLog(any());
    }

    @Test
    void testDeleteBulk_success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        routingsRequest.setId(2L);
        routings.setId(2L);
        when(routingsDao.findByIdIn(anyList())).thenReturn(List.of(routings));
        doNothing().when(auditLogService).addAuditLog(any());

        BulkRoutingResponse result = routingsService.deleteBulk(List.of(routingsRequest), Constants.SHIPMENT);

        assertTrue(result.getMessage().contains("deleted successfully"));
        verify(routingsDao).deleteByIdIn(anyList());
        verify(auditLogService).addAuditLog(any());
    }

    @Test
    void testList_Success() throws RunnerException {
        ListCommonRequest request = ListCommonRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        RoutingsResponse routingsResponse = RoutingsResponse.builder().id(2L).build();

        Page<Routings> page = new PageImpl<>(List.of(routings));
        when(routingsDao.findAll(any(), any())).thenReturn(page);
        when(modelMapper.map(any(), eq(RoutingsResponse.class))).thenReturn(routingsResponse);

        var response = routingsService.list(commonRequestModel, Constants.SHIPMENT);
        assertEquals(1, response.getRoutings().size());
        assertEquals(1, response.getTotalCount());
    }


    @Test
    void testList_RequestNull() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        assertThrows(RunnerException.class, () -> routingsService.list(commonRequestModel, Constants.SHIPMENT));
    }

    @Test
    void testGetAllMasterData_Success() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(2L);
        RoutingsResponse routingsResponse = RoutingsResponse.builder().id(2L).build();

        when(routingsDao.findById(anyLong())).thenReturn(Optional.of(routings));
        when(jsonHelper.convertValue(any(), eq(RoutingsResponse.class))).thenReturn(routingsResponse);
        routingsService.getAllMasterData(commonRequestModel, Constants.SHIPMENT);
        verify(routingsDao).findById(anyLong());
    }

    @Test
    void testGetAllMasterData_DaoFailure() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(2L);
        when(routingsDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> routingsService.getAllMasterData(commonRequestModel, Constants.SHIPMENT));
    }
}