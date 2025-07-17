package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dto.request.BulkUpdateRoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.BulkRoutingResponse;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.NetworkTransferV3Util;
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
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Executors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
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
    private IConsolidationV3Service consolidationV3Service;
    @Mock
    private ICarrierDetailsDao carrierDetailsDao;
    @Mock
    private DependentServiceHelper dependentServiceHelper;
    @Mock
    private NetworkTransferV3Util networkTransferV3Util;
    @InjectMocks
    private RoutingsV3Service routingsService;

    private Routings routings;
    private RoutingsRequest routingsRequest;

    private static JsonTestUtility jsonTestUtility;

    @BeforeAll
    static void init() {
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
        when(jsonHelper.convertValue(any(), eq(Routings.class))).thenReturn(routings);
        when(routingsDao.save(routings)).thenReturn(routings);
        when(jsonHelper.convertValue(any(), eq(RoutingsResponse.class))).thenReturn(response);
        when(shipmentServiceV3.findById(any())).thenReturn(Optional.of(shipmentDetails));
        var resp = routingsService.create(commonRequestModel, Constants.SHIPMENT);
        assertEquals(response.getId(), resp.getId());
    }

    @Test
    void testCreate_NullRequestError() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        assertThrows(RunnerException.class, () -> routingsService.create(commonRequestModel, Constants.SHIPMENT));
    }

    @Test
    void testCreate_DaoSaveError() {
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
        when(routingsDao.findById(anyLong())).thenReturn(Optional.of(oldEntity));
        when(jsonHelper.convertValue(any(), eq(Routings.class))).thenReturn(oldEntity);
        when(jsonHelper.convertValue(routingsRequest, Routings.class)).thenReturn(routings);
        when(routingsDao.save(routings)).thenReturn(routings);
        when(jsonHelper.convertValue(any(), eq(RoutingsResponse.class))).thenReturn(response);
        when(shipmentServiceV3.findById(any())).thenReturn(Optional.of(shipmentDetails));
        var resp = routingsService.update(commonRequestModel, Constants.SHIPMENT);
        assertEquals(response.getId(), resp.getId());
    }

    @Test
    void testUpdate_DaoError() {
        routingsRequest.setShipmentId(1L);
        routingsRequest.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(routingsRequest);
        when(routingsDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> routingsService.update(commonRequestModel, Constants.SHIPMENT));
    }

    @Test
    void testUpdate_DaoSaveError() {
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
        doThrow(new RuntimeException("Error")).when(routingsDao).delete(routings);
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
        routingsRequest.setId(2L);
        routings.setId(2L);
        routings.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routings.setVesselName("vessel");
        routings.setVoyage("0123");
        routingsRequest.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routingsRequest.setIsSelectedForDocument(Boolean.TRUE);
        RoutingsRequest routingsRequest1 = new RoutingsRequest();
        routingsRequest1.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routingsRequest1.setIsSelectedForDocument(Boolean.TRUE);
        Routings routings1 = new Routings();
        routings1.setIsSelectedForDocument(Boolean.TRUE);
        routings1.setId(2l);
        routings1.setVesselName("vessel");
        routings1.setVoyage("0123");
        routings1.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routings1.setPod("DEIMETA");
        routings1.setDestinationPortLocCode("destportLoc");
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(new CarrierDetails())
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .build();
        List<RoutingsRequest> requestList = List.of(routingsRequest, routingsRequest1);
        BulkUpdateRoutingsRequest bulkUpdateRoutingsRequest = new BulkUpdateRoutingsRequest();
        bulkUpdateRoutingsRequest.setRoutings(requestList);
        bulkUpdateRoutingsRequest.setTransportInfoStatus(TransportInfoStatus.YES);
        RoutingsResponse response = RoutingsResponse.builder().id(2L).build();

        when(routingsDao.findByIdIn(anyList())).thenReturn(List.of(routings, routings1));
        when(jsonHelper.convertValueToList(anyList(), eq(Routings.class))).thenReturn(List.of(routings, routings1));
        when(routingsDao.saveAll(anyList())).thenReturn(List.of(routings, routings1));
        when(shipmentServiceV3.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(jsonHelper.convertValueToList(anyList(), eq(RoutingsResponse.class))).thenReturn(List.of(response));
        doNothing().when(auditLogService).addAuditLog(any());

        BulkRoutingResponse result = routingsService.updateBulk(bulkUpdateRoutingsRequest, Constants.SHIPMENT);

        assertNotNull(result.getRoutingsResponseList());
        assertEquals(1, result.getRoutingsResponseList().size());
        verify(auditLogService, atLeastOnce()).addAuditLog(any());
    }
    @Test
    void testUpdateBulkAir_success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        routingsRequest.setId(2L);
        routings.setId(2L);
        routings.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routings.setCarrier("carrier");
        routings.setFlightNumber("0123");
        routingsRequest.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routingsRequest.setIsSelectedForDocument(Boolean.TRUE);
        RoutingsRequest routingsRequest1 = new RoutingsRequest();
        routingsRequest1.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routingsRequest1.setIsSelectedForDocument(Boolean.TRUE);
        Routings routings1 = new Routings();
        routings1.setIsSelectedForDocument(Boolean.TRUE);
        routings1.setId(2l);
        routings1.setCarrier("carr");
        routings1.setFlightNumber("0123");
        routings1.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routings1.setPod("DEIMETA");
        routings1.setDestinationPortLocCode("destportLoc");
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setIsSameAsDestinationPort(true);
        carrierDetails.setIsSameAsOriginPort(true);
        carrierDetails.setDestinationPort("DIEMATA");
        carrierDetails.setDestinationLocCode("PORT_CODE");
        carrierDetails.setDestinationPortCountry("ind");
        carrierDetails.setOriginPort("DIEMATA");
        carrierDetails.setOriginPortLocCode("PORT_CODE");
        carrierDetails.setOriginPortCountry("ind");

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(carrierDetails)
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .build();
        List<RoutingsRequest> requestList = List.of(routingsRequest, routingsRequest1);
        BulkUpdateRoutingsRequest bulkUpdateRoutingsRequest = new BulkUpdateRoutingsRequest();
        bulkUpdateRoutingsRequest.setRoutings(requestList);
        bulkUpdateRoutingsRequest.setTransportInfoStatus(TransportInfoStatus.YES);
        RoutingsResponse response = RoutingsResponse.builder().id(2L).build();

        when(routingsDao.findByIdIn(anyList())).thenReturn(List.of(routings, routings1));
        when(jsonHelper.convertValueToList(anyList(), eq(Routings.class))).thenReturn(List.of(routings, routings1));
        when(routingsDao.saveAll(anyList())).thenReturn(List.of(routings, routings1));
        when(shipmentServiceV3.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(jsonHelper.convertValueToList(anyList(), eq(RoutingsResponse.class))).thenReturn(List.of(response));
        doNothing().when(auditLogService).addAuditLog(any());

        BulkRoutingResponse result = routingsService.updateBulk(bulkUpdateRoutingsRequest, Constants.SHIPMENT);

        assertNotNull(result.getRoutingsResponseList());
        assertEquals(1, result.getRoutingsResponseList().size());
        verify(auditLogService, atLeastOnce()).addAuditLog(any());
    }

    @Test
    void testUpdateBulk_success_deleteMain() throws RunnerException {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(new CarrierDetails())
                .build();
        shipmentDetails.setId(1l);
        List<RoutingsRequest> requestList = Collections.emptyList();
        BulkUpdateRoutingsRequest bulkUpdateRoutingsRequest = new BulkUpdateRoutingsRequest();
        bulkUpdateRoutingsRequest.setRoutings(requestList);
        bulkUpdateRoutingsRequest.setEntityId(1l);
        RoutingsResponse response = RoutingsResponse.builder().id(2L).build();

        when(jsonHelper.convertValueToList(anyList(), eq(Routings.class))).thenReturn(Collections.emptyList());
        when(routingsDao.saveAll(anyList())).thenReturn(Collections.emptyList());
        when(shipmentServiceV3.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(jsonHelper.convertValueToList(anyList(), eq(RoutingsResponse.class))).thenReturn(List.of(response));

        BulkRoutingResponse result = routingsService.updateBulk(bulkUpdateRoutingsRequest, Constants.SHIPMENT);

        assertNotNull(result.getRoutingsResponseList());
        assertEquals(1, result.getRoutingsResponseList().size());
    }
    @Test
    void testConsolidationUpdateBulk_success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        routingsRequest.setId(2L);
        routings.setId(2L);
        routings.setConsolidationId(1L);
        routings.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routings.setIsSelectedForDocument(true);
        Routings routings1 = new Routings();
        routings1.setId(3l);
        routings1.setConsolidationId(1L);
        routings1.setCarriage(RoutingCarriage.PRE_CARRIAGE);

        Routings routings2 = new Routings();
        routings2.setId(3l);
        routings2.setConsolidationId(1L);
        routings2.setCarriage(RoutingCarriage.ON_CARRIAGE);

        routingsRequest.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(new CarrierDetails())
                .routingsList(List.of(routings, routings1, routings2))
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .build();
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder()
                .carrierDetails(new CarrierDetails())
                .shipmentsList(Set.of(shipmentDetails))
                .routingsList(List.of(routings, routings1, routings2))
                .build();
        List<RoutingsRequest> requestList = List.of(routingsRequest);
        BulkUpdateRoutingsRequest bulkUpdateRoutingsRequest = new BulkUpdateRoutingsRequest();
        bulkUpdateRoutingsRequest.setRoutings(requestList);
        bulkUpdateRoutingsRequest.setTransportInfoStatus(TransportInfoStatus.YES);
        RoutingsResponse response = RoutingsResponse.builder().id(2L).build();

        when(routingsDao.findByIdIn(anyList())).thenReturn(List.of(routings));
        when(jsonHelper.convertValueToList(anyList(), eq(Routings.class))).thenReturn(List.of(routings, routings1, routings2));
        when(routingsDao.saveAll(anyList())).thenReturn(List.of(routings));
        when(consolidationV3Service.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(consolidationV3Service.getConsolidationById(anyLong())).thenReturn(consolidationDetails);
        when(shipmentServiceV3.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(jsonHelper.convertValueToList(anyList(), eq(RoutingsResponse.class))).thenReturn(List.of(response));
        doNothing().when(auditLogService).addAuditLog(any());
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });

        BulkRoutingResponse result = routingsService.updateBulk(bulkUpdateRoutingsRequest, Constants.CONSOLIDATION);

        assertNotNull(result.getRoutingsResponseList());
        assertEquals(1, result.getRoutingsResponseList().size());
        verify(auditLogService, atLeastOnce()).addAuditLog(any());
    }

    @Test
    void deleteInheritedRoutingsFromShipment_success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        routingsRequest.setId(2L);
        routings.setId(2L);
        routings.setConsolidationId(1L);
        routings.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routings.setInheritedFromConsolidation(Boolean.TRUE);
        Routings routings1 = new Routings();
        routings1.setId(3l);
        routings1.setConsolidationId(1L);
        routings1.setCarriage(RoutingCarriage.PRE_CARRIAGE);

        Routings routings2 = new Routings();
        routings2.setId(3l);
        routings2.setConsolidationId(1L);
        routings2.setCarriage(RoutingCarriage.ON_CARRIAGE);

        routingsRequest.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        List<Routings> routingsList = new ArrayList<>();
        routingsList.add(routings);
        routingsList.add(routings1);
        routingsList.add(routings2);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(new CarrierDetails())
                .routingsList(routingsList)
                .build();
        List<RoutingsRequest> requestList = List.of(routingsRequest);
        BulkUpdateRoutingsRequest bulkUpdateRoutingsRequest = new BulkUpdateRoutingsRequest();
        bulkUpdateRoutingsRequest.setRoutings(requestList);

        when(routingsDao.saveAll(anyList())).thenReturn(List.of(routings));
        when(shipmentServiceV3.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(auditLogService).addAuditLog(any());

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(shipmentDetails);
        routingsService.deleteInheritedRoutingsFromShipment(shipmentDetailsList);

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
    void testConsolidationUpdateBulk_success_deleteMain() throws RunnerException {
        routingsRequest.setId(2L);
        routingsRequest.setEntityId(1l);
        Routings routings1 = new Routings();
        routings1.setId(3l);
        routings1.setConsolidationId(1L);
        routings1.setCarriage(RoutingCarriage.PRE_CARRIAGE);

        Routings routings2 = new Routings();
        routings2.setId(3l);
        routings2.setConsolidationId(1L);
        routings2.setCarriage(RoutingCarriage.ON_CARRIAGE);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(new CarrierDetails())
                .routingsList(List.of(routings1, routings2))
                .build();
        shipmentDetails.setId(1l);
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder()
                .carrierDetails(new CarrierDetails())
                .shipmentsList(Set.of(shipmentDetails))
                .routingsList(List.of(routings1, routings2))
                .build();
        List<RoutingsRequest> requestList = List.of(routingsRequest);
        BulkUpdateRoutingsRequest bulkUpdateRoutingsRequest = new BulkUpdateRoutingsRequest();
        bulkUpdateRoutingsRequest.setRoutings(requestList);
        bulkUpdateRoutingsRequest.setEntityId(1l);
        RoutingsResponse response = RoutingsResponse.builder().id(2L).build();

        when(routingsDao.findByIdIn(anyList())).thenReturn(List.of(routings));
        when(jsonHelper.convertValueToList(anyList(), eq(Routings.class))).thenReturn(List.of(routings1, routings2));
        when(routingsDao.saveAll(anyList())).thenReturn(List.of(routings));
        when(consolidationV3Service.getConsolidationById(anyLong())).thenReturn(consolidationDetails);
        when(shipmentServiceV3.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(jsonHelper.convertValueToList(anyList(), eq(RoutingsResponse.class))).thenReturn(List.of(response));

        BulkRoutingResponse result = routingsService.updateBulk(bulkUpdateRoutingsRequest, Constants.CONSOLIDATION);

        assertNotNull(result.getRoutingsResponseList());
        assertEquals(1, result.getRoutingsResponseList().size());
    }

    @Test
    void testList_Success() throws RunnerException {
        ListCommonRequest request = ListCommonRequest.builder().build();
        RoutingsResponse routingsResponse = RoutingsResponse.builder().id(2L).build();

        Page<Routings> page = new PageImpl<>(List.of(routings));
        when(routingsDao.findAll(any(), any())).thenReturn(page);
        when(modelMapper.map(any(), eq(RoutingsResponse.class))).thenReturn(routingsResponse);

        var response = routingsService.list(request, Constants.SHIPMENT);
        assertEquals(1, response.getRoutings().size());
        assertEquals(1, response.getTotalCount());
    }


    @Test
    void testList_RequestNull() {
        assertThrows(RunnerException.class, () -> routingsService.list(null, Constants.SHIPMENT));
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

    @Test
    void testGetMasterDataForList_success() {
        List<RoutingsResponse> mockResponse = List.of(new RoutingsResponse());
        // Stub the MDC wrapper to return the original runnable
        Mockito.lenient().when(masterDataUtils.withMdc(Mockito.any())).thenAnswer(invocation -> invocation.getArgument(0));

        // Act
        Map<String, Object> result = routingsService.getMasterDataForList(mockResponse);

        // Assert
        assertNotNull(result);
        // We can also verify that methods were called
        verify(routingV3Util).addAllUnlocationInSingleCallList(any(), any());
        verify(routingV3Util).addAllMasterDataInSingleCallList(any(), any());
        verify(routingV3Util).addAllVesselInSingleCallList(any(), any());
    }
    private CarrierDetails carrierDetailsWithDates(LocalDateTime eta, LocalDateTime etd, LocalDateTime ata, LocalDateTime atd) {
        CarrierDetails details = new CarrierDetails();
        details.setEta(eta);
        details.setEtd(etd);
        details.setAta(ata);
        details.setAtd(atd);
        return details;
    }

    @Test
    void testOldNullNewAllNullDates_returnsFalse() {
        CarrierDetails newDetails = carrierDetailsWithDates(null, null, null, null);
        boolean result = routingsService.isValidDateChange(newDetails, null);
        assertFalse(result);
    }

    @Test
    void testOldNullNewWithEta_returnsTrue() {
        CarrierDetails newDetails = carrierDetailsWithDates(LocalDateTime.now(), null, null, null);
        boolean result = routingsService.isValidDateChange(newDetails, null);
        assertTrue(result);
    }

    @Test
    void testBothNotNullDatesSame_returnsFalse() {
        LocalDateTime now = LocalDateTime.now();
        CarrierDetails oldDetails = carrierDetailsWithDates(now, now, now, now);
        CarrierDetails newDetails = carrierDetailsWithDates(now, now, now, now);

        boolean result = routingsService.isValidDateChange(newDetails, oldDetails);
        assertFalse(result);
    }

    @Test
    void testBothNotNullEtaChanged_returnsTrue() {
        LocalDateTime now = LocalDateTime.now();
        CarrierDetails oldDetails = carrierDetailsWithDates(now, now, now, now);
        CarrierDetails newDetails = carrierDetailsWithDates(now.plusDays(1), now, now, now);

        boolean result = routingsService.isValidDateChange(newDetails, oldDetails);
        assertTrue(result);
    }

    @Test
    void testNewNullOldNotNull_returnsFalse() {
        CarrierDetails oldDetails = carrierDetailsWithDates(LocalDateTime.now(), null, null, null);
        boolean result = routingsService.isValidDateChange(null, oldDetails);
        assertFalse(result);
    }
    private Routings routing(Boolean inherited) {
        Routings r = new Routings();
        r.setInheritedFromConsolidation(inherited);
        return r;
    }

    @Test
    void testLastIndexNegative_shouldAddToResult() {
        List<Routings> result = new ArrayList<>();
        List<Routings> newToAppendAtEnd = new ArrayList<>();
        List<Routings> buffer = List.of(routing(false));
        Routings current = routing(false);

        routingsService.setMainCarriageInheritedIndexPosition(result, newToAppendAtEnd, buffer, current, -1, true);

        assertEquals(1, result.size());
        assertTrue(newToAppendAtEnd.isEmpty());
    }

    @Test
    void testDifferentInheritedFlags_shouldAddToResult() {
        List<Routings> result = new ArrayList<>(List.of(routing(false)));
        List<Routings> newToAppendAtEnd = new ArrayList<>();
        List<Routings> buffer = List.of(routing(false));
        Routings current = routing(true);

        routingsService.setMainCarriageInheritedIndexPosition(result, newToAppendAtEnd, buffer, current, 0, true);

        assertEquals(2, result.size());
        assertTrue(newToAppendAtEnd.isEmpty());
    }

    @Test
    void testBothInheritedTrue_shouldAddToNewToAppendAtEnd() {
        List<Routings> result = new ArrayList<>(List.of(routing(true)));
        List<Routings> newToAppendAtEnd = new ArrayList<>();
        List<Routings> buffer = List.of(routing(false));
        Routings current = routing(true);

        routingsService.setMainCarriageInheritedIndexPosition(result, newToAppendAtEnd, buffer, current, 0, true);

        assertEquals(1, result.size());
        assertEquals(1, newToAppendAtEnd.size());
    }

    @Test
    void testCanInsertFalse_shouldAddToNewToAppendAtEnd() {
        List<Routings> result = new ArrayList<>(List.of(routing(true)));
        List<Routings> newToAppendAtEnd = new ArrayList<>();
        List<Routings> buffer = List.of(routing(false));
        Routings current = routing(true);

        routingsService.setMainCarriageInheritedIndexPosition(result, newToAppendAtEnd, buffer, current, 0, false);

        assertEquals(1, result.size());
        assertEquals(1, newToAppendAtEnd.size());
    }
}