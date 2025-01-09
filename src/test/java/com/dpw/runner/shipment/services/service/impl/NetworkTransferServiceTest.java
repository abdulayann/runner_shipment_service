package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.ReassignRequest;
import com.dpw.runner.shipment.services.dto.request.RequestForTransferRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.NetworkTransferListResponse;
import com.dpw.runner.shipment.services.dto.response.NetworkTransferResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.CommonMocks;
import com.fasterxml.jackson.databind.ObjectMapper;
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
import org.springframework.http.HttpStatus;

import java.io.IOException;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class NetworkTransferServiceTest extends CommonMocks{

    @Mock
    private ModelMapper modelMapper;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private INetworkTransferDao networkTransferDao;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private MasterDataKeyUtils masterDataKeyUtils;
    @InjectMocks
    private NetworkTransferService networkTransferService;
    @Mock
    private ExecutorService executorService;
    @Mock
    private INotificationDao notificationDao;
    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;


    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
    private static NetworkTransfer networkTransfer;
    private static ShipmentDetails shipmentDetails;
    private static ConsolidationDetails consolidationDetails;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
    }
    @BeforeEach
    void setUp() {
        networkTransfer = jsonTestUtility.getNetworkTransfer();
        shipmentDetails = jsonTestUtility.getTestShipment();
        consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        networkTransferService.executorService = Executors.newFixedThreadPool(2);
        UserContext.setUser(UsersDto.builder().Username("user").build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isNetworkTransferEntityEnabled(Boolean.TRUE).build());
        TenantContext.setCurrentTenant(1);
    }

    @AfterEach
    void tearDown() {
        networkTransferService.executorService.shutdown();
    }

    @Test
    void requestForTransfer() {
        RequestForTransferRequest requestForTransferRequest = RequestForTransferRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(requestForTransferRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(NetworkTransfer.builder().build()));
        when(notificationDao.save(any(Notification.class))).thenReturn(new Notification());
        var response = networkTransferService.requestForTransfer(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void requestForTransfer_DataRetrievalFailure() {
        RequestForTransferRequest requestForTransferRequest = RequestForTransferRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(requestForTransferRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> networkTransferService.requestForTransfer(request));
    }

    @Test
    void requestForTransfer_Already_REQUESTED_TO_TRANSFER() {
        RequestForTransferRequest requestForTransferRequest = RequestForTransferRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(requestForTransferRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(NetworkTransfer.builder().status(NetworkTransferStatus.REQUESTED_TO_TRANSFER).build()));
        assertThrows(DataRetrievalFailureException.class, () -> networkTransferService.requestForTransfer(request));
    }

    @Test
    void requestForReassign() {
        ReassignRequest reassignRequest = ReassignRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(reassignRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(NetworkTransfer.builder().build()));
        when(notificationDao.save(any(Notification.class))).thenReturn(new Notification());
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        mockShipmentSettings();
        var response = networkTransferService.requestForReassign(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void requestForReassign_DataRetrievalFailure() {
        ReassignRequest reassignRequest = ReassignRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(reassignRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.empty());
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        mockShipmentSettings();
        assertThrows(DataRetrievalFailureException.class, () -> networkTransferService.requestForReassign(request));
    }

    @Test
    void requestForReassign_DataRetrievalFailure2() {
        ReassignRequest reassignRequest = ReassignRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(reassignRequest).build();
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(0);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> networkTransferService.requestForReassign(request));
    }

    @Test
    void requestForReassign_DataRetrievalFailure3() {
        ReassignRequest reassignRequest = ReassignRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(reassignRequest).build();
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(null);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> networkTransferService.requestForReassign(request));
    }

    @Test
    void requestForReassign_Already_REASSIGNED() {
        ReassignRequest reassignRequest = ReassignRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(reassignRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(NetworkTransfer.builder().status(NetworkTransferStatus.REASSIGNED).build()));
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        mockShipmentSettings();
        assertThrows(DataRetrievalFailureException.class, () -> networkTransferService.requestForReassign(request));
    }

    @Test
    void testListWithEmptyRequest() {
        // Test
        var responseEntity = networkTransferService.list(CommonRequestModel.buildRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListWithEmptyException() {
        when(networkTransferDao.findAll(any(), any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = networkTransferService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListWithNoResult() {
        when(networkTransferDao.findAll(any(), any())).thenReturn(Page.empty());
        // Test
        var responseEntity = networkTransferService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertTrue(((RunnerListResponse) Objects.requireNonNull(responseEntity.getBody())).getData().isEmpty());
    }

    @Test
    void testListWithSuccessResult() {
        when(networkTransferDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(networkTransfer)));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        when(modelMapper.map(any(), eq(NetworkTransferListResponse.class))).thenReturn(objectMapper.convertValue(networkTransfer, NetworkTransferListResponse.class));
        // Test
        var responseEntity = networkTransferService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertFalse(((RunnerListResponse) Objects.requireNonNull(responseEntity.getBody())).getData().isEmpty());
    }

    @Test
    void testRetrieveByIdWithEmptyRequest() {
        var responseEntity = networkTransferService.retrieveById(CommonRequestModel.buildRequest());
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithEmptyIdRequest() {
        var responseEntity = networkTransferService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithIdNotPresent() {
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.empty());
        var responseEntity = networkTransferService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithExceptionInFindById() {
        when(networkTransferDao.findById(anyLong())).thenThrow(new RuntimeException());
        var responseEntity = networkTransferService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithSuccessResponse() {
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(networkTransfer));
        Runnable mockRunnable = mock(Runnable.class);
        // Define the behavior of the mock
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        var spyService = Mockito.spy(networkTransferService);
        when(jsonHelper.convertValue(any(), eq(NetworkTransferResponse.class))).thenReturn(objectMapper.convertValue(networkTransfer, NetworkTransferResponse.class));
        var responseEntity = spyService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithGuidWithSuccessResponse() {
        when(networkTransferDao.findByGuid(any())).thenReturn(Optional.of(networkTransfer));
        when(jsonHelper.convertValue(any(), eq(NetworkTransferResponse.class))).thenReturn(objectMapper.convertValue(networkTransfer, NetworkTransferResponse.class));
        var responseEntity = networkTransferService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().guid("1d27fe99-0874-4587-9a83-460bb5ba31f0").build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCreateNetworkTransferEntityWithShipment(){
        when(networkTransferDao.save(any())).thenReturn(networkTransfer);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(123L, null,
                Constants.SHIPMENT, shipmentDetails, null, Constants.SHIPMENT_TYPE_DRT, null));
    }

    @Test
    void testCreateNetworkTransferEntityWithEmptyShipment(){
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(123L, null,
                Constants.SHIPMENT, null, null, Constants.SHIPMENT_TYPE_DRT, null));

    }

    @Test
    void testCreateNetworkTransferEntityWithNewAndOldShipment(){
        when(networkTransferDao.save(any())).thenReturn(networkTransfer);
        when(networkTransferDao.findByTenantAndEntity(any(),any(), any())).thenReturn(Optional.of(networkTransfer));
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setId(1L);
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(123L, 132L,
                Constants.SHIPMENT, shipmentDetails, null, Constants.SHIPMENT_TYPE_DRT, null));

    }

    @Test
    void testCreateNetworkTransferEntityWithNewAndOldShipmentSame(){
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setId(1L);
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(123L, 123L,
                Constants.SHIPMENT, shipmentDetails, null, Constants.SHIPMENT_TYPE_DRT, null));

    }

    @Test
    void testCreateNetworkTransferEntityWithOldShipment(){
        when(networkTransferDao.findByTenantAndEntity(any(),any(), any())).thenReturn(Optional.of(networkTransfer));
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setId(1L);
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(null, 132L,
                Constants.SHIPMENT, shipmentDetails, null, Constants.DIRECTION_EXP, null));
    }

    @Test
    void testCreateNetworkTransferEntityWitSomeOtherEntity(){
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setId(1L);
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(null, 132L,
                Constants.Shipment, shipmentDetails, null, Constants.DIRECTION_EXP, null));
    }

    @Test
    void testCreateNetworkTransferEntityFailure(){
        when(networkTransferDao.save(any())).thenThrow(new RuntimeException("Error"));
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        assertThrows(RuntimeException.class, () -> networkTransferService.processNetworkTransferEntity(123L, null,
                Constants.SHIPMENT, shipmentDetails, null, Constants.DIRECTION_EXP, null));
    }

    @Test
    void testCreateNetworkTransferEntityWithConsolidation(){
        when(networkTransferDao.save(any())).thenReturn(networkTransfer);
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(123L, null,
                Constants.CONSOLIDATION, null, consolidationDetails, Constants.DIRECTION_CTS, null));
    }

    @Test
    void testCreateNetworkTransferEntityWithEmptyConsolidation(){
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(123L, null,
                Constants.CONSOLIDATION, null, null, Constants.DIRECTION_CTS, null));
    }

    @Test
    void testCreateNetworkTransferEntityWithOldConsolidation(){
        when(networkTransferDao.findByTenantAndEntity(any(),any(), any())).thenReturn(Optional.of(networkTransfer));
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(null, 132L,
                Constants.CONSOLIDATION, null, consolidationDetails, Constants.DIRECTION_EXP, null));
    }

    @Test
    void testCreateNetworkTransferEntityDBFindFailure(){
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenThrow(new RuntimeException("Connection Time out"));
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        assertThrows(RuntimeException.class, () -> networkTransferService.processNetworkTransferEntity(123L, 321L,
                Constants.SHIPMENT, shipmentDetails, null, Constants.DIRECTION_EXP, null));
    }

    @Test
    void testCreateNetworkTransferEntityDBDeleteFailure(){
        when(networkTransferDao.findByTenantAndEntity(any(),any(), any())).thenReturn(Optional.of(networkTransfer));
        doThrow(new RuntimeException("Connection Time out")).when(networkTransferDao).deleteAndLog(any(), any());
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        assertThrows(RuntimeException.class, () -> networkTransferService.processNetworkTransferEntity(123L, 321L,
                Constants.SHIPMENT, shipmentDetails, null, Constants.DIRECTION_EXP, null));
    }

    @Test
    void testUpdateNetworkTransferTransferred(){
        when(networkTransferDao.save(any())).thenReturn(networkTransfer);
        assertDoesNotThrow(() -> networkTransferService.updateNetworkTransferTransferred(networkTransfer, null));
    }

    @Test
    void testDeleteValidNetworkTransferEntity(){
        when(networkTransferDao.findByTenantAndEntity(938, 12146L, Constants.SHIPMENT)).thenReturn(Optional.of(networkTransfer));
        assertDoesNotThrow(() -> networkTransferService.deleteValidNetworkTransferEntity(938L, 12146L, Constants.SHIPMENT));
    }

    @Test
    void testDeleteValidNetworkTransferEntity2(){
        when(networkTransferDao.findByTenantAndEntity(938, 12146L, Constants.SHIPMENT)).thenReturn(Optional.empty());
        assertDoesNotThrow(() -> networkTransferService.deleteValidNetworkTransferEntity(938L, 12146L, Constants.SHIPMENT));
    }

    @Test
    void testDeleteValidNetworkTransferEntity3(){
        networkTransfer.setStatus(NetworkTransferStatus.ACCEPTED);
        when(networkTransferDao.findByTenantAndEntity(938, 12146L, Constants.SHIPMENT)).thenReturn(Optional.of(networkTransfer));
        assertDoesNotThrow(() -> networkTransferService.deleteValidNetworkTransferEntity(938L, 12146L, Constants.SHIPMENT));
    }

    @Test
    void testDeleteValidNetworkTransferEntityException(){
        when(networkTransferDao.findByTenantAndEntity(938, 12146L, Constants.SHIPMENT)).thenReturn(Optional.of(networkTransfer));
        doThrow(new RuntimeException("Connection Time out")).when(networkTransferDao).deleteAndLog(any(), any());
        assertDoesNotThrow(() -> networkTransferService.deleteValidNetworkTransferEntity(938L, 12146L, Constants.SHIPMENT));
    }

}