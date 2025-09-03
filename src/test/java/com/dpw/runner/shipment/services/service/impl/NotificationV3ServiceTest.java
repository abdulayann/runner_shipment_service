package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;

import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationDetailsV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.NotificationRequestType;
import com.dpw.runner.shipment.services.exception.exceptions.NotificationServiceException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.http.auth.AuthenticationException;
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

import org.springframework.http.HttpStatus;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class NotificationV3ServiceTest {

    @Mock
    private ModelMapper modelMapper;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private MasterDataKeyUtils masterDataKeyUtils;
    @Mock
    private ExecutorService executorService;
    @Mock
    private INotificationDao notificationDao;
    @Mock
    private IShipmentServiceV3 shipmentService;
    @Mock
    private IConsolidationV3Service consolidationService;
    @Mock
    private V1ServiceUtil v1ServiceUtil;
    @Mock
    private INetworkTransferDao networkTransferDao;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock
    private CommonUtils commonUtils;
    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @InjectMocks
    private NotificationV3Service notificationService;


    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
    private static Notification notification;
    private static ShipmentDetails shipmentDetails;
    private static ConsolidationDetails consolidationDetails;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
    }
    @BeforeEach
    void setUp() {
        notification = jsonTestUtility.getNotification();
        shipmentDetails = jsonTestUtility.getCompleteShipment();
        consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        notificationService.executorService = Executors.newFixedThreadPool(2);
        UserContext.setUser(UsersDto.builder().Username("user").build());
        TenantContext.setCurrentTenant(1);
    }

    @AfterEach
    void tearDown() {
        notificationService.executorService.shutdown();
    }

    @Test
    void testRetrieveByIdWithEmptyRequest() {
        var responseEntity = notificationService.retrieveById(CommonRequestModel.buildRequest());

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithEmptyIdRequest() {
        var responseEntity = notificationService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().build()));

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithIdNotPresent() {
        when(notificationDao.findById(anyLong())).thenReturn(Optional.empty());

        var responseEntity = notificationService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(11L).build()));

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithExceptionInFindById() {
        when(notificationDao.findById(anyLong())).thenThrow(new RuntimeException());

        var responseEntity = notificationService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(11L).build()));

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithSuccessResponse() {
        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        var spyService = Mockito.spy(notificationService);
        when(jsonHelper.convertValue(any(), eq(NotificationResponse.class))).thenReturn(objectMapper.convertValue(notification, NotificationResponse.class));

        var responseEntity = spyService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(11L).build()));

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithGuidWithSuccessResponse() {
        when(notificationDao.findByGuid(any())).thenReturn(Optional.of(notification));
        when(jsonHelper.convertValue(any(), eq(NotificationResponse.class))).thenReturn(objectMapper.convertValue(notification, NotificationResponse.class));

        var responseEntity = notificationService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().guid("893cc8fa-7315-4d23-a635-3ce8705a5140").build()));

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testConfirmationMessage_IdIsNull(){
        Long id= null;
        assertThrows(ValidationException.class, ()->notificationService.confirmationMessage(id));
    }

    @Test
    void testConfirmationMessage_NotificationResponseIsNull() {
        Long id = 11L;
        when(notificationDao.findById(anyLong())).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, ()->notificationService.confirmationMessage(id));
    }

    @Test
    void testConfirmationMessage_ShipmentEntityWithReceivingBranch() throws AuthenticationException, RunnerException {
        Long id = 11L;
        NotificationResponse mockResponse = new NotificationResponse();
        mockResponse.setEntityType(Constants.SHIPMENT);
        mockResponse.setEntityId(10L);
        mockResponse.setRequestedBranchId(1L);
        mockResponse.setReassignedFromBranchId(1);
        mockResponse.setReassignedToBranchId(2L);
        CommonGetRequest request = CommonGetRequest.builder().id(mockResponse.getEntityId()).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(request);
        ShipmentRetrieveLiteResponse shipmentDetailsResponse = objectMapper.convertValue(shipmentDetails, ShipmentRetrieveLiteResponse.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        when(jsonHelper.convertValue(any(), eq(NotificationResponse.class))).thenReturn(mockResponse);
        when(shipmentService.retrieveById(requestModel, false, null)).thenReturn(shipmentDetailsResponse);

        var response = notificationService.confirmationMessage(id);

        assertNotNull(response);
    }

    @Test
    void testConfirmationMessage_ConsolidationEntityWithReceivingBranch() throws AuthenticationException, RunnerException {
        Long id = 11L;
        NotificationResponse mockResponse = new NotificationResponse();
        mockResponse.setEntityType(Constants.CONSOLIDATION);
        mockResponse.setEntityId(10L);
        mockResponse.setRequestedBranchId(1L);
        mockResponse.setReassignedFromBranchId(1);
        mockResponse.setReassignedToBranchId(2L);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        when(jsonHelper.convertValue(any(), eq(NotificationResponse.class))).thenReturn(mockResponse);
        when(consolidationDetailsDao.findById(10L)).thenReturn(Optional.of(consolidationDetails));

        var response = notificationService.confirmationMessage(id);

        assertNotNull(response);
    }

    @Test
    void testConfirmationMessage_ShipmentEntityWithTriangulationPartners() throws AuthenticationException, RunnerException {
        Long id = 11L;
        List<Long> triangulationPartners = List.of(3L);
        NotificationResponse mockResponse = new NotificationResponse();
        mockResponse.setEntityType(Constants.SHIPMENT);
        mockResponse.setEntityId(10L);
        mockResponse.setRequestedBranchId(3L);
        mockResponse.setReassignedFromBranchId(3);
        mockResponse.setReassignedToBranchId(2L);
        CommonGetRequest request = CommonGetRequest.builder().id(mockResponse.getEntityId()).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(request);
        shipmentDetails.setTriangulationPartnerList(
                triangulationPartners.stream().map(partner ->
                        TriangulationPartner.builder().triangulationPartner(partner).build()).toList()
        );
        ShipmentRetrieveLiteResponse shipmentDetailsResponse = objectMapper.convertValue(shipmentDetails, ShipmentRetrieveLiteResponse.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        when(jsonHelper.convertValue(any(), eq(NotificationResponse.class))).thenReturn(mockResponse);
        when(shipmentService.retrieveById(requestModel, false, null)).thenReturn(shipmentDetailsResponse);

        var response = notificationService.confirmationMessage(id);

        assertNotNull(response);
    }

    @Test
    void testConfirmationMessage_ConsolidationEntityWithTriangulationPartners() throws AuthenticationException, RunnerException {
        Long id = 11L;
        List<Long> triangulationPartners = List.of(3L);
        NotificationResponse mockResponse = new NotificationResponse();
        mockResponse.setEntityType(Constants.CONSOLIDATION);
        mockResponse.setEntityId(10L);
        mockResponse.setRequestedBranchId(3L);
        mockResponse.setReassignedFromBranchId(3);
        mockResponse.setReassignedToBranchId(2L);
        consolidationDetails.setTriangulationPartnerList(
                triangulationPartners.stream().map(partner ->
                        TriangulationPartner.builder().triangulationPartner(partner).build()).toList()
        );

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        when(jsonHelper.convertValue(any(), eq(NotificationResponse.class))).thenReturn(mockResponse);
        when(consolidationDetailsDao.findById(10L)).thenReturn(Optional.of(consolidationDetails));

        var response = notificationService.confirmationMessage(id);

        assertNotNull(response);
    }

    @Test
    void testConfirmationMessage_InputMismatchException() throws AuthenticationException, RunnerException {
        Long id = 11L;
        NotificationResponse mockResponse = new NotificationResponse();
        mockResponse.setEntityType(Constants.SHIPMENT);
        mockResponse.setEntityId(10L);
        mockResponse.setRequestedBranchId(5L);
        mockResponse.setReassignedToBranchId(7L);
        CommonGetRequest request = CommonGetRequest.builder().id(mockResponse.getEntityId()).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(request);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        when(jsonHelper.convertValue(any(), eq(NotificationResponse.class))).thenReturn(mockResponse);
        ShipmentRetrieveLiteResponse shipmentDetailsResponse = objectMapper.convertValue(shipmentDetails, ShipmentRetrieveLiteResponse.class);
        when(shipmentService.retrieveById(requestModel, false, null)).thenReturn(shipmentDetailsResponse);

        assertThrows(NullPointerException.class, ()->notificationService.confirmationMessage(id));
    }

    @Test
    void testAcceptNotification_IdIsNull() {
        Long id = null;
        assertThrows(ValidationException.class, ()->notificationService.acceptNotification(id));
    }

    @Test
    void testAcceptNotification_NotificationResponseIsNull() {
        Long id = 11L;
        when(notificationDao.findById(anyLong())).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, ()->notificationService.acceptNotification(id));
    }

    @Test
    void testAcceptNotification_ShipmentEntity_ReassignBranch() throws RunnerException, AuthenticationException {
        Long id = 11L;
        notification.setNotificationRequestType(NotificationRequestType.REASSIGN);
        notification.setEntityType(Constants.SHIPMENT);
        notification.setEntityId(10L);
        notification.setRequestedBranchId(1);
        notification.setReassignedFromBranchId(1);
        notification.setReassignedToBranchId(2);
        Integer tenantId = 2;
        TenantModel tenantModel = new TenantModel();
        tenantModel.setDefaultOrgId(100L);
        tenantModel.setDefaultAddressId(200L);
        PartiesRequest partiesRequest = new PartiesRequest();

        CommonGetRequest request = CommonGetRequest.builder().id(notification.getEntityId()).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(request);
        ShipmentRetrieveLiteResponse shipmentDetailsResponse = objectMapper.convertValue(shipmentDetails, ShipmentRetrieveLiteResponse.class);
        ShipmentV3Request shipmentRequest = objectMapper.convertValue(shipmentDetails, ShipmentV3Request.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        when(shipmentService.retrieveById(requestModel, false, null)).thenReturn(shipmentDetailsResponse);
        when(jsonHelper.convertValue(shipmentDetailsResponse, ShipmentV3Request.class)).thenReturn(shipmentRequest);
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(Map.of(tenantId, new Object()));
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(v1ServiceUtil.getPartiesRequestFromOrgIdAndAddressId(100L, 200L)).thenReturn(partiesRequest);
        when(shipmentService.completeUpdate(any(CommonRequestModel.class))).thenReturn(ShipmentDetailsV3Response.builder().build());
        doNothing().when(notificationDao).delete(notification);

        assertDoesNotThrow(()->notificationService.acceptNotification(id));
        verify(notificationDao).delete(notification);
        verify(notificationDao).findById(id);
    }

    @Test
    void testAcceptNotification_ShipmentEntity_ReassignBranch_ThrowsRunnerException() throws RunnerException, AuthenticationException {
        Long id = 11L;
        notification.setNotificationRequestType(NotificationRequestType.REASSIGN);
        notification.setEntityType(Constants.SHIPMENT);
        notification.setEntityId(10L);
        notification.setRequestedBranchId(1);
        notification.setReassignedFromBranchId(1);
        notification.setReassignedToBranchId(2);
        Integer tenantId = 2;
        TenantModel tenantModel = new TenantModel();
        tenantModel.setDefaultOrgId(100L);
        tenantModel.setDefaultAddressId(200L);
        PartiesRequest partiesRequest = new PartiesRequest();

        CommonGetRequest request = CommonGetRequest.builder().id(notification.getEntityId()).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(request);
        ShipmentRetrieveLiteResponse shipmentDetailsResponse = objectMapper.convertValue(shipmentDetails, ShipmentRetrieveLiteResponse.class);
        ShipmentV3Request shipmentRequest = objectMapper.convertValue(shipmentDetails, ShipmentV3Request.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        when(shipmentService.retrieveById(requestModel, false, null)).thenReturn(shipmentDetailsResponse);
        when(jsonHelper.convertValue(shipmentDetailsResponse, ShipmentV3Request.class)).thenReturn(shipmentRequest);
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(Map.of(tenantId, new Object()));
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(v1ServiceUtil.getPartiesRequestFromOrgIdAndAddressId(100L, 200L)).thenReturn(partiesRequest);
        when(shipmentService.completeUpdate(any(CommonRequestModel.class))).thenThrow(new RunnerException());

        assertThrows(NotificationServiceException.class, ()->notificationService.acceptNotification(id));
        verify(notificationDao).findById(id);
    }

    @Test
    void testAcceptNotification_ShipmentEntity_ReassignBranch_ThrowsException() throws RunnerException, AuthenticationException {
        Long id = 11L;
        notification.setNotificationRequestType(NotificationRequestType.REASSIGN);
        notification.setEntityType(Constants.SHIPMENT);
        notification.setEntityId(10L);
        notification.setRequestedBranchId(1);
        notification.setReassignedFromBranchId(1);
        notification.setReassignedToBranchId(2);
        Integer tenantId = 2;
        TenantModel tenantModel = new TenantModel();
        tenantModel.setDefaultOrgId(100L);
        tenantModel.setDefaultAddressId(200L);
        PartiesRequest partiesRequest = new PartiesRequest();

        CommonGetRequest request = CommonGetRequest.builder().id(notification.getEntityId()).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(request);
        ShipmentRetrieveLiteResponse shipmentDetailsResponse = objectMapper.convertValue(shipmentDetails, ShipmentRetrieveLiteResponse.class);
        ShipmentV3Request shipmentRequest = objectMapper.convertValue(shipmentDetails, ShipmentV3Request.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        when(shipmentService.retrieveById(requestModel, false, null)).thenReturn(shipmentDetailsResponse);
        when(jsonHelper.convertValue(shipmentDetailsResponse, ShipmentV3Request.class)).thenReturn(shipmentRequest);
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(Map.of(tenantId, new Object()));
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(v1ServiceUtil.getPartiesRequestFromOrgIdAndAddressId(100L, 200L)).thenReturn(partiesRequest);
        when(shipmentService.completeUpdate(any(CommonRequestModel.class))).thenThrow(new RuntimeException("Error"));

        assertThrows(RuntimeException.class, ()->notificationService.acceptNotification(id));
        verify(notificationDao).findById(id);
    }

    @Test
    void testAcceptNotification_ConsolidationEntity_ReassignBranch() throws RunnerException {
        Long id = 11L;
        notification.setNotificationRequestType(NotificationRequestType.REASSIGN);
        notification.setEntityType(Constants.CONSOLIDATION);
        notification.setEntityId(10L);
        notification.setRequestedBranchId(1);
        notification.setReassignedFromBranchId(1);
        notification.setReassignedToBranchId(2);
        Integer tenantId = 2;
        TenantModel tenantModel = new TenantModel();
        tenantModel.setDefaultOrgId(100L);
        tenantModel.setDefaultAddressId(200L);
        PartiesRequest partiesRequest = new PartiesRequest();
        ConsolidationDetailsV3Request consolidationDetailsRequest = objectMapper.convertValue(consolidationDetails, ConsolidationDetailsV3Request.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        when(consolidationDetailsDao.findById(10L)).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsV3Request.class)).thenReturn(consolidationDetailsRequest);
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(Map.of(tenantId, new Object()));
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(v1ServiceUtil.getPartiesRequestFromOrgIdAndAddressId(100L, 200L)).thenReturn(partiesRequest);
        when(consolidationService.completeUpdate(any(ConsolidationDetailsV3Request.class))).thenReturn(ConsolidationDetailsV3Response.builder().build());
        doNothing().when(notificationDao).delete(notification);

        assertDoesNotThrow(()->notificationService.acceptNotification(id));
        verify(notificationDao).delete(notification);
        verify(notificationDao).findById(id);
    }

    @Test
    void testAcceptNotification_ShipmentEntity_TriangulationPartnerBranch() throws RunnerException, AuthenticationException {
        Long id = 11L;
        List<Long> triangulationPartners = List.of(3L);
        notification.setNotificationRequestType(NotificationRequestType.REASSIGN);
        notification.setEntityType(Constants.SHIPMENT);
        notification.setEntityId(10L);
        notification.setRequestedBranchId(3);
        notification.setReassignedFromBranchId(3);
        notification.setReassignedToBranchId(2);

        CommonGetRequest request = CommonGetRequest.builder().id(notification.getEntityId()).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(request);
        shipmentDetails.setTriangulationPartnerList(
                triangulationPartners.stream().map(partner ->
                        TriangulationPartner.builder().triangulationPartner(partner).build()).toList()
        );
        ShipmentRetrieveLiteResponse shipmentDetailsResponse = objectMapper.convertValue(shipmentDetails, ShipmentRetrieveLiteResponse.class);
        ShipmentV3Request shipmentRequest = objectMapper.convertValue(shipmentDetails, ShipmentV3Request.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        when(shipmentService.retrieveById(requestModel, false, null)).thenReturn(shipmentDetailsResponse);
        when(jsonHelper.convertValue(shipmentDetailsResponse, ShipmentV3Request.class)).thenReturn(shipmentRequest);
        when(shipmentService.completeUpdate(any(CommonRequestModel.class))).thenReturn(ShipmentDetailsV3Response.builder().build());
        doNothing().when(notificationDao).delete(notification);

        assertDoesNotThrow(()->notificationService.acceptNotification(id));
        verify(notificationDao).delete(notification);
        verify(notificationDao).findById(id);
    }

    @Test
    void testAcceptNotification_ConsolidationEntity_TriangulationPartnerBranch() throws RunnerException {
        Long id = 11L;
        List<Long> triangulationPartners = List.of(3L);
        notification.setNotificationRequestType(NotificationRequestType.REASSIGN);
        notification.setEntityType(Constants.CONSOLIDATION);
        notification.setEntityId(10L);
        notification.setRequestedBranchId(3);
        notification.setReassignedFromBranchId(3);
        notification.setReassignedToBranchId(2);

        consolidationDetails.setTriangulationPartnerList(
                triangulationPartners.stream().map(partner ->
                        TriangulationPartner.builder().triangulationPartner(partner).build()).toList()
        );

        ConsolidationDetailsV3Request consolidationDetailsRequest = objectMapper.convertValue(consolidationDetails, ConsolidationDetailsV3Request.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        when(consolidationDetailsDao.findById(10L)).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsV3Request.class)).thenReturn(consolidationDetailsRequest);
        when(consolidationService.completeUpdate(any(ConsolidationDetailsV3Request.class))).thenReturn(ConsolidationDetailsV3Response.builder().build());
        doNothing().when(notificationDao).delete(notification);

        assertDoesNotThrow(()->notificationService.acceptNotification(id));

        verify(notificationDao).delete(notification);
        verify(notificationDao).findById(id);
    }

    @Test
    void testGetPartiesRequestFromTenantDefaultOrg_Success() {
        Integer tenantId = 1;
        TenantModel tenantModel = new TenantModel();
        tenantModel.setDefaultOrgId(100L);
        tenantModel.setDefaultAddressId(200L);
        PartiesRequest partiesRequest = new PartiesRequest();

        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(Map.of(tenantId, new Object()));
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(v1ServiceUtil.getPartiesRequestFromOrgIdAndAddressId(100L, 200L)).thenReturn(partiesRequest);

        var result = notificationService.getPartiesRequestFromTenantDefaultOrg(tenantId);

        assertNotNull(result);
        assertEquals(partiesRequest, result);
        verify(v1ServiceUtil).getTenantDetails(anyList());
        verify(jsonHelper).convertValue(any(), eq(TenantModel.class));
        verify(v1ServiceUtil).getPartiesRequestFromOrgIdAndAddressId(100L, 200L);
    }

    @Test
    void testGetPartiesRequestFromTenantDefaultOrg_NoTenantDetails() {
        Integer tenantId = 1;
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(Map.of());

        var result = notificationService.getPartiesRequestFromTenantDefaultOrg(tenantId);

        assertNull(result);
        verify(v1ServiceUtil).getTenantDetails(anyList());
        verifyNoInteractions(jsonHelper);
    }

    @Test
    void testGetPartiesRequestFromTenantDefaultOrg_NullOrgOrAddress() {
        Integer tenantId = 1;
        TenantModel tenantModel = new TenantModel();
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(Map.of(tenantId, new Object()));
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(tenantModel);

        var result = notificationService.getPartiesRequestFromTenantDefaultOrg(tenantId);

        assertNull(result);
        verify(v1ServiceUtil).getTenantDetails(anyList());
        verify(jsonHelper).convertValue(any(), eq(TenantModel.class));
        verifyNoMoreInteractions(v1ServiceUtil);
    }

    @Test
    void testGetReassignType_Error() {
        List<Long> triangulationPartners = List.of(3L, 4L);
        assertThrows(InputMismatchException.class, () -> notificationService.getReassignType(1L, 2L, triangulationPartners));
    }

    @Test
    void testProcessTriangulationPartners_EmptyList() {
        var result = notificationService.processTriangulationPartners(Collections.emptyList(), 1L, 2L);

        assertEquals(Collections.emptyList(), result);
    }

}