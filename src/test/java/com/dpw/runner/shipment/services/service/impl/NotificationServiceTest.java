package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.dto.DeclineNotificationRequest;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.NotificationRequestType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class NotificationServiceTest {

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
    private IShipmentService shipmentService;
    @Mock
    private IConsolidationService consolidationService;
    @Mock
    private V1ServiceUtil v1ServiceUtil;
    @Mock
    private INetworkTransferDao networkTransferDao;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock
    private CommonUtils commonUtils;

    @InjectMocks
    private NotificationService notificationService;


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
    void testListWithEmptyRequest() {
        var responseEntity = notificationService.list(CommonRequestModel.buildRequest());

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListWithEmptyException() {
        when(notificationDao.findAll(any(), any())).thenThrow(new RuntimeException());

        var responseEntity = notificationService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListWithNoResult() {
        when(notificationDao.findAll(any(), any())).thenReturn(Page.empty());

        var responseEntity = notificationService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertTrue(((RunnerListResponse) Objects.requireNonNull(responseEntity.getBody())).getData().isEmpty());
    }

    @Test
    void testListWithSuccessResult() {
        when(notificationDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(notification)));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        when(modelMapper.map(any(), eq(NotificationListResponse.class))).thenReturn(objectMapper.convertValue(notification, NotificationListResponse.class));

        var responseEntity = notificationService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertFalse(((RunnerListResponse) Objects.requireNonNull(responseEntity.getBody())).getData().isEmpty());
    }

    @Test
    void testListWithErrorFromWithMdc() {
        when(notificationDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(notification)));
        when(modelMapper.map(any(), eq(NotificationListResponse.class))).thenReturn(objectMapper.convertValue(notification, NotificationListResponse.class));
        when(masterDataUtils.withMdc(any(Runnable.class))).thenThrow(new RuntimeException("Simulated exception"));

        var responseEntity = notificationService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
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
    void testConfirmationMessage_IdIsNull() {
        var response = notificationService.confirmationMessage(null);

        assertNotNull(response);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testConfirmationMessage_NotificationResponseIsNull() {
        Long id = 11L;
        when(notificationDao.findById(anyLong())).thenReturn(Optional.empty());

        var response = notificationService.confirmationMessage(id);

        assertNotNull(response);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testConfirmationMessage_ShipmentEntityWithReceivingBranch() {
        Long id = 11L;
        NotificationResponse mockResponse = new NotificationResponse();
        mockResponse.setEntityType(Constants.SHIPMENT);
        mockResponse.setEntityId(10L);
        mockResponse.setRequestedBranchId(1L);
        mockResponse.setReassignedFromBranchId(1);
        mockResponse.setReassignedToBranchId(2L);
        CommonGetRequest request = CommonGetRequest.builder().id(mockResponse.getEntityId()).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(request);
        ShipmentDetailsResponse shipmentDetailsResponse = objectMapper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        when(jsonHelper.convertValue(any(), eq(NotificationResponse.class))).thenReturn(mockResponse);
        when(shipmentService.retrieveById(requestModel)).thenReturn(getResponse(shipmentDetailsResponse, HttpStatus.OK));

        var response = notificationService.confirmationMessage(id);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testConfirmationMessage_ConsolidationEntityWithReceivingBranch() {
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
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testConfirmationMessage_ShipmentEntityWithTriangulationPartners() {
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
        ShipmentDetailsResponse shipmentDetailsResponse = objectMapper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        when(jsonHelper.convertValue(any(), eq(NotificationResponse.class))).thenReturn(mockResponse);
        when(shipmentService.retrieveById(requestModel)).thenReturn(getResponse(shipmentDetailsResponse, HttpStatus.OK));

        var response = notificationService.confirmationMessage(id);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testConfirmationMessage_ConsolidationEntityWithTriangulationPartners() {
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
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testConfirmationMessage_InputMismatchException() {
        Long id = 11L;
        NotificationResponse mockResponse = new NotificationResponse();
        mockResponse.setEntityType(Constants.SHIPMENT);
        mockResponse.setEntityId(10L);
        mockResponse.setRequestedBranchId(5L);
        mockResponse.setReassignedToBranchId(7L);
        CommonGetRequest request = CommonGetRequest.builder().id(mockResponse.getEntityId()).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(request);
        ShipmentDetailsResponse shipmentDetailsResponse = objectMapper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        when(jsonHelper.convertValue(any(), eq(NotificationResponse.class))).thenReturn(mockResponse);
        when(shipmentService.retrieveById(requestModel)).thenReturn(getResponse(shipmentDetailsResponse, HttpStatus.OK));

        var response = notificationService.confirmationMessage(id);

        assertNotNull(response);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testAcceptNotification_IdIsNull() {
        var response = notificationService.acceptNotification(null);

        assertNotNull(response);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testAcceptNotification_NotificationResponseIsNull() {
        Long id = 11L;
        when(notificationDao.findById(anyLong())).thenReturn(Optional.empty());

        var response = notificationService.acceptNotification(id);

        assertNotNull(response);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testAcceptNotification_ShipmentEntity_ReassignBranch() throws RunnerException {
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
        ShipmentDetailsResponse shipmentDetailsResponse = objectMapper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
        ShipmentRequest shipmentRequest = objectMapper.convertValue(shipmentDetails, ShipmentRequest.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        when(shipmentService.retrieveById(requestModel)).thenReturn(getResponse(shipmentDetailsResponse, HttpStatus.OK));
        when(jsonHelper.convertValue(shipmentDetailsResponse, ShipmentRequest.class)).thenReturn(shipmentRequest);
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(Map.of(tenantId, new Object()));
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(v1ServiceUtil.getPartiesRequestFromOrgIdAndAddressId(100L, 200L)).thenReturn(partiesRequest);
        when(shipmentService.completeUpdate(any(CommonRequestModel.class))).thenReturn(getResponse(shipmentDetailsResponse, HttpStatus.OK));
        doNothing().when(notificationDao).delete(notification);

        var response = notificationService.acceptNotification(id);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(notificationDao).delete(notification);
        verify(notificationDao).findById(id);
    }

    @Test
    void testAcceptNotification_ShipmentEntity_ReassignBranch_ThrowsRunnerException() throws RunnerException {
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
        ShipmentDetailsResponse shipmentDetailsResponse = objectMapper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
        ShipmentRequest shipmentRequest = objectMapper.convertValue(shipmentDetails, ShipmentRequest.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        when(shipmentService.retrieveById(requestModel)).thenReturn(getResponse(shipmentDetailsResponse, HttpStatus.OK));
        when(jsonHelper.convertValue(shipmentDetailsResponse, ShipmentRequest.class)).thenReturn(shipmentRequest);
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(Map.of(tenantId, new Object()));
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(v1ServiceUtil.getPartiesRequestFromOrgIdAndAddressId(100L, 200L)).thenReturn(partiesRequest);
        when(shipmentService.completeUpdate(any(CommonRequestModel.class))).thenThrow(new RunnerException());

        var response = notificationService.acceptNotification(id);

        assertNotNull(response);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        verify(notificationDao).findById(id);
    }

    @Test
    void testAcceptNotification_ShipmentEntity_ReassignBranch_ThrowsException() throws RunnerException {
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
        ShipmentDetailsResponse shipmentDetailsResponse = objectMapper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
        ShipmentRequest shipmentRequest = objectMapper.convertValue(shipmentDetails, ShipmentRequest.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        when(shipmentService.retrieveById(requestModel)).thenReturn(getResponse(shipmentDetailsResponse, HttpStatus.OK));
        when(jsonHelper.convertValue(shipmentDetailsResponse, ShipmentRequest.class)).thenReturn(shipmentRequest);
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(Map.of(tenantId, new Object()));
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(v1ServiceUtil.getPartiesRequestFromOrgIdAndAddressId(100L, 200L)).thenReturn(partiesRequest);
        when(shipmentService.completeUpdate(any(CommonRequestModel.class))).thenThrow(new RuntimeException("Error"));

        var response = notificationService.acceptNotification(id);

        assertNotNull(response);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
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

        ConsolidationDetailsResponse consolidationDetailsResponse = objectMapper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
        ConsolidationDetailsRequest consolidationDetailsRequest = objectMapper.convertValue(consolidationDetails, ConsolidationDetailsRequest.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        when(consolidationDetailsDao.findById(10L)).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsRequest.class)).thenReturn(consolidationDetailsRequest);
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(Map.of(tenantId, new Object()));
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(v1ServiceUtil.getPartiesRequestFromOrgIdAndAddressId(100L, 200L)).thenReturn(partiesRequest);
        when(consolidationService.completeUpdate(any(CommonRequestModel.class))).thenReturn(getResponse(consolidationDetailsResponse, HttpStatus.OK));
        doNothing().when(notificationDao).delete(notification);

        var response = notificationService.acceptNotification(id);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(notificationDao).delete(notification);
        verify(notificationDao).findById(id);
    }

    @Test
    void testAcceptNotification_ShipmentEntity_TriangulationPartnerBranch() throws RunnerException {
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
        ShipmentDetailsResponse shipmentDetailsResponse = objectMapper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
        ShipmentRequest shipmentRequest = objectMapper.convertValue(shipmentDetails, ShipmentRequest.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        when(shipmentService.retrieveById(requestModel)).thenReturn(getResponse(shipmentDetailsResponse, HttpStatus.OK));
        when(jsonHelper.convertValue(shipmentDetailsResponse, ShipmentRequest.class)).thenReturn(shipmentRequest);
        when(shipmentService.completeUpdate(any(CommonRequestModel.class))).thenReturn(getResponse(shipmentDetailsResponse, HttpStatus.OK));
        doNothing().when(notificationDao).delete(notification);

        var response = notificationService.acceptNotification(id);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
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
        ConsolidationDetailsResponse consolidationDetailsResponse = objectMapper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
        ConsolidationDetailsRequest consolidationDetailsRequest = objectMapper.convertValue(consolidationDetails, ConsolidationDetailsRequest.class);

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification));
        when(consolidationDetailsDao.findById(10L)).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsRequest.class)).thenReturn(consolidationDetailsRequest);
        when(consolidationService.completeUpdate(any(CommonRequestModel.class))).thenReturn(getResponse(consolidationDetailsResponse, HttpStatus.OK));
        doNothing().when(notificationDao).delete(notification);

        var response = notificationService.acceptNotification(id);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
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

    public ResponseEntity<IRunnerResponse> getResponse(IRunnerResponse data, HttpStatus status) {
        return new ResponseEntity<>(RunnerResponse.builder().success(true).data(data).build(), status);
    }

    @Test
    void testRejectNotification_Shipment_reassign() {
        DeclineNotificationRequest declineNotificationRequest = DeclineNotificationRequest.builder().id(1L).reason("test").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(declineNotificationRequest);
        Notification notification1 = Notification.builder()
                .entityId(1L)
                .entityType(Constants.SHIPMENT)
                .notificationRequestType(NotificationRequestType.REASSIGN)
                .requestedBranchId(2)
                .requestedUser("abc")
                .reassignedToBranchId(3)
                .build();
        NetworkTransfer networkTransfer = NetworkTransfer.builder()
                .entityNumber("SHP163834")
                .build();
        Map<Integer, Object> tenantModelMap = new HashMap<>(Map.of(1, new Object()));
        tenantModelMap.put(3, new Object());
        UserContext.getUser().setDisplayName("xyz");
        UserContext.getUser().setEmail("xyz@xyz.com");

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification1));
        when(networkTransferDao.findByTenantAndEntity(any(), anyLong(), anyString())).thenReturn(Optional.of(networkTransfer));
        doAnswer(invocation -> {
            Map<String, String> mapArg = invocation.getArgument(1);
            mapArg.put("abc", "abc@example.com");
            return null;
        }).when(commonUtils).getUserDetails(eq(Set.of("abc")), anyMap());
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(tenantModelMap);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(new TenantModel());
        var response = notificationService.rejectNotification(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());

    }

    @Test
    void testRejectNotification_Consolidation_reassign() {
        DeclineNotificationRequest declineNotificationRequest = DeclineNotificationRequest.builder().id(1L).reason("test").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(declineNotificationRequest);
        Notification notification1 = Notification.builder()
                .entityId(1L)
                .entityType(Constants.CONSOLIDATION)
                .notificationRequestType(NotificationRequestType.REASSIGN)
                .requestedBranchId(2)
                .requestedUser("abc")
                .reassignedToBranchId(3)
                .build();
        NetworkTransfer networkTransfer = NetworkTransfer.builder()
                .entityNumber("SHP163834")
                .build();
        Map<Integer, Object> tenantModelMap = new HashMap<>(Map.of(1, new Object()));
        tenantModelMap.put(3, new Object());
        UserContext.getUser().setDisplayName("xyz");
        UserContext.getUser().setEmail("xyz@xyz.com");

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification1));
        when(networkTransferDao.findByTenantAndEntity(any(), anyLong(), anyString())).thenReturn(Optional.of(networkTransfer));
        doAnswer(invocation -> {
            Map<String, String> mapArg = invocation.getArgument(1);
            mapArg.put("abc", "abc@example.com");
            return null;
        }).when(commonUtils).getUserDetails(eq(Set.of("abc")), anyMap());
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(tenantModelMap);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(new TenantModel());
        var response = notificationService.rejectNotification(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());

    }

    @Test
    void testRejectNotification_Shipment_requestTransfer() {
        DeclineNotificationRequest declineNotificationRequest = DeclineNotificationRequest.builder().id(1L).reason("test").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(declineNotificationRequest);
        Notification notification1 = Notification.builder()
                .entityId(1L)
                .entityType(Constants.SHIPMENT)
                .notificationRequestType(NotificationRequestType.REQUEST_TRANSFER)
                .requestedBranchId(2)
                .requestedUser("abc")
                .reassignedToBranchId(3)
                .build();
        NetworkTransfer networkTransfer = NetworkTransfer.builder()
                .entityNumber("SHP163834")
                .build();
        Map<Integer, Object> tenantModelMap = new HashMap<>(Map.of(1, new Object()));
        tenantModelMap.put(3, new Object());
        UserContext.getUser().setDisplayName("xyz");
        UserContext.getUser().setEmail("xyz@xyz.com");

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification1));
        when(networkTransferDao.findByTenantAndEntity(any(), anyLong(), anyString())).thenReturn(Optional.of(networkTransfer));
        doAnswer(invocation -> {
            Map<String, String> mapArg = invocation.getArgument(1);
            mapArg.put("abc", "abc@example.com");
            return null;
        }).when(commonUtils).getUserDetails(eq(Set.of("abc")), anyMap());
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isNteAdditionalEmailsEnabled(true).build());
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(tenantModelMap);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(new TenantModel());
        var response = notificationService.rejectNotification(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());

    }

    @Test
    void testRejectNotification_Consolidation_requestTransfer() {
        DeclineNotificationRequest declineNotificationRequest = DeclineNotificationRequest.builder().id(1L).reason("test").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(declineNotificationRequest);
        Notification notification1 = Notification.builder()
                .entityId(1L)
                .entityType(Constants.CONSOLIDATION)
                .notificationRequestType(NotificationRequestType.REQUEST_TRANSFER)
                .requestedBranchId(2)
                .requestedUser("abc")
                .reassignedToBranchId(3)
                .build();
        NetworkTransfer networkTransfer = NetworkTransfer.builder()
                .entityNumber("SHP163834")
                .build();
        Map<Integer, Object> tenantModelMap = new HashMap<>(Map.of(1, new Object()));
        tenantModelMap.put(3, new Object());
        UserContext.getUser().setDisplayName("xyz");
        UserContext.getUser().setEmail("xyz@xyz.com");

        when(notificationDao.findById(anyLong())).thenReturn(Optional.of(notification1));
        when(networkTransferDao.findByTenantAndEntity(any(), anyLong(), anyString())).thenReturn(Optional.of(networkTransfer));
        doAnswer(invocation -> {
            Map<String, String> mapArg = invocation.getArgument(1);
            mapArg.put("abc", "abc@example.com");
            return null;
        }).when(commonUtils).getUserDetails(eq(Set.of("abc")), anyMap());
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(tenantModelMap);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isNteAdditionalEmailsEnabled(true).build());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(new TenantModel());
        var response = notificationService.rejectNotification(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());

    }

    @Test
    void testRejectNotification_Shipment_reassign_error1() {
        DeclineNotificationRequest declineNotificationRequest = DeclineNotificationRequest.builder().id(null).reason("test").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(declineNotificationRequest);
        var response = notificationService.rejectNotification(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testRejectNotification_Shipment_reassign_error2() {
        DeclineNotificationRequest declineNotificationRequest = DeclineNotificationRequest.builder().id(1L).reason("test").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(declineNotificationRequest);
        when(notificationDao.findById(anyLong())).thenReturn(Optional.empty());
        var response = notificationService.rejectNotification(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

}