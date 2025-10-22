package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.NetworkTransferRequest;
import com.dpw.runner.shipment.services.dto.request.ReassignRequest;
import com.dpw.runner.shipment.services.dto.request.RequestForTransferRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.NetworkTransferListResponse;
import com.dpw.runner.shipment.services.dto.response.NetworkTransferResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.entitytransfer.service.impl.EntityTransferV3Service;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
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
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_CTS;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

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
    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private IConsolidationDetailsDao consolidationDao;
    @Mock
    private V1ServiceUtil v1ServiceUtil;
    @Mock
    private ConsolidationV3Service consolidationV3Service;
    @Mock
    private ShipmentServiceImplV3 shipmentServiceImplV3;
    @Mock
    private EntityTransferV3Service entityTransferV3Service;



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
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(NetworkTransfer.builder().entityId(12L).entityType(Constants.SHIPMENT).isInterBranchEntity(false).build()));
        when(notificationDao.save(any(Notification.class))).thenReturn(new Notification());
        var response = networkTransferService.requestForTransfer(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void requestForTransfer2() {
        RequestForTransferRequest requestForTransferRequest = RequestForTransferRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(requestForTransferRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(NetworkTransfer.builder().entityId(12L).entityType(Constants.SHIPMENT).isInterBranchEntity(false).build()));
        when(notificationDao.save(any(Notification.class))).thenReturn(new Notification());
        ShipmentDetails shipment = ShipmentDetails.builder().shipmentId("entityNumber").assignedTo("abc").build();
        shipment.setCreatedBy("creator");
        when(shipmentDao.findShipmentByIdWithQuery(12L)).thenReturn(Optional.of(shipment));
        doAnswer(invocation -> {
            Map<String, String> mapArg = invocation.getArgument(1);
            mapArg.put("abc", "abc@example.com");
            mapArg.put("creator", "creator@example.com");
            return null;
        }).when(commonUtils).getUserDetails(any(), anyMap());
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isNteAdditionalEmailsEnabled(true).build());
        UserContext.getUser().setDisplayName("xyz");
        UserContext.getUser().setEmail("xyz@xyz.com");
        Map<Integer, Object> tenantModelMap = new HashMap<>(Map.of(1, new Object()));
        tenantModelMap.put(3, new Object());
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(tenantModelMap);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(new TenantModel());
        var response = networkTransferService.requestForTransfer(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void requestForTransfer_ForInterConsole() {
        RequestForTransferRequest requestForTransferRequest = RequestForTransferRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(requestForTransferRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(NetworkTransfer.builder().entityId(12L).isInterBranchEntity(true).entityType(Constants.SHIPMENT).build()));
        when(notificationDao.save(any(Notification.class))).thenReturn(new Notification());
        when(consoleShipmentMappingDao.findByShipmentId(any())).thenReturn(Collections.singletonList(ConsoleShipmentMapping.builder().consolidationId(1L).build()));
        when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(Collections.singletonList(ConsoleShipmentMapping.builder().consolidationId(1L).build()));
        when(networkTransferDao.findByEntityIdAndEntityTypeAndIsInterBranchEntity(any(), any(), any(), any(), any())).thenReturn(Collections.singletonList(NetworkTransfer.builder().build()));
        var response = networkTransferService.requestForTransfer(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void requestForTransfer_ForInterConsole_EmptyMapping() {
        RequestForTransferRequest requestForTransferRequest = RequestForTransferRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(requestForTransferRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(NetworkTransfer.builder().entityId(12L).isInterBranchEntity(true).entityType(Constants.SHIPMENT).build()));
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
    void testRequestForTransfer_ConsolidationEntity() {
        RequestForTransferRequest requestForTransferRequest = RequestForTransferRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(requestForTransferRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(NetworkTransfer.builder().entityId(12L).entityType(Constants.CONSOLIDATION).isInterBranchEntity(false).build()));
        when(notificationDao.save(any(Notification.class))).thenReturn(new Notification());
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isNteAdditionalEmailsEnabled(true).build());
        ConsolidationDetails consolidation = ConsolidationDetails.builder().assignedTo("consoleAssigned").build();
        consolidation.setCreatedBy("consoleCreator");
        when(consolidationDao.findConsolidationByIdWithQuery(12L)).thenReturn(Optional.of(consolidation));
        doAnswer(invocation -> {
            Map<String, String> mapArg = invocation.getArgument(1);
            mapArg.put("consoleAssigned", "console@example.com");
            mapArg.put("consoleCreator", "creator@example.com");
            return null;
            }).when(commonUtils).getUserDetails(any(), anyMap());
        UserContext.getUser().setDisplayName("xyz");
        UserContext.getUser().setEmail("xyz@xyz.com");
        Map<Integer, Object> tenantModelMap = new HashMap<>(Map.of(1, new Object()));
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(tenantModelMap);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(new TenantModel());
        var response = networkTransferService.requestForTransfer(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
       void testIsNteAdditionalEmailFlagEnabled_NullSettings() {
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(null);
        RequestForTransferRequest requestForTransferRequest = RequestForTransferRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(requestForTransferRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(NetworkTransfer.builder().entityId(12L).entityType(Constants.SHIPMENT).isInterBranchEntity(false).build()));
        when(notificationDao.save(any(Notification.class))).thenReturn(new Notification());
        networkTransferService.requestForTransfer(request);
        verify(shipmentDao, never()).findShipmentByIdWithQuery(any());
        }


    @Test
    void requestForReassign() {
        ReassignRequest reassignRequest = ReassignRequest.builder().id(12L).branchId(12).remarks("Test").build();
        var request = CommonRequestModel.builder().data(reassignRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(NetworkTransfer.builder().isInterBranchEntity(false).build()));
        when(notificationDao.save(any(Notification.class))).thenReturn(new Notification());
        var response = networkTransferService.requestForReassign(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void requestForReassign2() {
        ReassignRequest reassignRequest = ReassignRequest.builder().id(12L).branchId(12).remarks("Test").build();
        var request = CommonRequestModel.builder().data(reassignRequest).build();
        NetworkTransfer networkTransfer1 = NetworkTransfer.builder().entityType("SHIPMENT").isInterBranchEntity(false).status(NetworkTransferStatus.TRANSFERRED).build();
        ShipmentDetails shipment = ShipmentDetails.builder().shipmentId(any()).build();
        shipment.setCreatedBy("entityCreator");
        when(shipmentDao.findShipmentByIdWithQuery(12L)).thenReturn(Optional.of(shipment));
        networkTransfer1.setCreatedBy("abc");
        doAnswer(invocation -> {
            Map<String, String> mapArg = invocation.getArgument(1);
            mapArg.put("abc", "abc@gmail.com");
            mapArg.put("entityCreator", "entityCreator@gmail.com");
            return null;
        }).when(commonUtils).getUserDetails(any(), any());
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isNteAdditionalEmailsEnabled(true).build());
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(networkTransfer1));
        when(notificationDao.save(any(Notification.class))).thenReturn(new Notification());
        var response = networkTransferService.requestForReassign(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void requestForReassign3() {
        ReassignRequest reassignRequest = ReassignRequest.builder().id(12L).branchId(12).remarks("Test").build();
        var request = CommonRequestModel.builder().data(reassignRequest).build();
        NetworkTransfer networkTransfer1 = NetworkTransfer.builder().entityNumber("abcd").entityType("SHIPMENT").isInterBranchEntity(false).build();
        networkTransfer1.setTenantId(3);
        networkTransfer1.setJobType(Constants.DIRECTION_IMP);
        ShipmentDetails shipment = ShipmentDetails.builder().shipmentId(any()).assignedTo("abc").build();
        shipment.setCreatedBy("entityCreator");
        when(shipmentDao.findShipmentByIdWithQuery(12L)).thenReturn(Optional.of(shipment));
        networkTransfer1.setCreatedBy("bcd");
        doAnswer(invocation -> {
            Map<String, String> mapArg = invocation.getArgument(1);
            mapArg.put("abc", "abc@gmail.com");
            mapArg.put("bcd", "bcd@gmail.com");
            mapArg.put("entityCreator", "entityCreator@gmail.com");
            return null;
        }).when(commonUtils).getUserDetails(any(), any());
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isNteAdditionalEmailsEnabled(true).build());
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(networkTransfer1));
        when(notificationDao.save(any(Notification.class))).thenReturn(new Notification());
        Map<Integer, Object> tenantModelMap = new HashMap<>(Map.of(1, new Object()));
        tenantModelMap.put(3, new Object());
        tenantModelMap.put(12, new Object());
        UserContext.getUser().setDisplayName("xyz");
        UserContext.getUser().setEmail("xyz@xyz.com");
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(tenantModelMap);
        TenantModel tenantModel = new TenantModel();
        tenantModel.setEmail("abcd@gmail.com");
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        var response = networkTransferService.requestForReassign(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void requestForReassign_InterConsoleTrue() {
        ReassignRequest reassignRequest = ReassignRequest.builder().id(12L).branchId(12).remarks("Test").build();
        var request = CommonRequestModel.builder().data(reassignRequest).build();
        NetworkTransfer networkTransfer1 = NetworkTransfer.builder().entityType(Constants.SHIPMENT).isInterBranchEntity(true).build();
        networkTransfer1.setTenantId(1);
        networkTransfer1.setJobType(DIRECTION_CTS);
        networkTransfer1.setEntityNumber("DIRECTION_CTS");
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(networkTransfer1));
        when(notificationDao.save(any(Notification.class))).thenReturn(new Notification());
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isNteAdditionalEmailsEnabled(true).build());
        doAnswer(invocation -> {
            Map<String, String> mapArg = invocation.getArgument(1);
            mapArg.put("abc", "abc@example.com");
            return null;
        }).when(commonUtils).getUserDetails(any(), any());
        UserContext.getUser().setDisplayName("xyz");
        UserContext.getUser().setEmail("xyz@xyz.com");
        Map<Integer, Object> tenantModelMap = new HashMap<>(Map.of(1, new Object()));
        tenantModelMap.put(3, new Object());
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(tenantModelMap);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(new TenantModel());
        var response = networkTransferService.requestForReassign(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void requestForReassign_InterConsoleTrue2() {
        ReassignRequest reassignRequest = ReassignRequest.builder().id(12L).branchId(12).remarks("Test").build();
        var request = CommonRequestModel.builder().data(reassignRequest).build();
        NetworkTransfer networkTransfer1 = NetworkTransfer.builder().entityType(Constants.SHIPMENT).isInterBranchEntity(true).build();
        networkTransfer1.setTenantId(1);
        networkTransfer1.setJobType(DIRECTION_CTS);
        networkTransfer1.setEntityNumber("DIRECTION_CTS");
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(networkTransfer1));
        when(notificationDao.save(any(Notification.class))).thenReturn(new Notification());
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isNteAdditionalEmailsEnabled(true).build());
        when(shipmentDao.findShipmentByIdWithQuery(any())).thenReturn(Optional.of(ShipmentDetails.builder().assignedTo("abc").build()));
        doAnswer(invocation -> {
            Map<String, String> mapArg = invocation.getArgument(1);
            mapArg.put("abc", "abc@example.com");
            return null;
        }).when(commonUtils).getUserDetails(any(), any());
        UserContext.getUser().setDisplayName("xyz");
        UserContext.getUser().setEmail("xyz@xyz.com");
        Map<Integer, Object> tenantModelMap = new HashMap<>(Map.of(1, new Object()));
        tenantModelMap.put(3, new Object());
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(tenantModelMap);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(new TenantModel());
        var response = networkTransferService.requestForReassign(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }


    @Test
    void requestForReassign_InterConsole() {
        UUID shipmentGuid = UUID.randomUUID();
        Map<String, Integer> shipmentGuidReassignBranch = Map.of(shipmentGuid.toString(), 1);
        ReassignRequest reassignRequest = ReassignRequest.builder().id(12L).branchId(12).shipmentGuidReassignBranch(shipmentGuidReassignBranch).remarks("Test").build();
        var request = CommonRequestModel.builder().data(reassignRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(NetworkTransfer.builder().entityType(Constants.CONSOLIDATION).isInterBranchEntity(true).build()));
        when(notificationDao.save(any(Notification.class))).thenReturn(new Notification());
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().receivingBranch(1L).build();
        shipmentDetails1.setGuid(shipmentGuid);
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(NetworkTransfer.builder().build()));
        when(consolidationDao.findConsolidationByIdWithQuery(any())).thenReturn(Optional.of(ConsolidationDetails.builder().interBranchConsole(true).shipmentsList(new HashSet<>(Collections.singletonList(shipmentDetails1))).build()));
        var response = networkTransferService.requestForReassign(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void requestForReassign_DataRetrievalFailure() {
        ReassignRequest reassignRequest = ReassignRequest.builder().id(12L).branchId(12).remarks("Test").build();
        var request = CommonRequestModel.builder().data(reassignRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> networkTransferService.requestForReassign(request));
    }

    @Test
    void requestForReassign_Already_REASSIGNED() {
        ReassignRequest reassignRequest = ReassignRequest.builder().id(12L).branchId(12).remarks("Test").build();
        var request = CommonRequestModel.builder().data(reassignRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(NetworkTransfer.builder().status(NetworkTransferStatus.REASSIGNED).build()));
        assertThrows(DataRetrievalFailureException.class, () -> networkTransferService.requestForReassign(request));
    }

    @Test
    void testRequestForReassign_ConsolidationEntity() {
        ReassignRequest reassignRequest = ReassignRequest.builder().id(12L).branchId(12).remarks("Test").build();
        var request = CommonRequestModel.builder().data(reassignRequest).build();
        NetworkTransfer networkTransfer1 = NetworkTransfer.builder().entityType(Constants.CONSOLIDATION).isInterBranchEntity(false).status(NetworkTransferStatus.TRANSFERRED).build();
        networkTransfer1.setEntityId(12L);
        networkTransfer1.setTenantId(3);
        networkTransfer1.setJobType(Constants.DIRECTION_IMP);
        networkTransfer1.setCreatedBy("nteCreator");
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(networkTransfer1));
        when(notificationDao.save(any(Notification.class))).thenReturn(new Notification());
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isNteAdditionalEmailsEnabled(true).build());
        ConsolidationDetails consolidation = ConsolidationDetails.builder().assignedTo("consoleAssigned").build();
        consolidation.setCreatedBy("consoleCreator");
        when(consolidationDao.findConsolidationByIdWithQuery(12L)).thenReturn(Optional.of(consolidation));
        doAnswer(invocation -> {
            Map<String, String> mapArg = invocation.getArgument(1);
            mapArg.put("consoleAssigned", "console@example.com");
            mapArg.put("nteCreator", "nte@example.com");
            mapArg.put("consoleCreator", "creator@example.com");
            return null;
        }).when(commonUtils).getUserDetails(any(), any());
        UserContext.getUser().setDisplayName("xyz");
        UserContext.getUser().setEmail("xyz@xyz.com");
        Map<Integer, Object> tenantModelMap = new HashMap<>(Map.of(1, new Object()));
        tenantModelMap.put(3, new Object());
        tenantModelMap.put(12, new Object());
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(tenantModelMap);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(new TenantModel());
        var response = networkTransferService.requestForReassign(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
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
    void testListWithSuccessResultWithShipmentNumberFilter() {
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
        ListCommonRequest request = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(25)
                .filterCriteria(new ArrayList<>(List.of(
                        FilterCriteria.builder()
                                .innerFilter(new ArrayList<>(List.of(
                                        FilterCriteria.builder()
                                                .criteria(Criteria.builder()
                                                        .fieldName("isHidden")
                                                        .operator("=")
                                                        .value(false)
                                                        .build())
                                                .build(),
                                        FilterCriteria.builder()
                                                .criteria(Criteria.builder()
                                                        .fieldName("shipmentNumber")
                                                        .operator("=")
                                                        .value("SHP000135316")
                                                        .build())
                                                .logicOperator("and")
                                                .build()
                                )))
                                .build()
                )))
                .sortRequest(SortRequest.builder()
                        .fieldName("createdAt")
                        .order("DESC")
                        .build())
                .build();

        when(modelMapper.map(any(), eq(NetworkTransferListResponse.class))).thenReturn(objectMapper.convertValue(networkTransfer, NetworkTransferListResponse.class));
        // Test
        var responseEntity = networkTransferService.list(CommonRequestModel.buildRequest(request));
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
        shipmentDetails.setId(1L);
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(123L, null,
                Constants.SHIPMENT, shipmentDetails, null, Constants.SHIPMENT_TYPE_DRT, null, false));
    }

    @Test
    void testCreateNetworkTransferEntityWithEmptyShipment(){
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(123L, null,
                Constants.SHIPMENT, null, null, Constants.SHIPMENT_TYPE_DRT, null, false));

    }

    @Test
    void testCreateNetworkTransferEntityWithNewAndOldShipment(){
        when(networkTransferDao.save(any())).thenReturn(networkTransfer);
        when(networkTransferDao.findByTenantAndEntityAndJobType(any(),any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setId(1L);
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(123L, 132L,
                Constants.SHIPMENT, shipmentDetails, null, Constants.SHIPMENT_TYPE_DRT, null, false));

    }

    @Test
    void testCreateNetworkTransferEntityWithNewAndOldShipmentSame(){
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setId(1L);
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(123L, 123L,
                Constants.SHIPMENT, shipmentDetails, null, Constants.SHIPMENT_TYPE_DRT, null, false));

    }

    @Test
    void testCreateNetworkTransferEntityWithOldShipment(){
        when(networkTransferDao.findByTenantAndEntityAndJobType(any(),any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setId(1L);
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(null, 132L,
                Constants.SHIPMENT, shipmentDetails, null, Constants.DIRECTION_EXP, null, false));
    }

    @Test
    void testCreateNetworkTransferEntityWitSomeOtherEntity(){
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setId(1L);
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(null, 132L,
                Constants.SHIPMENT_CAMELCASE, shipmentDetails, null, Constants.DIRECTION_EXP, null, false));
    }

    @Test
    void testCreateNetworkTransferEntityFailure(){
        when(networkTransferDao.save(any())).thenThrow(new RuntimeException("Error"));
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setId(1L);
        assertThrows(RuntimeException.class, () -> networkTransferService.processNetworkTransferEntity(123L, null,
                Constants.SHIPMENT, shipmentDetails, null, Constants.DIRECTION_EXP, null, false));
    }

    @Test
    void testCreateNetworkTransferEntityWithConsolidation(){
        when(networkTransferDao.save(any())).thenReturn(networkTransfer);
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(123L, null,
                Constants.CONSOLIDATION, null, consolidationDetails, Constants.DIRECTION_CTS, null, false));
    }

    @Test
    void testCreateNetworkTransferEntityWithEmptyConsolidation(){
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(123L, null,
                Constants.CONSOLIDATION, null, null, Constants.DIRECTION_CTS, null, false));
    }

    @Test
    void testCreateNetworkTransferEntityWithOldConsolidation(){
        when(networkTransferDao.findByTenantAndEntityAndJobType(any(),any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        assertDoesNotThrow(() -> networkTransferService.processNetworkTransferEntity(null, 132L,
                Constants.CONSOLIDATION, null, consolidationDetails, Constants.DIRECTION_EXP, null, false));
    }

    @Test
    void testCreateNetworkTransferEntityDBFindFailure(){
        when(networkTransferDao.findByTenantAndEntityAndJobType(any(), any(), any(), any())).thenThrow(new RuntimeException("Connection Time out"));
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        assertThrows(RuntimeException.class, () -> networkTransferService.processNetworkTransferEntity(123L, 321L,
                Constants.SHIPMENT, shipmentDetails, null, Constants.DIRECTION_EXP, null, false));
    }

    @Test
    void testCreateNetworkTransferEntityDBDeleteFailure(){
        when(networkTransferDao.findByTenantAndEntityAndJobType(any(),any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        doThrow(new RuntimeException("Connection Time out")).when(networkTransferDao).deleteAndLog(any(), any());
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        assertThrows(RuntimeException.class, () -> networkTransferService.processNetworkTransferEntity(123L, 321L,
                Constants.SHIPMENT, shipmentDetails, null, Constants.DIRECTION_EXP, null, false));
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

    @Test
    void testDeleteNetworkTransferEntity(){
        assertDoesNotThrow(() -> networkTransferService.deleteNetworkTransferEntity(networkTransfer));
    }


    @Test
    void testDeleteNetworkTransferEntity2(){
        networkTransfer.setStatus(NetworkTransferStatus.ACCEPTED);
        assertDoesNotThrow(() -> networkTransferService.deleteNetworkTransferEntity(networkTransfer));
    }

    @Test
    void testDeleteNetworkTransferEntityException(){
        doThrow(new RuntimeException("Connection Time out")).when(networkTransferDao).deleteAndLog(any(), any());
        assertDoesNotThrow(() -> networkTransferService.deleteNetworkTransferEntity(networkTransfer));
    }

    @Test
    void testUpdateStatusAndCreatedEntityId() {
        assertDoesNotThrow(() -> networkTransferService.updateStatusAndCreatedEntityId(1L, NetworkTransferStatus.ACCEPTED.name(), 2L));
    }

    @Test
    void testFetchEntityStatus() {
        var guid = UUID.randomUUID().toString();
        when(shipmentDao.findReceivingByGuid(UUID.fromString(guid))).thenReturn(1);
        when(networkTransferDao.findByEntityGuidAndTenantId(UUID.fromString(guid), 1)).thenReturn(NetworkTransferStatus.SCHEDULED.name());
        var response = networkTransferService.fetchEntityStatus(CommonGetRequest.builder().guid(guid).build());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testFetchEntityStatus1() {
        var guid = UUID.randomUUID().toString();
        when(shipmentDao.findReceivingByGuid(UUID.fromString(guid))).thenReturn(null);
        var response = networkTransferService.fetchEntityStatus(CommonGetRequest.builder().guid(guid).build());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testBulkProcessInterConsoleNte() {
        networkTransferService.bulkProcessInterConsoleNte(new ArrayList<>());
        verify(networkTransferDao, times(0)).saveAll(any());
    }

    @Test
    void testBulkProcessInterConsoleNte1() {
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().build();
        shipmentDetails1.setReceivingBranch(1L);
        networkTransferService.bulkProcessInterConsoleNte(Collections.singletonList(shipmentDetails1));
        verify(networkTransferDao, times(0)).saveAll(any());
    }

    @Test
    void testBulkProcessInterConsoleNte2() {
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().build();
        shipmentDetails1.setReceivingBranch(2L);
        networkTransferService.bulkProcessInterConsoleNte(Collections.singletonList(shipmentDetails1));
        verify(networkTransferDao, times(1)).saveAll(any());
    }

    @Test
    void testCreateExternal() {
        NetworkTransferRequest networkTransferRequest = NetworkTransferRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(networkTransferRequest);
        when(jsonHelper.convertCreateValue(any(), eq(NetworkTransfer.class))).thenReturn(NetworkTransfer.builder().status(NetworkTransferStatus.TRANSFERRED).build());
        var response = networkTransferService.createExternal(commonRequestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testCreateExternal_NewEntity() {
        // Arrange
        NetworkTransferRequest networkTransferRequest = NetworkTransferRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(networkTransferRequest);

        NetworkTransfer newEntity = NetworkTransfer.builder()
                .entityNumber("ENT123")
                .status(NetworkTransferStatus.TRANSFERRED)
                .build();

        when(jsonHelper.convertCreateValue(any(), eq(NetworkTransfer.class)))
                .thenReturn(newEntity);

        // No existing entity
        when(networkTransferDao.findByEntityNumber("ENT123"))
                .thenReturn(Optional.empty());

        when(networkTransferDao.save(any(NetworkTransfer.class)))
                .thenAnswer(invocation -> invocation.getArgument(0));

        // Act
        var response = networkTransferService.createExternal(commonRequestModel);

        // Assert
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(networkTransferDao).findByEntityNumber("ENT123");
        verify(networkTransferDao).save(any(NetworkTransfer.class));
    }

    @Test
    void testCreateExternal_UpdateExistingEntity() {
        // Arrange
        NetworkTransferRequest networkTransferRequest = NetworkTransferRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(networkTransferRequest);

        NetworkTransfer incoming = NetworkTransfer.builder()
                .entityNumber("ENT123")
                .status(NetworkTransferStatus.TRANSFERRED)
                .jobType("NewJob")
                .build();

        NetworkTransfer existing = NetworkTransfer.builder()
                .entityNumber("ENT123")
                .status(NetworkTransferStatus.SCHEDULED)
                .jobType("OldJob")
                .build();

        when(jsonHelper.convertCreateValue(any(), eq(NetworkTransfer.class)))
                .thenReturn(incoming);

        when(networkTransferDao.findByEntityNumber("ENT123"))
                .thenReturn(Optional.of(existing));

        when(networkTransferDao.save(any(NetworkTransfer.class)))
                .thenAnswer(invocation -> invocation.getArgument(0));

        // Act
        var response = networkTransferService.createExternal(commonRequestModel);

        // Assert
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(networkTransferDao).findByEntityNumber("ENT123");
        verify(networkTransferDao).save(existing); // ensure the existing entity is updated
        assertEquals("NewJob", existing.getJobType()); // confirm field update
        assertEquals(NetworkTransferStatus.TRANSFERRED, existing.getStatus());
    }


    @Test
    void testGetAllMasterDataForNT_withConsolidationAndShipments() {
        // Arrange
        Map<String, Object> requestPayload = new HashMap<>();

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder()
                .consolidationNumber("CON123")
                .shipmentsList(Set.of(
                        ShipmentDetails.builder().shipmentId("SHIP123").build()
                ))
                .build();

        ConsolidationDetailsV3Response consolidationDetailsV3Response =
                new ConsolidationDetailsV3Response();

        ShipmentDetailsResponse shipmentDetailsResponse1 = new ShipmentDetailsResponse();
        ShipmentDetailsResponse shipmentDetailsResponse2 = new ShipmentDetailsResponse();

        Map<String, Object> consolMasterData = Map.of("key1", "value1");
        Map<String, Object> shipmentMasterData1 = Map.of("s1", "v1");
        Map<String, Object> shipmentMasterData2 = Map.of("s2", "v2");

        when(jsonHelper.convertValue(requestPayload, ConsolidationDetails.class))
                .thenReturn(consolidationDetails);

//        when(networkTransferService.getAllMasterDataForNT(anyMap())).thenReturn(ResponseHelper.buildSuccessResponse());

        when(commonUtils.setIncludedFieldsToResponse(eq(consolidationDetails), anySet(), any(ConsolidationDetailsV3Response.class)))
                .thenReturn(consolidationDetailsV3Response);

        when(consolidationV3Service.fetchAllMasterDataByKey(consolidationDetailsV3Response))
                .thenReturn(consolMasterData);

        when(commonUtils.setIncludedFieldsToResponse(eq(consolidationDetails.getShipmentsList().iterator().next()), anySet(), any(ShipmentDetailsResponse.class)))
                .thenReturn(shipmentDetailsResponse1)
                .thenReturn(shipmentDetailsResponse2);

        when(shipmentServiceImplV3.fetchAllMasterDataByKey(any(), any()))
                .thenReturn(shipmentMasterData1)
                .thenReturn(shipmentMasterData2);

        // Act
        ResponseEntity<IRunnerResponse> response =
                networkTransferService.getAllMasterDataForNT(requestPayload);

        // Assert
        assertEquals(HttpStatus.OK, response.getStatusCode());

        // Verify interactions
        verify(jsonHelper).convertValue(requestPayload, ConsolidationDetails.class);
        verify(consolidationV3Service).fetchAllMasterDataByKey(consolidationDetailsV3Response);
        verify(shipmentServiceImplV3, times(1)).fetchAllMasterDataByKey(any(), any());
    }

    @Test
    void testGetAllMasterDataForNT_withEmptyShipments() {
        // Arrange
        Map<String, Object> requestPayload = new HashMap<>();

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder()
                .consolidationNumber("CON999")
                .shipmentsList(Collections.emptySet())
                .build();

        ConsolidationDetailsV3Response consolidationDetailsV3Response =
                new ConsolidationDetailsV3Response();
        Map<String, Object> consolMasterData = Map.of("keyX", "valueX");

        when(jsonHelper.convertValue(requestPayload, ConsolidationDetails.class))
                .thenReturn(consolidationDetails);
        when(commonUtils.setIncludedFieldsToResponse(eq(consolidationDetails), anySet(), any(ConsolidationDetailsV3Response.class)))
                .thenReturn(consolidationDetailsV3Response);
        when(consolidationV3Service.fetchAllMasterDataByKey(consolidationDetailsV3Response))
                .thenReturn(consolMasterData);

        // Act
        ResponseEntity<IRunnerResponse> response =
                networkTransferService.getAllMasterDataForNT(requestPayload);

        // Assert
        assertEquals(HttpStatus.OK, response.getStatusCode());

        verify(shipmentServiceImplV3, never()).fetchAllMasterDataByKey(any(), any());
    }


    @Test
    void testGetAllDestinationBranchEmailsForNT_ShouldReturnEmailList() {
        // Arrange
        Integer destinationBranch = 10;
        List<String> expectedEmails = List.of("test1@dpworld.com", "test2@dpworld.com");

        when(entityTransferV3Service.getEmailsListByPermissionKeysAndTenantId(
                Collections.singletonList(PermissionConstants.SHIPMENT_IN_PIPELINE_MODIFY),
                destinationBranch
        )).thenReturn(expectedEmails);

        // Act
        List<String> actualEmails = networkTransferService.getAllDestinationBranchEmailsForNT(destinationBranch);

        // Assert
        assertEquals(expectedEmails, actualEmails);
        verify(entityTransferV3Service).getEmailsListByPermissionKeysAndTenantId(
                Collections.singletonList(PermissionConstants.SHIPMENT_IN_PIPELINE_MODIFY),
                destinationBranch
        );
    }

    @Test
    void testGetAllDestinationBranchEmailsForNT_WhenEmptyListReturned_ShouldReturnEmptyList() {
        // Arrange
        Integer destinationBranch = 20;
        when(entityTransferV3Service.getEmailsListByPermissionKeysAndTenantId(
                Collections.singletonList(PermissionConstants.SHIPMENT_IN_PIPELINE_MODIFY),
                destinationBranch
        )).thenReturn(Collections.emptyList());

        // Act
        List<String> result = networkTransferService.getAllDestinationBranchEmailsForNT(destinationBranch);

        // Assert
        assertEquals(Collections.emptyList(), result);
        verify(entityTransferV3Service).getEmailsListByPermissionKeysAndTenantId(
                Collections.singletonList(PermissionConstants.SHIPMENT_IN_PIPELINE_MODIFY),
                destinationBranch
        );
    }

}