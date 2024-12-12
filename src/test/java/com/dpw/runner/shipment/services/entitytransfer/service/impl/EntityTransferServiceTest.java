package com.dpw.runner.shipment.services.entitytransfer.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.anyInt;
import static org.mockito.Mockito.anyList;
import static org.mockito.Mockito.anyLong;
import static org.mockito.Mockito.anyMap;
import static org.mockito.Mockito.anySet;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.LogHistoryResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.request.TaskCreateRequest;
import com.dpw.runner.shipment.services.dto.v1.request.V1UsersEmailRequest;
import com.dpw.runner.shipment.services.dto.v1.response.SendEntityResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TenantIdResponse;
import com.dpw.runner.shipment.services.dto.v1.response.UsersRoleListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.TaskStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferConsolidationDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferShipmentDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.CheckEntityExistRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.CheckTaskExistRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.ImportConsolidationRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.ImportShipmentRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.PostArValidationRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.SendConsolidationRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.SendShipmentRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.ValidateSendConsolidationRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.ValidateSendShipmentRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.CheckTaskExistResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.ValidationResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.service.impl.NetworkTransferService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.ILogsHistoryService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.interfaces.ITasksService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.impl.ConsolidationSync;
import com.dpw.runner.shipment.services.syncing.impl.ShipmentSync;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
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
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class EntityTransferServiceTest extends CommonMocks {

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private IShipmentService shipmentService;
    @Mock
    private IConsolidationService consolidationService;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private IV1Service v1Service;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IV1Service iv1Service;
    @Mock
    private INotificationService notificationService;
    @Mock
    private IHblDao hblDao;
    @Mock
    private UsersDto user;
    @Mock
    private IAwbDao awbDao;
    @Mock
    private INetworkTransferDao networkTransferDao;
    @Mock
    private ILogsHistoryService logsHistoryService;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private EmailTemplatesRequest template;

    @Mock
    private ConsolidationDetails consolidationDetails;

    @Mock
    private ConsolidationDetailsResponse consolidationDetailsResponse;
    @Mock
    private ShipmentDetails shipmentDetails;
    @Mock
    private V1TenantResponse v1TenantResponse;
    @Mock
    private TenantModel tenantModel;
    @Mock
    private UsersDto usersDto;
    @Mock
    private ShipmentSettingsDetails shipmentSettingsDetails;
    @Mock
    private List<ShipmentDetails> shipmentDetailsForTenant;
    @Mock
    private IEventDao eventDao;
    @Mock
    private NetworkTransferService networkTransferService;
    @Mock
    private V1MasterDataImpl v1MasterData;
    @Mock
    MasterDataFactory masterDataFactory;
    @Mock
    private ITasksService tasksService;
    @Mock
    private IContainerDao containerDao;
    @Mock
    private IPackingDao packingDao;
    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Mock
    V1ServiceUtil v1ServiceUtil;
    @InjectMocks
    private EntityTransferService entityTransferService;
    @Mock
    private ShipmentSync shipmentSync;
    @Mock
    private ConsolidationSync consolidationSync;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;

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
//        testairMessagingLogs = jsonTestUtility.getTestAirMessagingLogs();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setTenantDisplayName("abc");
        mockUser.setDisplayName("abc");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void testSendShipmentThrowsExceptionIfSendToBranchIsEmpty() {
        SendShipmentRequest sendShipmentRequest = new SendShipmentRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendShipmentRequest);

        var e = assertThrows(ValidationException.class, () ->
            entityTransferService.sendShipment(commonRequestModel));

    }

    @Test
    void testSendShipment_Failure_DataRetrievalException() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();
        SendShipmentRequest request = SendShipmentRequest.builder()
                .shipId(shipmentDetails.getId())
                .sendToBranch(List.of(66))
                .sendToOrg(List.of(organizations.getOrganizationCode()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        when(shipmentDao.findById(shipmentDetails.getId())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> entityTransferService.sendShipment(commonRequestModel));
    }

    @Test
    void testSendShipmentThrowsErrorForTenantWithoutApproverRole() {
        Long shipmentId = 1L;
        int mockTenantId = 10;

        SendShipmentRequest sendShipmentRequest = new SendShipmentRequest();
        sendShipmentRequest.setSendToBranch(List.of(1,2,3));
        sendShipmentRequest.setShipId(shipmentId);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendShipmentRequest);

        ShipmentDetails mockShipmentDetails = jsonTestUtility.getCompleteShipment();
        mockShipmentDetails.setTenantId(mockTenantId);
        EntityTransferShipmentDetails mockETPayload = objectMapperTest.convertValue(mockShipmentDetails, EntityTransferShipmentDetails.class);
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );

        // Mocking
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(mockShipmentDetails));
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));

        var e = assertThrows(ValidationException.class, () -> entityTransferService.sendShipment(commonRequestModel));

    }
    @Test
    void testSendShipmentSuccess() {
        Long shipmentId = 1L;
        int mockTenantId = 10;

        SendShipmentRequest sendShipmentRequest = new SendShipmentRequest();
        sendShipmentRequest.setSendToBranch(List.of(1,2,3));
        sendShipmentRequest.setShipId(shipmentId);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendShipmentRequest);

        ShipmentDetails mockShipmentDetails = jsonTestUtility.getCompleteShipment();
        mockShipmentDetails.setTenantId(mockTenantId);
        EntityTransferShipmentDetails mockETPayload = objectMapperTest.convertValue(mockShipmentDetails, EntityTransferShipmentDetails.class);
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );

        // Mocking
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(mockShipmentDetails));
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));
        when(jsonHelper.convertValue(any(), eq(EntityTransferShipmentDetails.class))).thenReturn(mockETPayload);
        when(shipmentService.fetchAllMasterDataByKey(any(), any())).thenReturn(new HashMap<String, Object>());

        mockShipmentSettings();
        var httpResponse = entityTransferService.sendShipment(commonRequestModel);

        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
//        verify(eventDao, atLeast(1)).generateEvents(any());
        verify(shipmentDao, times(1)).saveEntityTransfer(any(), any());
        verify(tasksService, times(3)).createTask(any());

    }

    @Test
    void testSendShipmentSuccess_WithTriangulationPartner() {
        Long shipmentId = 1L;
        int mockTenantId = 10;

        // Prepare SendShipmentRequest
        SendShipmentRequest sendShipmentRequest = new SendShipmentRequest();
        sendShipmentRequest.setSendToBranch(List.of(2, 3, 4)); // Include tenants, one of which is in triangulation partners
        sendShipmentRequest.setShipId(shipmentId);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendShipmentRequest);

        // Mock ShipmentDetails
        ShipmentDetails mockShipmentDetails = jsonTestUtility.getCompleteShipment();
        mockShipmentDetails.setTenantId(mockTenantId);
        mockShipmentDetails.setReceivingBranch(1L); // Ensure 1L is the receiving branch
        mockShipmentDetails.setTriangulationPartnerList(List.of(3L, 4L)); // Set triangulation partners

        // Mock Payload and Response
        EntityTransferShipmentDetails mockETPayload = new EntityTransferShipmentDetails();
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );

        // Mocking
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(mockShipmentDetails));
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValue(any(), eq(EntityTransferShipmentDetails.class))).thenReturn(mockETPayload);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);

        mockShipmentSettings();
        var httpResponse = entityTransferService.sendShipment(commonRequestModel);

        // Assertions
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }



    @Test
    void testSendConsolidationFailureInCaseOfEmptySendToOrg() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().get(0).getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(null)
                .consolId(consolidationDetails.getId())
                .sendToOrg(null)
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendConsolidationRequest);

        assertThrows(ValidationException.class, ()-> entityTransferService.sendConsolidation(commonRequestModel));
    }

    @Test
    void testSendConsolidation_Failure_DataRetrievalError() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().get(0).getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails.getId())
                .sendToOrg(null)
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendConsolidationRequest);

        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, ()-> entityTransferService.sendConsolidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationSuccess() {
        int mockTenantId = 10;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTenantId(mockTenantId);
        consolidationDetails.getShipmentsList().stream().forEach(i -> i.setTenantId(mockTenantId));
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();
        V1DataResponse v1DataResponseOrg = V1DataResponse.builder().entities(List.of(organizations)).build();

        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().get(0).getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();

        EntityTransferConsolidationDetails mockETPayload = objectMapperTest.convertValue(consolidationDetails, EntityTransferConsolidationDetails.class);
        ShipmentDetails mockLinkedShipment = new ShipmentDetails();
        EntityTransferShipmentDetails mockETShipment = new EntityTransferShipmentDetails();
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );

        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.of(consolidationDetails));
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferShipmentDetails.class))).thenReturn(mockETShipment);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testConsolidationInterBranchSuccess() {
        int mockTenantId = 10;
        UUID shipGuid = UUID.randomUUID();
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTenantId(mockTenantId);
        consolidationDetails.setInterBranchConsole(true);
        consolidationDetails.getShipmentsList().stream().forEach(i -> {
            i.setTenantId(mockTenantId);
            i.setGuid(shipGuid);
        });

        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().get(0).getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails.getId())
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .shipmentGuidSendToBranch(Map.ofEntries(
                        Map.entry(shipGuid.toString(), List.of(69))
                ))
                .build();

        EntityTransferConsolidationDetails mockETPayload = objectMapperTest.convertValue(consolidationDetails, EntityTransferConsolidationDetails.class);
        ShipmentDetails mockLinkedShipment = consolidationDetails.getShipmentsList().get(0);
        EntityTransferShipmentDetails mockETShipment = objectMapperTest.convertValue(mockLinkedShipment, EntityTransferShipmentDetails.class);
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );
        V1TenantSettingsResponse mockV1TenantSettings = V1TenantSettingsResponse.builder()
                .IsColoadingMAWBStationEnabled(true)
                .ColoadingBranchIds(List.of(69))
                .build();
        Map<Integer, V1TenantSettingsResponse> mockV1TenantSettingsMap = Map.ofEntries(
                Map.entry(66, mockV1TenantSettings)
        );

        var coLoadMap = new HashMap<Integer, Set<Integer>>();
        coLoadMap.put(66, Set.of(69));

        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.of(consolidationDetails));
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(v1ServiceUtil.getTenantSettingsMap(anyList())).thenReturn(mockV1TenantSettingsMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferShipmentDetails.class))).thenReturn(mockETShipment);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));
        when(v1ServiceUtil.fetchCoLoadInfo(any(), any())).thenReturn(coLoadMap);
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

    }


    @Test
    // change IMP -> EXP || EXP -> IMP if receiving branch == send to branch
    void testConsolidationSuccessReverseDirectionInResponsePayload() {
        int mockTenantId = 10;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setReceivingBranch(66L);
        consolidationDetails.setTenantId(mockTenantId);
        consolidationDetails.getShipmentsList().stream().forEach(i -> i.setTenantId(mockTenantId));
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();
        V1DataResponse v1DataResponseOrg = V1DataResponse.builder().entities(List.of(organizations)).build();

        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().get(0).getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();

        EntityTransferConsolidationDetails mockETPayload = objectMapperTest.convertValue(consolidationDetails, EntityTransferConsolidationDetails.class);
        ShipmentDetails mockLinkedShipment = new ShipmentDetails();
        EntityTransferShipmentDetails mockETShipment = new EntityTransferShipmentDetails();
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );

        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.of(consolidationDetails));
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferShipmentDetails.class))).thenReturn(mockETShipment);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

    }

//    @Test
    void testSendConsolidation_Failure1() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();
        V1DataResponse v1DataResponseOrg = V1DataResponse.builder().entities(List.of(organizations)).build();
        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().get(0).getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendConsolidationRequest);

        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.of(consolidationDetails));
        when(v1Service.fetchOrganization(any())).thenReturn(v1DataResponseOrg);
        when(jsonHelper.convertValueToList(List.of(organizations), EntityTransferOrganizations.class)).thenReturn(List.of(organizations));
        when(v1Service.tenantByGuid(any())).thenReturn(TenantIdResponse.builder().id(69).build());
        when(v1Service.sendV1ConsolidationTask(any())).thenThrow(new RuntimeException());

        assertThrows(RuntimeException.class, ()-> entityTransferService.sendConsolidation(commonRequestModel));
    }

//    @Test
    void testSendConsolidation_Failure2() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();
        V1DataResponse v1DataResponseOrg = V1DataResponse.builder().entities(List.of(organizations)).build();
        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().get(0).getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendConsolidationRequest);

        SendEntityResponse v1ConsoleTaskResponse = new SendEntityResponse();
        v1ConsoleTaskResponse.setIsCreated(false);

        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.of(consolidationDetails));
        when(v1Service.fetchOrganization(any())).thenReturn(v1DataResponseOrg);
        when(jsonHelper.convertValueToList(List.of(organizations), EntityTransferOrganizations.class)).thenReturn(List.of(organizations));
        when(v1Service.tenantByGuid(any())).thenReturn(TenantIdResponse.builder().id(69).build());
        when(v1Service.sendV1ConsolidationTask(any())).thenReturn(v1ConsoleTaskResponse);

        assertThrows(RuntimeException.class, ()-> entityTransferService.sendConsolidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Success() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        ValidationResponse response = ValidationResponse.builder().success(true).build();
        TenantModel tenantModel = new TenantModel();
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(hblDao.findByShipmentId(consolidationDetails.getShipmentsList().get(0).getId())).thenReturn(List.of(new Hbl()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidationValidation(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
    }

    @Test
    void testSendConsolidationValidation_Failure_DataRetrievalException() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Failure_ConsoleFieldsException() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setBol(null);
        consolidationDetails.getCarrierDetails().setVoyage(null);
        consolidationDetails.getCarrierDetails().setVessel(null);
        consolidationDetails.getCarrierDetails().setEta(null);
        consolidationDetails.getCarrierDetails().setEtd(null);
        consolidationDetails.getCarrierDetails().setOriginPort(null);
        consolidationDetails.getCarrierDetails().setDestinationPort(null);
        consolidationDetails.setReceivingBranch(null);
        consolidationDetails.setTriangulationPartnerList(null);
        consolidationDetails.setTriangulationPartner(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Failure_Console_Rail() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_RAI);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Failure_ShipmentFieldsException_Hbl() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().get(0);
        shipmentDetails.getCarrierDetails().setVessel(null);
        shipmentDetails.getCarrierDetails().setVoyage(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel = new TenantModel();
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(hblDao.findByShipmentId(consolidationDetails.getShipmentsList().get(0).getId())).thenReturn(List.of());
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }
    @Test
    void testSendConsolidationValidation_Failure_ShipmentFieldsException() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().get(0);
        shipmentDetails.getCarrierDetails().setVessel(null);
        shipmentDetails.getCarrierDetails().setVoyage(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel = new TenantModel();
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(hblDao.findByShipmentId(consolidationDetails.getShipmentsList().get(0).getId())).thenReturn(List.of(new Hbl()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Success_Air() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.getCarrierDetails().setFlightNumber("A123");
        consolidationDetails.getCarrierDetails().setShippingLine("Air India");
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().get(0);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.getCarrierDetails().setFlightNumber("A123");
        shipmentDetails.getCarrierDetails().setShippingLine("Air India");
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        ValidationResponse response = ValidationResponse.builder().success(true).build();
        TenantModel tenantModel = new TenantModel();
        tenantModel.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(awbDao.findByShipmentId(consolidationDetails.getShipmentsList().get(0).getId())).thenReturn(List.of(new Awb()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidationValidation(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
    }

    @Test
    void testSendConsolidationValidation_Failure_Air_ConsoleFieldsException() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.getCarrierDetails().setFlightNumber(null);
        consolidationDetails.setBol(null);
        consolidationDetails.getCarrierDetails().setShippingLine(null);
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().get(0);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.getCarrierDetails().setFlightNumber("A123");
        shipmentDetails.getCarrierDetails().setShippingLine("Air India");
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Failure_Air_ShipmentFieldsException_Awb() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.getCarrierDetails().setFlightNumber("A123");
        consolidationDetails.getCarrierDetails().setShippingLine("Air India");
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().get(0);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.getCarrierDetails().setFlightNumber(null);
        shipmentDetails.getCarrierDetails().setShippingLine(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel = new TenantModel();
        tenantModel.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(awbDao.findByShipmentId(consolidationDetails.getShipmentsList().get(0).getId())).thenReturn(List.of(new Awb()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Failure_Air_ShipmentFieldsException() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.getCarrierDetails().setFlightNumber("A123");
        consolidationDetails.getCarrierDetails().setShippingLine("Air India");
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().get(0);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.getCarrierDetails().setFlightNumber(null);
        shipmentDetails.getCarrierDetails().setShippingLine(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel = new TenantModel();
        tenantModel.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(awbDao.findByShipmentId(consolidationDetails.getShipmentsList().get(0).getId())).thenReturn(List.of());
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Failure_Air_NON_STD_ShipmentFieldsException() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.getCarrierDetails().setFlightNumber("A123");
        consolidationDetails.getCarrierDetails().setShippingLine("Air India");
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().get(0);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.CONSOLIDATION_TYPE_CLD);
        shipmentDetails.getCarrierDetails().setFlightNumber(null);
        shipmentDetails.getCarrierDetails().setShippingLine(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel = new TenantModel();
        tenantModel.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }
    @Test
    void testSendConsolidationValidation_Failure_Air_ShipmentFieldsException_InterBranch() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.getCarrierDetails().setFlightNumber("A123");
        consolidationDetails.getCarrierDetails().setShippingLine("Air India");
        consolidationDetails.setInterBranchConsole(true);
        consolidationDetails.setReceivingBranch(12L);
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().get(0);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setReceivingBranch(null);
        shipmentDetails.getCarrierDetails().setFlightNumber(null);
        shipmentDetails.getCarrierDetails().setShippingLine(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel = new TenantModel();
        tenantModel.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(awbDao.findByShipmentId(consolidationDetails.getShipmentsList().get(0).getId())).thenReturn(List.of());
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendShipmentValidation_Success() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setHouseBill("QWERT324");
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails.getId()).build();
        ValidationResponse response = ValidationResponse.builder().success(true).build();
        TenantModel tenantModel = new TenantModel();
        tenantModel.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails));
        when(hblDao.findByShipmentId(shipmentDetails.getId())).thenReturn(List.of(new Hbl()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendShipmentValidation(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
    }

    @Test
    void testSendShipmentValidation_Failure_DataRetrievalFailure() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setHouseBill("QWERT324");
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
    }

    @Test
    void testSendShipmentValidation_Failure_ShipmentFieldsException() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setHouseBill(null);
        shipmentDetails.setMasterBill(null);
        shipmentDetails.getCarrierDetails().setVoyage(null);
        shipmentDetails.getCarrierDetails().setVessel(null);
        shipmentDetails.getCarrierDetails().setEta(null);
        shipmentDetails.getCarrierDetails().setEtd(null);
        shipmentDetails.getCarrierDetails().setOriginPort(null);
        shipmentDetails.getCarrierDetails().setDestinationPort(null);
        shipmentDetails.setReceivingBranch(null);
        shipmentDetails.setTriangulationPartnerList(null);
        shipmentDetails.setTriangulationPartner(null);
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails));
        assertThrows(ValidationException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
    }

    @Test
    void testSendShipmentValidation_Failure_HblError() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setHouseBill("QWERT324");
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails));
        when(hblDao.findByShipmentId(shipmentDetails.getId())).thenReturn(List.of());
        assertThrows(ValidationException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
    }

    @Test
    void testSendShipmentValidation_Success_Air() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.getCarrierDetails().setFlightNumber("W233");
        shipmentDetails.getCarrierDetails().setShippingLine("Air India");
        shipmentDetails.setHouseBill("QWERT324");
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails.getId()).build();
        ValidationResponse response = ValidationResponse.builder().success(true).build();
        TenantModel tenantModel = new TenantModel();
        tenantModel.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails));
        when(awbDao.findByShipmentId(shipmentDetails.getId())).thenReturn(List.of(new Awb()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendShipmentValidation(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
    }

    @Test
    void testSendShipmentValidation_Failure_Air_ShipmentFieldsError() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.getCarrierDetails().setFlightNumber(null);
        shipmentDetails.getCarrierDetails().setShippingLine(null);
        shipmentDetails.setHouseBill(null);
        shipmentDetails.setMasterBill(null);
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails));
        assertThrows(ValidationException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
    }

    @Test
    void testSendShipmentValidation_Failure_Air_AwbError() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.getCarrierDetails().setFlightNumber("W233");
        shipmentDetails.getCarrierDetails().setShippingLine("Air India");
        shipmentDetails.setHouseBill("QWERT324");
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel = new TenantModel();
        tenantModel.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails));
        when(awbDao.findByShipmentId(shipmentDetails.getId())).thenReturn(List.of());
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        assertThrows(ValidationException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
    }

    @Test
    void testCheckTaskExist_Success_Shipment() {
        try {
            CheckTaskExistRequest request = CheckTaskExistRequest.builder()
                .entityId(123L)
                .entityType(Constants.Shipments)
                .sendToBranch(List.of(66))
                .build();
            CheckTaskExistResponse response = CheckTaskExistResponse.builder().sendToBranch(Set.of(66)).build();
            List<TaskCreateRequest> taskCreateRequests = new ArrayList<>();
            taskCreateRequests.add(TaskCreateRequest.builder().tenantId(String.valueOf(66)).build());
            V1DataResponse v1DataResponse = V1DataResponse.builder().entities(taskCreateRequests).build();

            when(v1Service.listTask(any())).thenReturn(v1DataResponse);
            when(jsonHelper.convertValueToList(any(), eq(TaskCreateRequest.class))).thenReturn(taskCreateRequests);

            ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.checkTaskExist(CommonRequestModel.buildRequest(request));
            assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
            assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
        }
        catch (Exception e) {
            fail(e);
        }
    }

    @Test
    void testCheckTaskExist_Empty_sendToBranch() {
        try {
            CheckTaskExistRequest request = CheckTaskExistRequest.builder()
                    .entityId(123L)
                    .entityType(Constants.Shipments)
                    .sendToBranch(List.of())
                    .build();
            ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.checkTaskExist(CommonRequestModel.buildRequest(request));
            assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        }
        catch (Exception e) {
            fail(e);
        }
    }
    @Test
    void testCheckTaskExist_V1Error_Shipment() {
        CheckTaskExistRequest request = CheckTaskExistRequest.builder()
                .entityId(123L)
                .entityType(Constants.Shipments)
                .sendToBranch(List.of(66))
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        when(v1Service.listTask(any())).thenThrow(new RuntimeException());

        assertThrows(RunnerException.class, () -> entityTransferService.checkTaskExist(commonRequestModel));
    }

    @Test
    void testPostArValidation_Success() throws RunnerException {
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setTenantId(33);
        ConsolidationDetails consolidationDetails = shipmentDetails.getConsolidationList().get(0);
        consolidationDetails.setGuid(UUID.randomUUID());
        consolidationDetails.setReceivingBranch(123L);
        consolidationDetails.setTriangulationPartnerList(List.of(231L));
        consolidationDetails.setTriangulationPartner(231L);
        ShipmentDetails shipmentDetailsDrt = jsonTestUtility.getCompleteShipment();
        shipmentDetailsDrt.setGuid(UUID.randomUUID());
        shipmentDetailsDrt.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetailsDrt.setTenantId(33);

        ShipmentDetails shipmentDetailsImp = jsonTestUtility.getCompleteShipment();
        shipmentDetailsImp.setGuid(UUID.randomUUID());
        shipmentDetailsImp.setDirection(Constants.DIRECTION_IMP);
        shipmentDetailsImp.setSourceGuid(UUID.randomUUID());
        shipmentDetailsImp.setTenantId(33);
        ConsolidationDetails consolidationDetailsImp = shipmentDetailsImp.getConsolidationList().get(0);
        consolidationDetailsImp.setGuid(UUID.randomUUID());
        consolidationDetailsImp.setShipmentType(Constants.DIRECTION_IMP);
        consolidationDetailsImp.setReceivingBranch(33L);
        consolidationDetailsImp.setTriangulationPartnerList(List.of(33L));
        consolidationDetailsImp.setTriangulationPartner(33L);

        ShipmentDetails shipmentDetailsImp1 = jsonTestUtility.getCompleteShipment();
        shipmentDetailsImp1.setGuid(UUID.randomUUID());
        shipmentDetailsImp1.setDirection(Constants.DIRECTION_IMP);
        shipmentDetailsImp1.setSourceGuid(UUID.randomUUID());
        shipmentDetailsImp1.setTenantId(33);
        ConsolidationDetails consolidationDetailsImp1 = shipmentDetailsImp1.getConsolidationList().get(0);
        consolidationDetailsImp1.setGuid(UUID.randomUUID());
        consolidationDetailsImp1.setShipmentType(Constants.DIRECTION_IMP);
        consolidationDetailsImp1.setReceivingBranch(null);
        consolidationDetailsImp1.setTriangulationPartnerList(List.of(33L));
        consolidationDetailsImp1.setTriangulationPartner(33L);

        ShipmentDetails shipmentDetailsImp2 = new ShipmentDetails();
        shipmentDetailsImp2.setGuid(UUID.randomUUID());
        shipmentDetailsImp2.setDirection(Constants.DIRECTION_IMP);
        shipmentDetailsImp2.setSourceGuid(UUID.randomUUID());
        shipmentDetailsImp2.setTenantId(33);

        ShipmentDetails shipmentDetailsImp3 = new ShipmentDetails();
        shipmentDetailsImp3.setGuid(UUID.randomUUID());
        shipmentDetailsImp3.setDirection(Constants.DIRECTION_IMP);
        shipmentDetailsImp3.setSourceGuid(UUID.randomUUID());
        shipmentDetailsImp3.setTenantId(33);

        ShipmentDetails shipmentDetailsExp = jsonTestUtility.getCompleteShipment();
        shipmentDetailsExp.setGuid(UUID.randomUUID());
        shipmentDetailsExp.setDirection(Constants.DIRECTION_EXP);
        shipmentDetailsExp.setTenantId(33);
        ConsolidationDetails consolidationDetailsExp = shipmentDetailsExp.getConsolidationList().get(0);
        consolidationDetailsExp.setGuid(UUID.randomUUID());
        consolidationDetailsExp.setShipmentType(Constants.DIRECTION_EXP);
        consolidationDetailsExp.setReceivingBranch(null);

        LocalDateTime timeStamp = LocalDateTime.now();
        PostArValidationRequest postArValidationRequest = new PostArValidationRequest(List.of(shipmentDetails.getGuid(), shipmentDetailsDrt.getGuid(), shipmentDetailsImp.getGuid(), shipmentDetailsImp1.getGuid(), shipmentDetailsExp.getGuid(), shipmentDetailsImp2.getGuid(), shipmentDetailsImp3.getGuid()), timeStamp);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(postArValidationRequest);

        ShipmentDetails destShipment = new ShipmentDetails();
        destShipment.setGuid(UUID.randomUUID());
        destShipment.setSourceGuid(shipmentDetails.getGuid());
        destShipment.setTenantId(123);

        ShipmentDetails destShipmentForTriangulation = new ShipmentDetails();
        destShipmentForTriangulation.setGuid(UUID.randomUUID());
        destShipmentForTriangulation.setSourceGuid(shipmentDetails.getGuid());
        destShipmentForTriangulation.setTenantId(231);

        ShipmentDetails originShipment = new ShipmentDetails();
        originShipment.setGuid(shipmentDetailsImp.getSourceGuid());
        originShipment.setTenantId(432);
        ConsolidationDetails originShipConsole = new ConsolidationDetails();
        originShipConsole.setGuid(UUID.randomUUID());
        originShipConsole.setShipmentType(Constants.DIRECTION_IMP);
        originShipConsole.setReceivingBranch(33L);
        originShipConsole.setTriangulationPartnerList(List.of(33L));
        originShipConsole.setTriangulationPartner(33L);
        originShipment.setConsolidationList(new ArrayList<>(List.of(originShipConsole)));

        ShipmentDetails originShipment1 = new ShipmentDetails();
        originShipment1.setGuid(shipmentDetailsImp1.getSourceGuid());
        originShipment1.setTenantId(432);
        ConsolidationDetails originShipConsole1 = new ConsolidationDetails();
        originShipConsole1.setGuid(UUID.randomUUID());
        originShipConsole1.setShipmentType(Constants.DIRECTION_IMP);
        originShipConsole1.setReceivingBranch(null);
        originShipConsole1.setTriangulationPartnerList(List.of(33L));
        originShipConsole1.setTriangulationPartner(33L);
        originShipment1.setConsolidationList(new ArrayList<>(List.of(originShipConsole1)));

        ShipmentDetails originShipment2 = new ShipmentDetails();
        originShipment2.setGuid(shipmentDetailsImp2.getSourceGuid());
        originShipment2.setTenantId(432);
        ConsolidationDetails originShipConsole2 = new ConsolidationDetails();
        originShipConsole2.setGuid(UUID.randomUUID());
        originShipConsole2.setShipmentType(Constants.DIRECTION_EXP);
        originShipConsole2.setReceivingBranch(33L);
        originShipConsole2.setTriangulationPartnerList(List.of(35L));
        originShipConsole2.setTriangulationPartner(33L);
        originShipment2.setConsolidationList(new ArrayList<>(List.of(originShipConsole2)));

        ShipmentDetails triangulationShipment = new ShipmentDetails();
        triangulationShipment.setGuid(UUID.randomUUID());
        triangulationShipment.setSourceGuid(originShipment2.getGuid());
        triangulationShipment.setTenantId(35);

        ShipmentDetails originShipment3 = new ShipmentDetails();
        originShipment3.setGuid(shipmentDetailsImp3.getSourceGuid());
        originShipment3.setTenantId(432);
        ConsolidationDetails originShipConsole3 = new ConsolidationDetails();
        originShipConsole3.setGuid(UUID.randomUUID());
        originShipConsole3.setShipmentType(Constants.DIRECTION_EXP);
        originShipConsole3.setReceivingBranch(35L);
        originShipConsole3.setTriangulationPartnerList(List.of(33L));
        originShipConsole3.setTriangulationPartnerList(List.of(33L));
        originShipment3.setConsolidationList(new ArrayList<>(List.of(originShipConsole3)));

        ShipmentDetails receivingShipment = new ShipmentDetails();
        receivingShipment.setGuid(UUID.randomUUID());
        receivingShipment.setSourceGuid(originShipment3.getGuid());
        receivingShipment.setTenantId(35);



        String locationRefGuid = shipmentDetailsExp.getCarrierDetails().getDestination();
        Map<String, UnlocationsResponse> unlocationsResponseMap = new HashMap<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setLocationsReferenceGUID(locationRefGuid);
        unlocationsResponse.setCountry("IND");
        unlocationsResponseMap.put(locationRefGuid, unlocationsResponse);

        LogHistoryResponse logHistoryResponse = LogHistoryResponse.builder().entityGuid(shipmentDetails.getGuid()).entityPayload(jsonTestUtility.convertToJson(shipmentDetails)).build();
        List<UUID> shipGuids = new ArrayList<>(List.of(shipmentDetails.getGuid(), shipmentDetailsDrt.getGuid(), shipmentDetailsImp.getGuid(), shipmentDetailsImp1.getGuid(), shipmentDetailsExp.getGuid(), shipmentDetailsImp2.getGuid(), shipmentDetailsImp3.getGuid()));
        Set<UUID> shipGuidSet = new HashSet<>(shipGuids);
        Set<UUID> shipGuidSet1 = new HashSet<>(shipGuidSet);
        shipGuidSet1.remove(shipmentDetails.getGuid());
        Set<UUID> consoleGuids = new LinkedHashSet<>(List.of(shipmentDetails.getConsolidationList().get(0).getGuid(), shipmentDetailsDrt.getConsolidationList().get(0).getGuid(), shipmentDetailsImp.getConsolidationList().get(0).getGuid(),
                shipmentDetailsImp1.getConsolidationList().get(0).getGuid(), shipmentDetailsExp.getConsolidationList().get(0).getGuid()));
        Set<UUID> consoleGuids1 = new HashSet<>(consoleGuids);
        consoleGuids1.remove(shipmentDetails.getConsolidationList().get(0).getGuid());

        LogHistoryResponse consoleLogHistoryResponse = LogHistoryResponse.builder().entityGuid(shipmentDetails.getConsolidationList().get(0).getGuid()).entityPayload(jsonTestUtility.convertToJson(shipmentDetails.getConsolidationList().get(0))).build();

        when(shipmentDao.findShipmentsByGuids(shipGuidSet1)).thenReturn(List.of(shipmentDetailsDrt, shipmentDetailsImp, shipmentDetailsImp1, shipmentDetailsExp, shipmentDetailsImp2, shipmentDetailsImp3));
        when(shipmentDao.findShipmentsBySourceGuids(Set.of(shipmentDetails.getGuid(), originShipment.getGuid(), originShipment1.getGuid(), originShipment2.getGuid(), originShipment3.getGuid()))).thenReturn(List.of(destShipment, destShipmentForTriangulation, shipmentDetailsImp, shipmentDetailsImp1, shipmentDetailsImp2, triangulationShipment, receivingShipment));
        when(shipmentDao.findShipmentsByGuids(Set.of(shipmentDetailsImp.getSourceGuid(), shipmentDetailsImp1.getSourceGuid(), shipmentDetailsImp2.getSourceGuid(), shipmentDetailsImp3.getSourceGuid()))).thenReturn(List.of(originShipment, originShipment1, originShipment2, originShipment3));
        when(masterDataUtils.getLocationData(Set.of(locationRefGuid))).thenReturn(unlocationsResponseMap);
        when(consolidationDetailsDao.findConsolidationsByGuids(consoleGuids1))
                .thenReturn(List.of(shipmentDetailsDrt.getConsolidationList().get(0), shipmentDetailsImp.getConsolidationList().get(0), shipmentDetailsImp1.getConsolidationList().get(0), shipmentDetailsExp.getConsolidationList().get(0)));
        when(consolidationDetailsDao.findConsolidationsByGuids(Set.of(originShipment.getConsolidationList().get(0).getGuid(), originShipment1.getConsolidationList().get(0).getGuid(), originShipment2.getConsolidationList().get(0).getGuid(), originShipment3.getConsolidationList().get(0).getGuid())))
                .thenReturn(List.of(originShipment.getConsolidationList().get(0), originShipment1.getConsolidationList().get(0), originShipment2.getConsolidationList().get(0), originShipment3.getConsolidationList().get(0)));
        when(logsHistoryService.findByEntityGuidsAndTimeStamp(shipGuidSet.stream().toList(), timeStamp)).thenReturn(List.of(logHistoryResponse));
        when(jsonHelper.readFromJson(logHistoryResponse.getEntityPayload(), ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(logsHistoryService.findByEntityGuidsAndTimeStamp(consoleGuids.stream().toList(), timeStamp)).thenReturn(List.of(consoleLogHistoryResponse));
        when(jsonHelper.readFromJson(consoleLogHistoryResponse.getEntityPayload(), ConsolidationDetails.class)).thenReturn(shipmentDetails.getConsolidationList().get(0));
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.postArValidation(commonRequestModel);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testPostArValidation_Failure() {
        PostArValidationRequest request = new PostArValidationRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        assertThrows(RunnerException.class, () -> entityTransferService.postArValidation(commonRequestModel));
    }

    @Test
    void testCheckEntityExists() {
        var request = CheckEntityExistRequest.builder().entityId(UUID.randomUUID().toString()).entityType(Constants.Shipment).build();
        when(shipmentDao.findBySourceGuid(any())).thenReturn(List.of(new ShipmentDetails()));
        var responseEntity = entityTransferService.checkEntityExists(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCheckEntityExists2() {
        var request = CheckEntityExistRequest.builder().entityId(UUID.randomUUID().toString()).entityType(Constants.Consolidation).build();
        when(consolidationDetailsDao.findBySourceGuid(any())).thenReturn(List.of(new ConsolidationDetails()));
        var responseEntity = entityTransferService.checkEntityExists(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCheckEntityExists3() {
        var request = CheckEntityExistRequest.builder().entityId(UUID.randomUUID().toString()).entityType(Constants.Consolidation).build();
        when(consolidationDetailsDao.findBySourceGuid(any())).thenThrow(new RuntimeException(""));
        var responseEntity = entityTransferService.checkEntityExists(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCheckEntityExists4() {
        var request = CheckEntityExistRequest.builder().entityId(UUID.randomUUID().toString()).build();
        var responseEntity = entityTransferService.checkEntityExists(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }


    @Test
    void testPopulateTagDetails() {
        // Arrange
        Map<String, Object> tagDetails = new HashMap<>();
        String consolidationBranch = "TestBranch";

        // Act
        entityTransferService.populateTagDetails(tagDetails, consolidationBranch);

        // Assert
        assertEquals(1, tagDetails.size());
        assertTrue(tagDetails.containsKey("GS_ConsolidationBranch"));
        assertEquals(consolidationBranch, tagDetails.get("GS_ConsolidationBranch"));
    }

    @Test
    void testPopulateTagDetails_EmptyBranch() {
        // Arrange
        Map<String, Object> tagDetails = new HashMap<>();
        String consolidationBranch = "";

        entityTransferService.populateTagDetails(tagDetails, consolidationBranch);

        // Assert
        assertEquals(1, tagDetails.size());
        assertTrue(tagDetails.containsKey("GS_ConsolidationBranch"));
        assertEquals(consolidationBranch, tagDetails.get("GS_ConsolidationBranch"));
    }

    @Test
    void testPopulateTagDetails_NullBranch() {
        // Arrange
        Map<String, Object> tagDetails = new HashMap<>();
        String consolidationBranch = null;

        // Act
        entityTransferService.populateTagDetails(tagDetails, consolidationBranch);

        // Assert
        assertEquals(1, tagDetails.size());
        assertTrue(tagDetails.containsKey("GS_ConsolidationBranch"));
        assertNull(tagDetails.get("GS_ConsolidationBranch"));
    }

    @Test
    void testReplaceTagsValues_AllTagsPresent() {
        // Arrange
        Map<String, Object> tagDetails = new HashMap<>();
        tagDetails.put("GS_ConsolidationBranch", "Branch001");
        tagDetails.put("SD_ShipmentNumber", "SN12345");
        tagDetails.put("SD_SendingBranch", "SendingBranch01");

        String htmlElement = "<p>Branch: {GS_ConsolidationBranch}</p><p>Shipment: {SD_ShipmentNumber}</p><p>Branch: {SD_SendingBranch}</p>";

        // Act
        String result = entityTransferService.replaceTagsValues(tagDetails, htmlElement);

        // Assert
        String expected = "<p>Branch: Branch001</p><p>Shipment: SN12345</p><p>Branch: SendingBranch01</p>";
        assertEquals(expected, result);
    }

    @Test
    void testReplaceTagsValues_SomeTagsMissing() {
        // Arrange
        Map<String, Object> tagDetails = new HashMap<>();
        tagDetails.put("GS_ConsolidationBranch", "Branch001");

        String htmlElement = "<p>Branch: {GS_ConsolidationBranch}</p><p>Shipment: {SD_ShipmentNumber}</p><p>Branch: {SD_SendingBranch}</p>";

        // Act
        String result = entityTransferService.replaceTagsValues(tagDetails, htmlElement);

        // Assert
        String expected = "<p>Branch: Branch001</p><p>Shipment: {SD_ShipmentNumber}</p><p>Branch: {SD_SendingBranch}</p>";
        assertEquals(expected, result);
    }

    @Test
    void testReplaceTagsValues_EmptyTagDetails() {
        // Arrange
        Map<String, Object> tagDetails = new HashMap<>();

        String htmlElement = "<p>Branch: {GS_ConsolidationBranch}</p><p>Shipment: {SD_ShipmentNumber}</p>";

        // Act
        String result = entityTransferService.replaceTagsValues(tagDetails, htmlElement);

        // Assert
        String expected = "<p>Branch: {GS_ConsolidationBranch}</p><p>Shipment: {SD_ShipmentNumber}</p>";
        assertEquals(expected, result);
    }

    @Test
    void testReplaceTagsValues_NullValueInTagDetails() {
        // Arrange
        Map<String, Object> tagDetails = new HashMap<>();
        tagDetails.put("GS_ConsolidationBranch", null);

        String htmlElement = "<p>Branch: {GS_ConsolidationBranch}</p><p>Shipment: {SD_ShipmentNumber}</p>";

        // Act
        String result = entityTransferService.replaceTagsValues(tagDetails, htmlElement);

        // Assert
        String expected = "<p>Branch: </p><p>Shipment: {SD_ShipmentNumber}</p>";
        assertEquals(expected, result);
    }

    @Test
    void testReplaceTagsValues_NoTagsInHtmlElement() {
        // Arrange
        Map<String, Object> tagDetails = new HashMap<>();
        tagDetails.put("GS_ConsolidationBranch", "Branch001");

        String htmlElement = "<p>Branch: GS_ConsolidationBranch</p><p>Shipment: SD_ShipmentNumber</p>";

        // Act
        String result = entityTransferService.replaceTagsValues(tagDetails, htmlElement);

        // Assert
        String expected = "<p>Branch: GS_ConsolidationBranch</p><p>Shipment: SD_ShipmentNumber</p>";
        assertEquals(expected, result);
    }


    @Test
    void testGetRoleListByRoleId_ValidResponse() {
        MockitoAnnotations.openMocks(this);

        Integer roleId = 1;
        V1UsersEmailRequest request = new V1UsersEmailRequest();
        request.setRoleId(roleId);
        request.setTake(10);

        UsersRoleListResponse response1 = new UsersRoleListResponse();
        response1.setEmail("user1@example.com");

        UsersRoleListResponse response2 = new UsersRoleListResponse();
        response2.setEmail("user2@example.com");

        List<UsersRoleListResponse> mockResponse = new ArrayList<>();
        mockResponse.add(response1);
        mockResponse.add(response2);

        when(iv1Service.getUserEmailsByRoleId(any())).thenReturn(mockResponse);

        List<String> result = entityTransferService.getRoleListByRoleId(roleId);

        assertEquals(2, result.size());
        assertEquals("user1@example.com", result.get(0));
        assertEquals("user2@example.com", result.get(1));
    }


    @Test
    void testImportShipment_rejection() throws RunnerException {
        ImportShipmentRequest importShipmentRequest = ImportShipmentRequest.builder().taskId(1L).operation(TaskStatus.REJECTED.getDescription()).rejectRemarks("test rejected").build();
        var response = entityTransferService.importShipment(CommonRequestModel.buildRequest(importShipmentRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportShipment_nullPayload() {
        ImportShipmentRequest importShipmentRequest = ImportShipmentRequest.builder().taskId(1L).operation(TaskStatus.APPROVED.getDescription()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(importShipmentRequest).build();
        ValidationException ex = assertThrows(ValidationException.class, ()-> entityTransferService.importShipment(commonRequestModel));
        assertEquals("No Shipment payload present please check", ex.getMessage());
    }

    @Test
    void testImportShipment_Success_Create() throws RunnerException {
        EntityTransferShipmentDetails entityTransferShipmentDetails = jsonTestUtility.getImportShipmentData();
        ImportShipmentRequest importShipmentRequest = ImportShipmentRequest.builder()
                .entityData(entityTransferShipmentDetails)
                .taskId(1L)
                .operation(TaskStatus.APPROVED.getDescription())
                .build();

        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setTenantId(12);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());

        when(modelMapper.map(any(), eq(ShipmentRequest.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentRequest.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(any(), any())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);

        var response = entityTransferService.importShipment(CommonRequestModel.buildRequest(importShipmentRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportShipment_Success_Create_NetworkTransfer() throws RunnerException {
        EntityTransferShipmentDetails entityTransferShipmentDetails = jsonTestUtility.getImportShipmentData();
        ImportShipmentRequest importShipmentRequest = ImportShipmentRequest.builder()
                .entityData(entityTransferShipmentDetails)
                .taskId(1L)
                .isFromNte(true)
                .build();

        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setTenantId(12);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);

        when(modelMapper.map(any(), eq(ShipmentRequest.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentRequest.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(any(), any())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);
        mockShipmentSettings();

        var response = entityTransferService.importShipment(CommonRequestModel.buildRequest(importShipmentRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportShipment_Success_Create_NetworkTransfer_validation() {
        EntityTransferShipmentDetails entityTransferShipmentDetails = jsonTestUtility.getImportShipmentData();
        ImportShipmentRequest importShipmentRequest = ImportShipmentRequest.builder()
                .entityData(entityTransferShipmentDetails)
                .taskId(1L)
                .isFromNte(true)
                .build();

        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setTenantId(12);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(false);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(importShipmentRequest);

        when(modelMapper.map(any(), eq(ShipmentRequest.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentRequest.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(any(), any())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);
        mockShipmentSettings();
        assertThrows(ValidationException.class, ()-> entityTransferService.importShipment(commonRequestModel));
    }

    @Test
    void testImportShipment_Success_Update() throws RunnerException {
        EntityTransferShipmentDetails entityTransferShipmentDetails = jsonTestUtility.getImportShipmentData();
        entityTransferShipmentDetails.setAdditionalDocs(null);
        ImportShipmentRequest importShipmentRequest = ImportShipmentRequest.builder()
                .entityData(entityTransferShipmentDetails)
                .taskId(1L)
                .operation(TaskStatus.APPROVED.getDescription())
                .build();

        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setTenantId(12);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());

        ShipmentDetails shipmentDetails = objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentDetails.class);
        shipmentDetails.setId(2L);
        shipmentDetails.setGuid(shipmentDetailsResponse.getGuid());
        shipmentDetails.setTenantId(12);

        ShipmentRequest shipmentRequest = objectMapperTest.convertValue(shipmentDetails, ShipmentRequest.class);

        when(modelMapper.map(any(), eq(ShipmentRequest.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentRequest.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(any(), any())).thenReturn(List.of(shipmentDetails));
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(shipmentRequest);
        when(shipmentService.completeUpdateShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);

        var response = entityTransferService.importShipment(CommonRequestModel.buildRequest(importShipmentRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }


    @Test
    void testImportConsolidation_rejectTask() throws RunnerException {
        ImportConsolidationRequest importConsolidationRequest = ImportConsolidationRequest.builder().taskId(1L).operation(TaskStatus.REJECTED.getDescription()).rejectRemarks("test rejected").build();
        var response =  entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportConsolidation_nullPayload() {
        ImportConsolidationRequest importConsolidationRequest = ImportConsolidationRequest.builder().taskId(1L).operation(TaskStatus.APPROVED.getDescription()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(importConsolidationRequest).build();
        ValidationException ex = assertThrows(ValidationException.class, ()-> entityTransferService.importConsolidation(commonRequestModel));
        assertEquals("No consolidation payload present please check", ex.getMessage());
    }

    @Test
    void testImportConsolidation_Success_Create_Air() throws RunnerException {
        UserContext.getUser().setTenantId(728);
        EntityTransferConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getImportConsolidationAir();
        ImportConsolidationRequest importConsolidationRequest = ImportConsolidationRequest.builder()
                .operation(TaskStatus.APPROVED.getDescription())
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .build();
        EntityTransferShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().get(0);
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        ConsolidationDetailsRequest consolidationDetailsRequest = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationDetailsRequest.class);
        ConsolidationDetailsResponse consolidationDetailsResponse = new ConsolidationDetailsResponse();
        consolidationDetailsResponse.setId(3L);
        consolidationDetailsResponse.setGuid(UUID.randomUUID());
        consolidationDetailsResponse.setTenantId(entityTransferConsolidationDetails.getSendToBranch());
        consolidationDetailsResponse.setConsolidationNumber(entityTransferConsolidationDetails.getConsolidationNumber());

        when(consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid())).thenReturn(List.of());
        when(modelMapper.map(any(), eq(ShipmentRequest.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentRequest.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);
        when(modelMapper.map(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(consolidationDetailsRequest);
        when(consolidationService.createConsolidationFromEntityTransfer(any())).thenReturn(consolidationDetailsResponse);

        var response = entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportConsolidation_Success_Update_Sea() throws RunnerException {
        UserContext.getUser().setTenantId(728);
        EntityTransferConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getImportConsolidationSea();
        ImportConsolidationRequest importConsolidationRequest = ImportConsolidationRequest.builder()
                .operation(TaskStatus.APPROVED.getDescription())
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .build();
        EntityTransferShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().get(0);
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        ConsolidationDetailsRequest consolidationDetailsRequest = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationDetailsRequest.class);
        ContainerResponse containerResponse = objectMapperTest.convertValue(entityTransferConsolidationDetails.getContainersList().get(0), ContainerResponse.class);
        ConsolidationDetailsResponse consolidationDetailsResponse = new ConsolidationDetailsResponse();
        consolidationDetailsResponse.setId(3L);
        consolidationDetailsResponse.setGuid(UUID.randomUUID());
        consolidationDetailsResponse.setTenantId(entityTransferConsolidationDetails.getSendToBranch());
        consolidationDetailsResponse.setConsolidationNumber(entityTransferConsolidationDetails.getConsolidationNumber());
        consolidationDetailsResponse.setContainersList(List.of(containerResponse));

        ConsolidationDetails oldConsolidationDetails = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationDetails.class);
        oldConsolidationDetails.setId(2L);
        oldConsolidationDetails.setGuid(consolidationDetailsResponse.getGuid());
        oldConsolidationDetails.setTenantId(728);

        ShipmentDetails oldShipmentDetails = objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentDetails.class);
        oldShipmentDetails.setId(2L);
        oldShipmentDetails.setGuid(shipmentDetailsResponse.getGuid());
        oldShipmentDetails.setTenantId(728);

        Containers containers = objectMapperTest.convertValue(entityTransferConsolidationDetails.getContainersList().get(0), Containers.class);
        Packing packing = objectMapperTest.convertValue(entityTransferShipmentDetails.getPackingList().get(0), Packing.class);

        ShipmentRequest shipmentRequest = objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentRequest.class);

        when(consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid())).thenReturn(List.of(oldConsolidationDetails));
        when(modelMapper.map(any(), eq(ShipmentRequest.class))).thenReturn(shipmentRequest);
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of(oldShipmentDetails));
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(shipmentRequest);
        when(shipmentService.completeUpdateShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);

        doNothing().when(modelMapper).map(any(EntityTransferShipmentDetails.class), any(ShipmentRequest.class));
        when(modelMapper.map(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(consolidationDetailsRequest);
        when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(consolidationDetailsRequest);
        when(consolidationService.completeUpdateConsolidationFromEntityTransfer(any())).thenReturn(consolidationDetailsResponse);

        when(containerDao.findByConsolidationId(anyLong())).thenReturn(List.of(containers));
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(packing)));
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(Arrays.asList(oldShipmentDetails));

        var response = entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportConsolidation_Success_Create_Air_interBranch() throws RunnerException {
        UserContext.getUser().setTenantId(728);
        EntityTransferConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getImportConsolidationAir();
        ImportConsolidationRequest importConsolidationRequest = ImportConsolidationRequest.builder()
                .operation(TaskStatus.APPROVED.getDescription())
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .build();
        EntityTransferShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().get(0);
        entityTransferShipmentDetails.setSendToBranch(720);
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        ConsolidationDetailsRequest consolidationDetailsRequest = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationDetailsRequest.class);
        ConsolidationDetailsResponse consolidationDetailsResponse = new ConsolidationDetailsResponse();
        consolidationDetailsResponse.setId(3L);
        consolidationDetailsResponse.setGuid(UUID.randomUUID());
        consolidationDetailsResponse.setTenantId(entityTransferConsolidationDetails.getSendToBranch());
        consolidationDetailsResponse.setConsolidationNumber(entityTransferConsolidationDetails.getConsolidationNumber());

        when(consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid())).thenReturn(List.of());
        when(modelMapper.map(any(), eq(ShipmentRequest.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentRequest.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);
        when(modelMapper.map(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(consolidationDetailsRequest);
        when(consolidationService.createConsolidationFromEntityTransfer(any())).thenReturn(consolidationDetailsResponse);

        var response = entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportConsolidation_Success_Create_Air_interBranch_NTE() throws RunnerException {
        UserContext.getUser().setTenantId(728);
        EntityTransferConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getImportConsolidationAir();
        ImportConsolidationRequest importConsolidationRequest = ImportConsolidationRequest.builder()
                .isFromNte(true)
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .build();
        EntityTransferShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().get(0);
        entityTransferShipmentDetails.setSendToBranch(720);
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        ConsolidationDetailsRequest consolidationDetailsRequest = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationDetailsRequest.class);
        ConsolidationDetailsResponse consoleDetailsResponse = new ConsolidationDetailsResponse();
        consoleDetailsResponse.setId(3L);
        consoleDetailsResponse.setGuid(UUID.randomUUID());
        consoleDetailsResponse.setTenantId(entityTransferConsolidationDetails.getSendToBranch());
        consoleDetailsResponse.setConsolidationNumber(entityTransferConsolidationDetails.getConsolidationNumber());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);

        when(consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid())).thenReturn(List.of());
        when(modelMapper.map(any(), eq(ShipmentRequest.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentRequest.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);
        when(modelMapper.map(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(consolidationDetailsRequest);
        when(consolidationService.createConsolidationFromEntityTransfer(any())).thenReturn(consoleDetailsResponse);
        mockShipmentSettings();

        var response = entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportConsolidation_Success_Create_Air_interBranch_NTE_Validation() {
        UserContext.getUser().setTenantId(728);
        EntityTransferConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getImportConsolidationAir();
        ImportConsolidationRequest importConsolidationRequest = ImportConsolidationRequest.builder()
                .isFromNte(true)
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .build();
        EntityTransferShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().get(0);
        entityTransferShipmentDetails.setSendToBranch(720);
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        ConsolidationDetailsRequest consolidationDetailsRequest = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationDetailsRequest.class);
        ConsolidationDetailsResponse consoleDetailsResponse = new ConsolidationDetailsResponse();
        consoleDetailsResponse.setId(3L);
        consoleDetailsResponse.setGuid(UUID.randomUUID());
        consoleDetailsResponse.setTenantId(entityTransferConsolidationDetails.getSendToBranch());
        consoleDetailsResponse.setConsolidationNumber(entityTransferConsolidationDetails.getConsolidationNumber());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(false);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(importConsolidationRequest);

        when(consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid())).thenReturn(List.of());
        when(modelMapper.map(any(), eq(ShipmentRequest.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentRequest.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);
        when(modelMapper.map(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(consolidationDetailsRequest);
        when(consolidationService.createConsolidationFromEntityTransfer(any())).thenReturn(consoleDetailsResponse);
        mockShipmentSettings();

        assertThrows(ValidationException.class, () -> entityTransferService.importConsolidation(commonRequestModel));
    }


    @Test
    void testSendGroupedEmailForShipmentImport_Success() {
        // Given
        String consoleSourceBranchTenantName = "consoleBranch";
        when(shipmentDetails.getTenantId()).thenReturn(100);
        when(shipmentDetails.getSourceTenantId()).thenReturn(123L);
        when(shipmentDetails.getShipmentId()).thenReturn("1");
        when(shipmentDetails.getHouseBill()).thenReturn("hbn");
        when(shipmentDetails.getMasterBill()).thenReturn("mbn");
        when(shipmentDetails.getShipmentCreatedOn()).thenReturn(LocalDateTime.now());

        List<ShipmentDetails> shipmentDetailsList = List.of(
                shipmentDetails,
                shipmentDetails
        );

        // Mocks
        doNothing().when(commonUtils).setInterBranchContextForHub();
        doNothing().when(commonUtils).getToAndCCEmailIdsFromTenantSettings(anySet(), anyMap());
        EmailTemplatesRequest emailTemplatesRequest = mock(EmailTemplatesRequest.class);
        when(jsonHelper.convertValueToList(any(), eq(EmailTemplatesRequest.class))).thenReturn(List.of(emailTemplatesRequest));
        when(iv1Service.getEmailTemplates(any())).thenReturn(V1DataResponse.builder().build());
        doAnswer(invocation -> {
            Set<String> toEmailIds = invocation.getArgument(0);
            toEmailIds.add("toEmail@example.com");
            return null;
        }).when(commonUtils).getToAndCcEmailMasterLists(anySet(), anySet(), anyMap(), anyInt(), anyBoolean());

        Map<Integer, Object> mockV1Map = new HashMap<>();
        mockV1Map.put(123, new Object());
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(mockV1Map);

        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(v1TenantResponse);
        when(v1TenantResponse.getTenantName()).thenReturn("abcd");

        // When
        entityTransferService.sendGroupedEmailForShipmentImport(shipmentDetailsList, consoleSourceBranchTenantName);

        // Then
        verify(commonUtils, times(1)).setInterBranchContextForHub();
        verify(commonUtils, times(1)).getToAndCCEmailIdsFromTenantSettings(anySet(), anyMap());
    }


    @Test
    void testSendConsolidationEmailNotification_Success() {
        List<Integer> destinationBranches = List.of(1,2,3);

        when(iv1Service.getEmailTemplates(any())).thenReturn(V1DataResponse.builder().build());
        Map<Integer, Object> mockV1Map = new HashMap<>();
        TenantContext.setCurrentTenant(1);
        mockV1Map.put(1, new Object());
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(mockV1Map);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(consolidationDetails.getConsolidationNumber()).thenReturn("1");
        when(consolidationDetails.getShipmentsList()).thenReturn(List.of(shipmentDetails));
        when(consolidationDetails.getTransportMode()).thenReturn("SEA");
        when(consolidationDetails.getBol()).thenReturn("bol");

        Map<String, List<Integer>> shipmentGuidSendToBranch = new HashMap<>();
        shipmentGuidSendToBranch.put("1", List.of(1,2,3));

        entityTransferService.sendConsolidationEmailNotification(consolidationDetails, destinationBranches, shipmentGuidSendToBranch);

        verify(v1ServiceUtil, times(1)).getTenantDetails(anyList());

    }
    @Test
    void testSendConsolidationEmailNotification_Success1() {
        List<Integer> destinationBranches = List.of(1,2,3);

        when(iv1Service.getEmailTemplates(any())).thenReturn(V1DataResponse.builder().build());
        Map<Integer, Object> mockV1Map = new HashMap<>();
        TenantContext.setCurrentTenant(1);
        mockV1Map.put(1, new Object());
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(mockV1Map);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(consolidationDetails.getConsolidationNumber()).thenReturn("1");
        when(consolidationDetails.getShipmentsList()).thenReturn(List.of(shipmentDetails));
        when(consolidationDetails.getTransportMode()).thenReturn("SEA");
        when(consolidationDetails.getBol()).thenReturn("bol");

        Map<String, List<Integer>> shipmentGuidSendToBranch = null;

        entityTransferService.sendConsolidationEmailNotification(consolidationDetails, destinationBranches, shipmentGuidSendToBranch);

        verify(v1ServiceUtil, times(1)).getTenantDetails(anyList());

    }

    @Test
    void testSendShipmentEmailNotification_Success() {
        List<Integer> destinationBranches = List.of(1,2,3);

        when(iv1Service.getEmailTemplates(any())).thenReturn(V1DataResponse.builder().build());
        Map<Integer, Object> mockV1Map = new HashMap<>();
        TenantContext.setCurrentTenant(1);
        mockV1Map.put(1, new Object());

        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(mockV1Map);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(shipmentDetails.getShipmentId()).thenReturn("1");
        when(shipmentDetails.getHouseBill()).thenReturn("hbn");
        when(shipmentDetails.getMasterBill()).thenReturn("mbn");
        when(shipmentSettingsDao.getSettingsByTenantIds(destinationBranches)).thenReturn(List.of(shipmentSettingsDetails));
        when(shipmentSettingsDetails.getShipmentConsoleImportApproverRole()).thenReturn("123");

        entityTransferService.sendShipmentEmailNotification(shipmentDetails, destinationBranches);

        verify(shipmentSettingsDao, times(1)).getSettingsByTenantIds(anyList());

    }

    @Test
    void testSendShipmentForNTESuccess() {
        Long shipmentId = 1L;
        int mockTenantId = 10;

        SendShipmentRequest sendShipmentRequest = new SendShipmentRequest();
        sendShipmentRequest.setSendToBranch(List.of(1,2,3));
        sendShipmentRequest.setShipId(shipmentId);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendShipmentRequest);

        ShipmentDetails mockShipmentDetails = jsonTestUtility.getCompleteShipment();
        mockShipmentDetails.setTenantId(mockTenantId);
        EntityTransferShipmentDetails mockETPayload = objectMapperTest.convertValue(mockShipmentDetails, EntityTransferShipmentDetails.class);
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );

        NetworkTransfer mockNetworkTransfer = jsonTestUtility.getNetworkTransfer();

        // Mocking
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(mockShipmentDetails));
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));
        when(jsonHelper.convertValue(any(), eq(EntityTransferShipmentDetails.class))).thenReturn(mockETPayload);
        when(shipmentService.fetchAllMasterDataByKey(any(), any())).thenReturn(new HashMap<String, Object>());
        when(networkTransferDao.findByTenantAndEntity(1, 155357L, Constants.SHIPMENT)).thenReturn(Optional.of(mockNetworkTransfer));
        when(networkTransferDao.findByTenantAndEntity(2, 155357L, Constants.SHIPMENT)).thenReturn(Optional.empty());
        when(networkTransferDao.findByTenantAndEntity(2, 155357L, Constants.SHIPMENT)).thenReturn(Optional.empty());

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(Boolean.TRUE);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());

        var httpResponse = entityTransferService.sendShipment(commonRequestModel);

        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        verify(shipmentDao, times(1)).saveEntityTransfer(any(), any());
    }

    @Test
    void testSendConsolidationForNTESuccess() {
        int mockTenantId = 10;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTenantId(mockTenantId);
        consolidationDetails.getShipmentsList().forEach(i -> i.setTenantId(mockTenantId));
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();

        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().get(0).getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66, 11))
                .consolId(consolidationDetails.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();

        EntityTransferConsolidationDetails mockETPayload = objectMapperTest.convertValue(consolidationDetails, EntityTransferConsolidationDetails.class);
        ShipmentDetails mockLinkedShipment = new ShipmentDetails();
        EntityTransferShipmentDetails mockETShipment = new EntityTransferShipmentDetails();
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );
        NetworkTransfer mockNetworkTransfer = jsonTestUtility.getNetworkTransfer();

        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.of(consolidationDetails));
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferShipmentDetails.class))).thenReturn(mockETShipment);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));
        when(networkTransferDao.findByTenantAndEntity(66, 2258L, Constants.CONSOLIDATION)).thenReturn(Optional.of(mockNetworkTransfer));
        when(networkTransferDao.findByTenantAndEntity(11, 2258L, Constants.CONSOLIDATION)).thenReturn(Optional.empty());

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(Boolean.TRUE);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testSendConsolidationWithTriangulationPartner() {
        int mockTenantId = 10;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTenantId(mockTenantId);
        consolidationDetails.getShipmentsList().forEach(i -> i.setTenantId(mockTenantId));
        consolidationDetails.setTriangulationPartnerList(List.of(66L));  // Mock triangulation partner

        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();
        V1DataResponse v1DataResponseOrg = V1DataResponse.builder().entities(List.of(organizations)).build();

        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().get(0).getGuid().toString(), List.of(UUID.randomUUID().toString()));

        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66)) // Send to the triangulation partner
                .consolId(consolidationDetails.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();

        EntityTransferConsolidationDetails mockETPayload = objectMapperTest.convertValue(consolidationDetails, EntityTransferConsolidationDetails.class);
        ShipmentDetails mockLinkedShipment = new ShipmentDetails();
        EntityTransferShipmentDetails mockETShipment = new EntityTransferShipmentDetails();
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );

        // Mocks
        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.of(consolidationDetails));
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferShipmentDetails.class))).thenReturn(mockETShipment);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));

        mockShipmentSettings();

        // Call method
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));

        // Assertions
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

        // Verify that the shipment direction for the triangulation partner was set to DIRECTION_CTS
        verify(jsonHelper, times(1)).convertValue(any(), eq(EntityTransferShipmentDetails.class)); // Ensure convertValue was called

        // Check the direction for the triangulation partner shipment
        assertEquals(Constants.DIRECTION_CTS, mockETShipment.getDirection(), "Direction should be set to DIRECTION_CTS for triangulation partner.");
    }



    private Runnable mockRunnable() {
        return null;
    }

}