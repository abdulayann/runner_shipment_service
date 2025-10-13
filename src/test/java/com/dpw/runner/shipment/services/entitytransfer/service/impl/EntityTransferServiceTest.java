package com.dpw.runner.shipment.services.entitytransfer.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.interfaces.IBridgeServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.LoggingConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.request.TaskCreateRequest;
import com.dpw.runner.shipment.services.dto.v1.request.V1UsersEmailRequest;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferSource;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.entity.enums.PrintType;
import com.dpw.runner.shipment.services.entity.enums.TaskStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.ArValidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.CheckTaskExistResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.ValidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.enums.TransferStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.migration.service.interfaces.IConsolidationMigrationV3Service;
import com.dpw.runner.shipment.services.migration.service.interfaces.IShipmentMigrationV3Service;
import com.dpw.runner.shipment.services.migration.utils.NotesUtil;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.service.impl.NetworkTransferService;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.impl.ConsolidationSync;
import com.dpw.runner.shipment.services.syncing.impl.ShipmentSync;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.slf4j.MDC;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.IOException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.stream.Collectors;
import static com.dpw.runner.shipment.services.commons.constants.ShipmentConstants.CONSOLIDATION;
import static com.dpw.runner.shipment.services.commons.constants.ShipmentConstants.SHIPMENT;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

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
    private ConsolidationDetails consoleDetails;

    @Mock
    private ConsolidationDetailsResponse consoleDetailsResponse;
    @Mock
    private ShipmentDetails shipmentData;
    @Mock
    private V1TenantResponse v1TenantResponse;
    @Mock
    private TenantModel mockedTenantModel;
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
    private IBridgeServiceAdapter bridgeServiceAdapter;
    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Mock
    V1ServiceUtil v1ServiceUtil;
    @Mock
    private IEventService eventService;
    @InjectMocks
    private EntityTransferService entityTransferService;
    @Mock
    private ShipmentSync shipmentSync;
    @Mock
    private ConsolidationSync consolidationSync;
    @Mock
    private INotificationDao notificationDao;
    @Mock
    private ExecutorService executorService;
    @Mock
    private IShipmentMigrationV3Service shipmentMigrationService;
    @Mock
    private IConsolidationMigrationV3Service consolidationMigrationV3Service;
    @Mock
    private NotesUtil notesUtil;
    @Mock
    private IApplicationConfigService applicationConfigService;

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
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setTenantDisplayName("abc");
        mockUser.setDisplayName("abc");
        mockUser.setUserId(1L);
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void testSendShipmentThrowsExceptionIfSendToBranchIsEmpty() {
        SendShipmentRequest sendShipmentRequest = new SendShipmentRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendShipmentRequest);

        assertThrows(ValidationException.class, () ->
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
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();
        mockShipmentSettings();

        // Mocking
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(mockShipmentDetails));
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));

        assertThrows(ValidationException.class, () -> entityTransferService.sendShipment(commonRequestModel));

    }
    @Test
    void testSendShipmentSuccess() throws RunnerException {
        Long shipmentId = 1L;
        int mockTenantId = 10;

        SendShipmentRequest sendShipmentRequest = new SendShipmentRequest();
        sendShipmentRequest.setSendToBranch(List.of(1,2,3));
        sendShipmentRequest.setShipId(shipmentId);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendShipmentRequest);

        ShipmentDetails mockShipmentDetails = jsonTestUtility.getCompleteShipment();
        mockShipmentDetails.setTenantId(mockTenantId);
        EntityTransferV3ShipmentDetails mockETPayload = objectMapperTest.convertValue(mockShipmentDetails, EntityTransferV3ShipmentDetails.class);
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
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETPayload);
        when(shipmentService.fetchAllMasterDataByKey(any(), any())).thenReturn(new HashMap<String, Object>());

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(3);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(4);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        mockShipmentSettings();
        var httpResponse = entityTransferService.sendShipment(commonRequestModel);

        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        verify(shipmentDao, times(1)).saveEntityTransfer(any(), any());
        verify(tasksService, times(3)).createTask(any());

    }

    @Test
    void testSendShipmentSuccess_WithTriangulationPartner() throws RunnerException {
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
        TriangulationPartner triangulationPartner = TriangulationPartner.builder().triangulationPartner(3L).isAccepted(false).build();
        TriangulationPartner triangulationPartner1 = TriangulationPartner.builder().triangulationPartner(4L).isAccepted(false).build();
        mockShipmentDetails.setTriangulationPartnerList(List.of(triangulationPartner, triangulationPartner1)); // Set triangulation partners

        // Mock Payload and Response
        EntityTransferV3ShipmentDetails mockETPayload = new EntityTransferV3ShipmentDetails();
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );

        // Mocking
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(mockShipmentDetails));
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETPayload);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);

        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>();
        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(3);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(4);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        shipmentSettingsDetailsList.addAll(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        mockShipmentSettings();
        var httpResponse = entityTransferService.sendShipment(commonRequestModel);

        // Assertions
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }


    @Test
    void testSendShipmentSuccess_WithTriangulationPartner2() throws RunnerException {
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
        TriangulationPartner triangulationPartner = TriangulationPartner.builder().triangulationPartner(3L).isAccepted(false).build();
        TriangulationPartner triangulationPartner1 = TriangulationPartner.builder().triangulationPartner(4L).isAccepted(false).build();
        mockShipmentDetails.setTriangulationPartnerList(List.of(triangulationPartner, triangulationPartner1)); // Set triangulation partners

        // Mock Payload and Response
        EntityTransferV3ShipmentDetails mockETPayload = new EntityTransferV3ShipmentDetails();
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );

        // Mocking
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(mockShipmentDetails));
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETPayload);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipmentDetails);
        when(shipmentMigrationService.mapShipmentV2ToV3(any(), eq(null), eq(false))).thenReturn(mockShipmentDetails);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(ShipmentDetailsResponse.builder().build());

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        shipmentSettingsDetails2.setIsRunnerV3Enabled(true);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(3);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(4);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        mockShipmentSettings();
        var httpResponse = entityTransferService.sendShipment(commonRequestModel);

        // Assertions
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }



    @Test
    void testSendConsolidationFailureInCaseOfEmptySendToOrg() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().iterator().next().getGuid().toString(), List.of(UUID.randomUUID().toString()));
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
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().iterator().next().getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails.getId())
                .sendToOrg(null)
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendConsolidationRequest);
        mockShipmentSettings();
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, ()-> entityTransferService.sendConsolidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationSuccess() throws RunnerException {
        int mockTenantId = 10;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTenantId(mockTenantId);
        consolidationDetails.getShipmentsList().forEach(i -> i.setTenantId(mockTenantId));
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();

        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().iterator().next().getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();

        EntityTransferV3ConsolidationDetails mockETPayload = objectMapperTest.convertValue(consolidationDetails, EntityTransferV3ConsolidationDetails.class);
        ShipmentDetails mockLinkedShipment = new ShipmentDetails();
        EntityTransferV3ShipmentDetails mockETShipment = new EntityTransferV3ShipmentDetails();
        mockETShipment.setGuid(UUID.randomUUID());
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );

        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.of(consolidationDetails));
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETShipment);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));
        mockShipmentSettings();
        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(66);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(69);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testSendConsolidationSuccess2() throws RunnerException {
        int mockTenantId = 10;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTenantId(mockTenantId);
        consolidationDetails.getShipmentsList().forEach(i -> i.setTenantId(mockTenantId));
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();

        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().iterator().next().getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();

        EntityTransferV3ConsolidationDetails mockETPayload = objectMapperTest.convertValue(consolidationDetails, EntityTransferV3ConsolidationDetails.class);
        ShipmentDetails mockLinkedShipment = new ShipmentDetails();
        EntityTransferV3ShipmentDetails mockETShipment = new EntityTransferV3ShipmentDetails();
        mockETShipment.setGuid(UUID.randomUUID());
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );

        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.of(consolidationDetails));
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);

        when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
        Map<String, BigDecimal> codeTeuMap = new HashMap<>();
        when(consolidationMigrationV3Service.mapConsoleV2ToV3(any(), any(), eq(false), eq(codeTeuMap),any(), any())).thenReturn(consolidationDetails);

        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(ShipmentDetailsResponse.builder().build());
        when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsResponse.class))).thenReturn(ConsolidationDetailsResponse.builder().build());

        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETShipment);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));
        mockShipmentSettings();
        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(66);
        shipmentSettingsDetails3.setIsRunnerV3Enabled(true);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(69);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }


    @Test
    void testSendConsolidationSuccess3() throws RunnerException {
        int mockTenantId = 10;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTenantId(mockTenantId);
        consolidationDetails.getShipmentsList().forEach(i -> i.setTenantId(mockTenantId));
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();

        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().iterator().next().getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();

        EntityTransferV3ConsolidationDetails mockETPayload = objectMapperTest.convertValue(consolidationDetails, EntityTransferV3ConsolidationDetails.class);
        ShipmentDetails mockLinkedShipment = new ShipmentDetails();
        EntityTransferV3ShipmentDetails mockETShipment = new EntityTransferV3ShipmentDetails();
        mockETShipment.setGuid(UUID.randomUUID());
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );

        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.of(consolidationDetails));
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);

        when(commonUtils.getCurrentTenantSettings()).thenReturn(V1TenantSettingsResponse.builder().WeightDecimalPlace(1).build());

        when(jsonHelper.convertValue(any(), eq(ConsolidationDetails.class))).thenReturn(consolidationDetails);
        Map<String, BigDecimal> codeTeuMap = new HashMap<>();
        when(consolidationMigrationV3Service.mapConsoleV2ToV3(any(), any(), eq(false), eq(codeTeuMap),any(), any())).thenReturn(consolidationDetails);

        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(ShipmentDetailsResponse.builder().build());
        when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsResponse.class))).thenReturn(ConsolidationDetailsResponse.builder().build());

        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETShipment);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));
        mockShipmentSettings();
        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(66);
        shipmentSettingsDetails3.setIsRunnerV3Enabled(true);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(69);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testConsolidationInterBranchSuccess() throws RunnerException {
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
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().iterator().next().getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails.getId())
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .shipmentGuidSendToBranch(Map.ofEntries(
                        Map.entry(shipGuid.toString(), List.of(69))
                ))
                .build();

        EntityTransferV3ConsolidationDetails mockETPayload = objectMapperTest.convertValue(consolidationDetails, EntityTransferV3ConsolidationDetails.class);
        ShipmentDetails mockLinkedShipment = consolidationDetails.getShipmentsList().iterator().next();
        EntityTransferV3ShipmentDetails mockETShipment = objectMapperTest.convertValue(mockLinkedShipment, EntityTransferV3ShipmentDetails.class);
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
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETShipment);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));
        when(v1ServiceUtil.fetchCoLoadInfo(any(), any())).thenReturn(coLoadMap);
        mockShipmentSettings();
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>();
        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(1);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(10);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(66);
        shipmentSettingsDetailsList.add(shipmentSettingsDetails2);
        shipmentSettingsDetailsList.add(shipmentSettingsDetails1);
        shipmentSettingsDetailsList.add(shipmentSettingsDetails3);
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

    }


    @Test
    // change IMP -> EXP || EXP -> IMP if receiving branch == send to branch
    void testConsolidationSuccessReverseDirectionInResponsePayload() throws RunnerException {
        int mockTenantId = 10;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setReceivingBranch(66L);
        consolidationDetails.setTenantId(mockTenantId);
        consolidationDetails.getShipmentsList().forEach(i -> i.setTenantId(mockTenantId));
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();

        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().iterator().next().getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();

        EntityTransferV3ConsolidationDetails mockETPayload = objectMapperTest.convertValue(consolidationDetails, EntityTransferV3ConsolidationDetails.class);
        ShipmentDetails mockLinkedShipment = new ShipmentDetails();
        EntityTransferV3ShipmentDetails mockETShipment = new EntityTransferV3ShipmentDetails();
        mockETShipment.setGuid(UUID.randomUUID());
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );

        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.of(consolidationDetails));
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETShipment);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));
        mockShipmentSettings();
        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(66);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(69);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

    }

//    @Test
    void testSendConsolidation_Failure1() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();
        V1DataResponse v1DataResponseOrg = V1DataResponse.builder().entities(List.of(organizations)).build();
        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().iterator().next().getGuid().toString(), List.of(UUID.randomUUID().toString()));
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
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().iterator().next().getGuid().toString(), List.of(UUID.randomUUID().toString()));
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
        when(hblDao.findByShipmentId(consolidationDetails.getShipmentsList().iterator().next().getId())).thenReturn(List.of(new Hbl()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidationValidation(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
    }

    @Test
    void testSendConsolidationValidation_Success_NetworkTransfer() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails1.getId()).build();
        ValidationResponse response = ValidationResponse.builder().success(true).build();
        consolidationDetails1.getShipmentsList().iterator().next().getAdditionalDetails().setPrintedOriginal(true);
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails1));
        when(hblDao.findByShipmentId(consolidationDetails1.getShipmentsList().iterator().next().getId())).thenReturn(List.of(new Hbl()));
        mockShipmentSettings();
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
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }
    @Test
    void testSendConsolidationValidation_Failure_NetworkTransferConsoleFieldsException() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setBol(null);
        consolidationDetails1.getCarrierDetails().setVoyage(null);
        consolidationDetails1.getCarrierDetails().setVessel(null);
        consolidationDetails1.getCarrierDetails().setEta(null);
        consolidationDetails1.getCarrierDetails().setEtd(null);
        consolidationDetails1.getCarrierDetails().setShippingLine(null);
        consolidationDetails1.setReceivingBranch(null);
        consolidationDetails1.setTriangulationPartnerList(null);
        consolidationDetails1.setTriangulationPartner(null);
        consolidationDetails1.setSendingAgent(null);
        consolidationDetails1.setReceivingAgent(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails1));
        Exception exception = assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
        assertEquals("Please enter the MBL, Eta, Etd, Shipping line, Vessel, Voyage, Origin agent, Destination agent, one of the branches in the entity transfer details section for the consolidation and generate HBL for the shipment/s SHP000110207 to transfer the files.", exception.getMessage());
    }

    @Test
    void testSendConsolidationValidation_Failure_NetworkTransferConsoleFieldsException1() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setBol("23459876");
        consolidationDetails1.getCarrierDetails().setVoyage("34432");
        consolidationDetails1.getCarrierDetails().setVessel("ABC");
        consolidationDetails1.getCarrierDetails().setEta(LocalDateTime.now());
        consolidationDetails1.getCarrierDetails().setEtd(LocalDateTime.now());
        consolidationDetails1.getCarrierDetails().setShippingLine("QWERTY");
        consolidationDetails1.setReceivingBranch(2L);
        consolidationDetails1.setSendingAgent(Parties.builder().orgCode("ORG").build());
        consolidationDetails1.setReceivingAgent(Parties.builder().orgCode("ORG").build());
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails1));
        Exception exception = assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
        assertEquals("Please generate HBL for the shipment/s SHP000110207 to transfer the files.", exception.getMessage());
    }

    @Test
    void testSendConsolidationValidation_Failure_NetworkTransferConsoleFieldsException2() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails1.setConsolidationType(Constants.SHIPMENT_TYPE_STD);
        consolidationDetails1.getCarrierDetails().setFlightNumber(null);
        consolidationDetails1.getCarrierDetails().setEta(null);
        consolidationDetails1.getCarrierDetails().setEtd(null);
        consolidationDetails1.setReceivingBranch(null);
        consolidationDetails1.setTriangulationPartner(null);
        consolidationDetails1.setTriangulationPartnerList(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails1));
        Exception exception = assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
        assertEquals("Please enter the Flight Number, Eta, Etd, one of the branches in the entity transfer details section and generate MAWB for the consolidation and generate HAWB for the shipment/s SHP000110207 to transfer the files.", exception.getMessage());
    }

    @Test
    void testSendConsolidationValidation_Failure_NetworkTransferConsoleFieldsException_AIR() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails1.setConsolidationType(Constants.SHIPMENT_TYPE_STD);
        consolidationDetails1.getCarrierDetails().setFlightNumber("123");
        consolidationDetails1.getCarrierDetails().setEta(LocalDateTime.now());
        consolidationDetails1.getCarrierDetails().setEtd(LocalDateTime.now());
        consolidationDetails1.setReceivingBranch(3L);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails1));
        Exception exception = assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
        assertEquals("Please generate MAWB for the consolidation and generate HAWB for the shipment/s SHP000110207 to transfer the files.", exception.getMessage());
    }

    @Test
    void testSendConsolidationValidation_Failure_NetworkTransferShipmentException_AIR2() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails1.setConsolidationType(Constants.SHIPMENT_TYPE_STD);
        consolidationDetails1.getCarrierDetails().setFlightNumber("123");
        consolidationDetails1.getCarrierDetails().setEta(LocalDateTime.now());
        consolidationDetails1.getCarrierDetails().setEtd(LocalDateTime.now());
        consolidationDetails1.setReceivingBranch(3L);
        List<Awb> mawbs = new ArrayList<>();
        mawbs.add(Awb.builder().printType(PrintType.ORIGINAL_PRINTED).build());
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails1));
        when(awbDao.findByConsolidationId(anyLong())).thenReturn(mawbs);
        Exception exception = assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
        assertEquals("Please generate HAWB for the shipment/s SHP000110207 to transfer the files.", exception.getMessage());
    }
    @Test
    void testSendConsolidationValidation_Success_NetworkTransfer_AIR() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails1.setConsolidationType(Constants.SHIPMENT_TYPE_STD);
        consolidationDetails1.getCarrierDetails().setFlightNumber("123");
        consolidationDetails1.getCarrierDetails().setEta(LocalDateTime.now());
        consolidationDetails1.getCarrierDetails().setEtd(LocalDateTime.now());
        consolidationDetails1.setReceivingBranch(3L);
        List<Awb> mawbs = new ArrayList<>();
        mawbs.add(Awb.builder().printType(PrintType.ORIGINAL_PRINTED).build());
        List<Awb> hawb = new ArrayList<>();
        hawb.add(Awb.builder().printType(PrintType.ORIGINAL_PRINTED).build());
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails1));
        when(awbDao.findByConsolidationId(anyLong())).thenReturn(mawbs);
        when(awbDao.findByShipmentId(anyLong())).thenReturn(hawb);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendConsolidationValidation_Failure_NetworkTransferConsoleFieldsException3() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.getShipmentsList().iterator().next().setJobType(Constants.CONSOLIDATION_TYPE_AGT);
        consolidationDetails1.getShipmentsList().iterator().next().setHouseBill(null);
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails1.setConsolidationType(Constants.CONSOLIDATION_TYPE_AGT);
        consolidationDetails1.setBol(null);
        consolidationDetails1.getCarrierDetails().setFlightNumber(null);
        consolidationDetails1.getCarrierDetails().setEta(null);
        consolidationDetails1.getCarrierDetails().setEtd(null);
        consolidationDetails1.setReceivingBranch(null);
        consolidationDetails1.setTriangulationPartner(null);
        consolidationDetails1.setTriangulationPartnerList(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails1));
        Exception exception = assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
        assertEquals("Please enter the Flight Number, Eta, Etd, one of the branches in the entity transfer details section, MAWB Number for the consolidation and enter the HAWB number for the shipment/s SHP000110207 to transfer the files.", exception.getMessage());
    }

    @Test
    void testSendConsolidationValidation_Success_NetworkTransfer_ROA() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_ROA);
        consolidationDetails1.getCarrierDetails().setEta(LocalDateTime.now());
        consolidationDetails1.getCarrierDetails().setEtd(LocalDateTime.now());
        consolidationDetails1.setReceivingBranch(3L);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails1));
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendConsolidationValidation_Failure_NetworkTransferConsoleFieldError_ROA() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_ROA);
        consolidationDetails1.getCarrierDetails().setEta(null);
        consolidationDetails1.getCarrierDetails().setEtd(null);
        consolidationDetails1.setSendingAgent(null);
        consolidationDetails1.setReceivingAgent(null);
        consolidationDetails1.setReceivingBranch(null);
        consolidationDetails1.setTriangulationPartner(null);
        consolidationDetails1.setTriangulationPartnerList(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails1));
        Exception exception = assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
        assertEquals("Please enter the Eta, Etd, Origin agent, Destination agent, one of the branches in the entity transfer details section for the consolidation to transfer the files.", exception.getMessage());
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
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        ShipmentDetails shipmentDetails1 = consolidationDetails1.getShipmentsList().iterator().next();
        shipmentDetails1.getCarrierDetails().setVessel(null);
        shipmentDetails1.getCarrierDetails().setVoyage(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel1 = new TenantModel();
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel1).build();
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails1));
        when(hblDao.findByShipmentId(consolidationDetails1.getShipmentsList().iterator().next().getId())).thenReturn(List.of());
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel1);
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }
    @Test
    void testSendConsolidationValidation_Failure_ShipmentFieldsException() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().iterator().next();
        shipmentDetails.getCarrierDetails().setVessel(null);
        shipmentDetails.getCarrierDetails().setVoyage(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel = new TenantModel();
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(hblDao.findByShipmentId(consolidationDetails.getShipmentsList().iterator().next().getId())).thenReturn(List.of(new Hbl()));
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
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().iterator().next();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.getCarrierDetails().setFlightNumber("A123");
        shipmentDetails.getCarrierDetails().setShippingLine("Air India");
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        ValidationResponse response = ValidationResponse.builder().success(true).build();
        TenantModel tenantModel = new TenantModel();
        tenantModel.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel).build();

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(awbDao.findByShipmentId(consolidationDetails.getShipmentsList().iterator().next().getId())).thenReturn(List.of(new Awb()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel);
        mockShipmentSettings();
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
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().iterator().next();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.getCarrierDetails().setFlightNumber("A123");
        shipmentDetails.getCarrierDetails().setShippingLine("Air India");
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Failure_Air_ShipmentFieldsException_Awb() {
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails1.getCarrierDetails().setFlightNumber("A123");
        consolidationDetails1.getCarrierDetails().setShippingLine("Air India");
        ShipmentDetails shipmentDetails1 = consolidationDetails1.getShipmentsList().iterator().next();
        shipmentDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails1.getCarrierDetails().setFlightNumber(null);
        shipmentDetails1.getCarrierDetails().setShippingLine(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel1 = new TenantModel();
        tenantModel1.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel1).build();
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails1));
        when(awbDao.findByShipmentId(consolidationDetails1.getShipmentsList().iterator().next().getId())).thenReturn(List.of(new Awb()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel1);
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Failure_Air_ShipmentFieldsException() {
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails1.getCarrierDetails().setFlightNumber("A123");
        consolidationDetails1.getCarrierDetails().setShippingLine("Air India");
        ShipmentDetails shipmentDetails1 = consolidationDetails1.getShipmentsList().iterator().next();
        shipmentDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails1.getCarrierDetails().setFlightNumber(null);
        shipmentDetails1.getCarrierDetails().setShippingLine(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel1 = new TenantModel();
        tenantModel1.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel1).build();
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails1));
        when(awbDao.findByShipmentId(consolidationDetails1.getShipmentsList().iterator().next().getId())).thenReturn(List.of());
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel1);
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendConsolidationValidation_Failure_Air_NON_STD_ShipmentFieldsException() {
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails1.getCarrierDetails().setFlightNumber("A123");
        consolidationDetails1.getCarrierDetails().setShippingLine("Air India");
        ShipmentDetails shipmentDetails1 = consolidationDetails1.getShipmentsList().iterator().next();
        shipmentDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails1.setJobType(Constants.CONSOLIDATION_TYPE_CLD);
        shipmentDetails1.getCarrierDetails().setFlightNumber(null);
        shipmentDetails1.getCarrierDetails().setShippingLine(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel1 = new TenantModel();
        tenantModel1.IATAAgent = true;
        mockShipmentSettings();
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel1).build();

        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails1));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel1);
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }
    @Test
    void testSendConsolidationValidation_Failure_Air_ShipmentFieldsException_InterBranch() {
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails1.getCarrierDetails().setFlightNumber("A123");
        consolidationDetails1.getCarrierDetails().setShippingLine("Air India");
        consolidationDetails1.setInterBranchConsole(true);
        consolidationDetails1.setReceivingBranch(12L);
        ShipmentDetails shipmentDetails1 = consolidationDetails1.getShipmentsList().iterator().next();
        shipmentDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails1.setReceivingBranch(null);
        shipmentDetails1.getCarrierDetails().setFlightNumber(null);
        shipmentDetails1.getCarrierDetails().setShippingLine(null);
        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel1 = new TenantModel();
        tenantModel1.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel1).build();
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails1));
        when(awbDao.findByShipmentId(consolidationDetails1.getShipmentsList().iterator().next().getId())).thenReturn(List.of());
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel1);
        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));
    }

    @Test
    void testSendShipmentValidation_Success() {
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setHouseBill("QWERT324");
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails1.getId()).build();
        ValidationResponse response = ValidationResponse.builder().success(true).build();
        TenantModel tenantModel1 = new TenantModel();
        tenantModel1.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel1).build();
        mockShipmentSettings();
        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails1));
        when(hblDao.findByShipmentId(shipmentDetails1.getId())).thenReturn(List.of(new Hbl()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel1);
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendShipmentValidation(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
    }

    @Test
    void testSendShipmentValidation_Failure_DataRetrievalFailure() {
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setHouseBill("QWERT324");
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
    }

    @Test
    void testSendShipmentValidation_Failure_ShipmentFieldsException() {
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setHouseBill(null);
        shipmentDetails1.setMasterBill(null);
        shipmentDetails1.getCarrierDetails().setVoyage(null);
        shipmentDetails1.getCarrierDetails().setVessel(null);
        shipmentDetails1.getCarrierDetails().setEta(null);
        shipmentDetails1.getCarrierDetails().setEtd(null);
        shipmentDetails1.getCarrierDetails().setOriginPort(null);
        shipmentDetails1.getCarrierDetails().setDestinationPort(null);
        shipmentDetails1.setReceivingBranch(null);
        shipmentDetails1.setTriangulationPartnerList(null);
        shipmentDetails1.setTriangulationPartner(null);
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails1));
        assertThrows(ValidationException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
    }

    @Test
    void testSendShipmentValidation_Failure_NetworkTransferShipmentFieldsException() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails1.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails1.setHouseBill(null);
        shipmentDetails1.setMasterBill(null);
        shipmentDetails1.getCarrierDetails().setEta(null);
        shipmentDetails1.getCarrierDetails().setEtd(null);
        shipmentDetails1.setReceivingBranch(null);
        shipmentDetails1.setTriangulationPartnerList(null);
        shipmentDetails1.setTriangulationPartner(null);
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails1));
        Exception exception = assertThrows(ValidationException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
        assertEquals("Please enter the Flight number,Eta,Etd,one of the branches in the entity transfer details section and generate MAWB to transfer the shipment.", exception.getMessage());
    }
    @Test
    void testSendShipmentValidation_Failure_NetworkTransferShipmentFieldsException1() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_STD);
        shipmentDetails.setHouseBill(null);
        shipmentDetails.setMasterBill(null);
        shipmentDetails.getCarrierDetails().setEta(null);
        shipmentDetails.getCarrierDetails().setEtd(null);
        shipmentDetails.setReceivingBranch(null);
        shipmentDetails.setTriangulationPartnerList(null);
        shipmentDetails.setTriangulationPartner(null);
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails));
        Exception exception = assertThrows(ValidationException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
        assertEquals("Entity Transfer not allowed for this shipment.", exception.getMessage());
    }

    @Test
    void testSendShipmentValidation_Failure_NetworkTransferShipmentFieldsException2() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails1.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails1.setHouseBill(null);
        shipmentDetails1.setMasterBill(null);
        shipmentDetails1.getCarrierDetails().setEta(LocalDateTime.now());
        shipmentDetails1.getCarrierDetails().setEtd(LocalDateTime.now());
        shipmentDetails1.getCarrierDetails().setFlightNumber("123");
        shipmentDetails1.setReceivingBranch(23L);
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails1));
        Exception exception = assertThrows(ValidationException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
        assertEquals("Please generate MAWB to transfer the shipment.", exception.getMessage());
    }
    @Test
    void testSendShipmentValidation_Success_NetworkTransferShipmentFieldsException3() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails1.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails1.setHouseBill(null);
        shipmentDetails1.setMasterBill(null);
        shipmentDetails1.getCarrierDetails().setEta(LocalDateTime.now());
        shipmentDetails1.getCarrierDetails().setEtd(LocalDateTime.now());
        shipmentDetails1.getCarrierDetails().setFlightNumber("123");
        shipmentDetails1.setReceivingBranch(23L);
        List<Awb> hawb = new ArrayList<>();
        hawb.add(Awb.builder().printType(PrintType.ORIGINAL_PRINTED).build());
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails1));
        when(awbDao.findByShipmentId(shipmentDetails1.getId())).thenReturn(hawb);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }


    @Test
    void testSendShipmentValidation_Failure_HblError() {
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setHouseBill("QWERT324");
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails1));
        when(hblDao.findByShipmentId(shipmentDetails1.getId())).thenReturn(List.of());
        assertThrows(ValidationException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
    }

    @Test
    void testSendShipmentValidation_Success_Air() {
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails1.getCarrierDetails().setFlightNumber("W233");
        shipmentDetails1.getCarrierDetails().setShippingLine("Air India");
        shipmentDetails1.setHouseBill("QWERT324");
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails1.getId()).build();
        ValidationResponse response = ValidationResponse.builder().success(true).build();
        TenantModel tenantModel1 = new TenantModel();
        tenantModel1.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel1).build();
        mockShipmentSettings();
        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails1));
        when(awbDao.findByShipmentId(shipmentDetails1.getId())).thenReturn(List.of(new Awb()));
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel1);
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendShipmentValidation(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(response), responseEntity);
    }

    @Test
    void testSendShipmentValidation_Failure_Air_ShipmentFieldsError() {
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails1.getCarrierDetails().setFlightNumber(null);
        shipmentDetails1.getCarrierDetails().setShippingLine(null);
        shipmentDetails1.setHouseBill(null);
        shipmentDetails1.setMasterBill(null);
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails1));
        assertThrows(ValidationException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
    }

    @Test
    void testSendShipmentValidation_Failure_Air_AwbError() {
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails1.getCarrierDetails().setFlightNumber("W233");
        shipmentDetails1.getCarrierDetails().setShippingLine("Air India");
        shipmentDetails1.setHouseBill("QWERT324");
        ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(shipmentDetails1.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        TenantModel tenantModel1 = new TenantModel();
        tenantModel1.IATAAgent = true;
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(tenantModel1).build();
        mockShipmentSettings();
        when(shipmentDao.findById(request.getShipId())).thenReturn(Optional.of(shipmentDetails1));
        when(awbDao.findByShipmentId(shipmentDetails1.getId())).thenReturn(List.of());
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(tenantModel1);
        assertThrows(ValidationException.class, () -> entityTransferService.sendShipmentValidation(commonRequestModel));
    }

    @Test
    void testCheckTaskExist_Success_Shipment() {
        try {
            CheckTaskExistRequest request = CheckTaskExistRequest.builder()
                .entityId(123L)
                .entityType(Constants.SHIPMENTS_WITH_SQ_BRACKETS)
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
                    .entityType(Constants.SHIPMENTS_WITH_SQ_BRACKETS)
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
                .entityType(Constants.SHIPMENTS_WITH_SQ_BRACKETS)
                .sendToBranch(List.of(66))
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        when(v1Service.listTask(any())).thenThrow(new RuntimeException());

        assertThrows(RunnerException.class, () -> entityTransferService.checkTaskExist(commonRequestModel));
    }

    @Test
    void testPostArValidation_Success() throws RunnerException {
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setTenantId(33);
        ConsolidationDetails consolidationDetails1 = shipmentDetails1.getConsolidationList().iterator().next();
        consolidationDetails1.setGuid(UUID.randomUUID());
        consolidationDetails1.setReceivingBranch(123L);
        TriangulationPartner triangulationPartner = TriangulationPartner.builder().triangulationPartner(231L).isAccepted(false).build();
        consolidationDetails1.setTriangulationPartnerList(List.of(triangulationPartner));
        consolidationDetails1.setTriangulationPartner(231L);
        ShipmentDetails shipmentDetailsDrt = jsonTestUtility.getCompleteShipment();
        shipmentDetailsDrt.setGuid(UUID.randomUUID());
        shipmentDetailsDrt.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetailsDrt.setTenantId(33);

        ShipmentDetails shipmentDetailsImp = jsonTestUtility.getCompleteShipment();
        shipmentDetailsImp.setGuid(UUID.randomUUID());
        shipmentDetailsImp.setDirection(Constants.DIRECTION_IMP);
        shipmentDetailsImp.setSourceGuid(UUID.randomUUID());
        shipmentDetailsImp.setTenantId(33);
        ConsolidationDetails consolidationDetailsImp = shipmentDetailsImp.getConsolidationList().iterator().next();
        consolidationDetailsImp.setGuid(UUID.randomUUID());
        consolidationDetailsImp.setShipmentType(Constants.DIRECTION_IMP);
        consolidationDetailsImp.setReceivingBranch(33L);
        TriangulationPartner triangulationPartner1 = TriangulationPartner.builder().triangulationPartner(33L).isAccepted(false).build();
        consolidationDetailsImp.setTriangulationPartnerList(List.of(triangulationPartner1));
        consolidationDetailsImp.setTriangulationPartner(33L);

        ShipmentDetails shipmentDetailsImp1 = jsonTestUtility.getCompleteShipment();
        shipmentDetailsImp1.setGuid(UUID.randomUUID());
        shipmentDetailsImp1.setDirection(Constants.DIRECTION_IMP);
        shipmentDetailsImp1.setSourceGuid(UUID.randomUUID());
        shipmentDetailsImp1.setTenantId(33);
        ConsolidationDetails consolidationDetailsImp1 = shipmentDetailsImp1.getConsolidationList().iterator().next();
        consolidationDetailsImp1.setGuid(UUID.randomUUID());
        consolidationDetailsImp1.setShipmentType(Constants.DIRECTION_IMP);
        consolidationDetailsImp1.setReceivingBranch(null);
        consolidationDetailsImp1.setTriangulationPartnerList(List.of(triangulationPartner1));
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
        ConsolidationDetails consolidationDetailsExp = shipmentDetailsExp.getConsolidationList().iterator().next();
        consolidationDetailsExp.setGuid(UUID.randomUUID());
        consolidationDetailsExp.setShipmentType(Constants.DIRECTION_EXP);
        consolidationDetailsExp.setReceivingBranch(null);

        LocalDateTime timeStamp = LocalDateTime.now();
        PostArValidationRequest postArValidationRequest = new PostArValidationRequest(List.of(shipmentDetails1.getGuid(), shipmentDetailsDrt.getGuid(), shipmentDetailsImp.getGuid(), shipmentDetailsImp1.getGuid(), shipmentDetailsExp.getGuid(), shipmentDetailsImp2.getGuid(), shipmentDetailsImp3.getGuid()), timeStamp);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(postArValidationRequest);

        ShipmentDetails destShipment = new ShipmentDetails();
        destShipment.setGuid(UUID.randomUUID());
        destShipment.setSourceGuid(shipmentDetails1.getGuid());
        destShipment.setTenantId(123);

        ShipmentDetails destShipmentForTriangulation = new ShipmentDetails();
        destShipmentForTriangulation.setGuid(UUID.randomUUID());
        destShipmentForTriangulation.setSourceGuid(shipmentDetails1.getGuid());
        destShipmentForTriangulation.setTenantId(231);

        ShipmentDetails originShipment = new ShipmentDetails();
        originShipment.setGuid(shipmentDetailsImp.getSourceGuid());
        originShipment.setTenantId(432);
        ConsolidationDetails originShipConsole = new ConsolidationDetails();
        originShipConsole.setGuid(UUID.randomUUID());
        originShipConsole.setShipmentType(Constants.DIRECTION_IMP);
        originShipConsole.setReceivingBranch(33L);
        originShipConsole.setTriangulationPartnerList(List.of(triangulationPartner1));
        originShipConsole.setTriangulationPartner(33L);
        originShipment.setConsolidationList(new HashSet<>(List.of(originShipConsole)));

        ShipmentDetails originShipment1 = new ShipmentDetails();
        originShipment1.setGuid(shipmentDetailsImp1.getSourceGuid());
        originShipment1.setTenantId(432);
        ConsolidationDetails originShipConsole1 = new ConsolidationDetails();
        originShipConsole1.setGuid(UUID.randomUUID());
        originShipConsole1.setShipmentType(Constants.DIRECTION_IMP);
        originShipConsole1.setReceivingBranch(null);
        originShipConsole1.setTriangulationPartnerList(List.of(triangulationPartner1));
        originShipConsole1.setTriangulationPartner(33L);
        originShipment1.setConsolidationList(new HashSet<>(List.of(originShipConsole1)));

        ShipmentDetails originShipment2 = new ShipmentDetails();
        originShipment2.setGuid(shipmentDetailsImp2.getSourceGuid());
        originShipment2.setTenantId(432);
        ConsolidationDetails originShipConsole2 = new ConsolidationDetails();
        originShipConsole2.setGuid(UUID.randomUUID());
        originShipConsole2.setShipmentType(Constants.DIRECTION_EXP);
        originShipConsole2.setReceivingBranch(33L);
        originShipConsole2.setTriangulationPartnerList(List.of(triangulationPartner1));
        originShipConsole1.setTriangulationPartner(33L);
        originShipment2.setConsolidationList(new HashSet<>(List.of(originShipConsole2)));

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
        originShipConsole3.setTriangulationPartnerList(List.of(triangulationPartner1));
        originShipConsole3.setTriangulationPartner(33L);
        originShipment3.setConsolidationList(new HashSet<>(List.of(originShipConsole3)));

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

        LogHistoryResponse logHistoryResponse = LogHistoryResponse.builder().entityGuid(shipmentDetails1.getGuid()).entityPayload(jsonTestUtility.convertToJson(shipmentDetails1)).build();
        List<UUID> shipGuids = new ArrayList<>(List.of(shipmentDetails1.getGuid(), shipmentDetailsDrt.getGuid(), shipmentDetailsImp.getGuid(), shipmentDetailsImp1.getGuid(), shipmentDetailsExp.getGuid(), shipmentDetailsImp2.getGuid(), shipmentDetailsImp3.getGuid()));
        Set<UUID> shipGuidSet = new HashSet<>(shipGuids);
        Set<UUID> shipGuidSet1 = new HashSet<>(shipGuidSet);
        shipGuidSet1.remove(shipmentDetails1.getGuid());
        Set<UUID> consoleGuids = new LinkedHashSet<>(List.of(shipmentDetails1.getConsolidationList().iterator().next().getGuid(), shipmentDetailsDrt.getConsolidationList().iterator().next().getGuid(), shipmentDetailsImp.getConsolidationList().iterator().next().getGuid(),
                shipmentDetailsImp1.getConsolidationList().iterator().next().getGuid(), shipmentDetailsExp.getConsolidationList().iterator().next().getGuid()));
        Set<UUID> consoleGuids1 = new HashSet<>(consoleGuids);
        consoleGuids1.remove(shipmentDetails1.getConsolidationList().iterator().next().getGuid());

        LogHistoryResponse consoleLogHistoryResponse = LogHistoryResponse.builder().entityGuid(shipmentDetails1.getConsolidationList().iterator().next().getGuid()).entityPayload(jsonTestUtility.convertToJson(shipmentDetails1.getConsolidationList().iterator().next())).build();

        mockShipmentSettings();
        when(shipmentDao.findShipmentsByGuids(any())).thenReturn(List.of(shipmentDetailsDrt, shipmentDetailsImp, shipmentDetailsImp1, shipmentDetailsExp, shipmentDetailsImp2, shipmentDetailsImp3));
        when(consolidationDetailsDao.findConsolidationsByGuids(consoleGuids1))
                .thenReturn(List.of(shipmentDetailsDrt.getConsolidationList().iterator().next(), shipmentDetailsImp.getConsolidationList().iterator().next(), shipmentDetailsImp1.getConsolidationList().iterator().next(), shipmentDetailsExp.getConsolidationList().iterator().next()));
        when(jsonHelper.readFromJson(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails1);
        when(logsHistoryService.findByEntityGuidsAndTimeStamp(anyList(), eq(timeStamp))).thenReturn(List.of(consoleLogHistoryResponse));
        when(jsonHelper.readFromJson(consoleLogHistoryResponse.getEntityPayload(), ConsolidationDetails.class)).thenReturn(shipmentDetails1.getConsolidationList().iterator().next());
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.postArValidation(commonRequestModel);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }
    @Test
    void testPostArValidation_SuccessWithoutConsol() throws RunnerException {
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setTenantId(33);
        shipmentDetails1.setConsolidationList(Set.of());
        ShipmentDetails shipmentDetailsDrt = jsonTestUtility.getCompleteShipment();
        shipmentDetailsDrt.setGuid(UUID.randomUUID());
        shipmentDetailsDrt.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetailsDrt.setTenantId(33);
        shipmentDetailsDrt.setConsolidationList(Set.of());

        ShipmentDetails shipmentDetailsImp = jsonTestUtility.getCompleteShipment();
        shipmentDetailsImp.setGuid(UUID.randomUUID());
        shipmentDetailsImp.setDirection(Constants.DIRECTION_IMP);
        shipmentDetailsImp.setSourceGuid(UUID.randomUUID());
        shipmentDetailsImp.setTenantId(33);
        shipmentDetailsImp.setConsolidationList(Set.of());

        ShipmentDetails shipmentDetailsImp1 = jsonTestUtility.getCompleteShipment();
        shipmentDetailsImp1.setGuid(UUID.randomUUID());
        shipmentDetailsImp1.setDirection(Constants.DIRECTION_IMP);
        shipmentDetailsImp1.setSourceGuid(UUID.randomUUID());
        shipmentDetailsImp1.setTenantId(33);
        shipmentDetailsImp1.setConsolidationList(Set.of());

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
        shipmentDetailsExp.setConsolidationList(Set.of());

        LocalDateTime timeStamp = LocalDateTime.now();
        PostArValidationRequest postArValidationRequest = new PostArValidationRequest(List.of(shipmentDetails1.getGuid(), shipmentDetailsDrt.getGuid(), shipmentDetailsImp.getGuid(), shipmentDetailsImp1.getGuid(), shipmentDetailsExp.getGuid(), shipmentDetailsImp2.getGuid(), shipmentDetailsImp3.getGuid()), timeStamp);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(postArValidationRequest);

        ShipmentDetails destShipment = new ShipmentDetails();
        destShipment.setGuid(UUID.randomUUID());
        destShipment.setSourceGuid(shipmentDetails1.getGuid());
        destShipment.setTenantId(123);

        ShipmentDetails destShipmentForTriangulation = new ShipmentDetails();
        destShipmentForTriangulation.setGuid(UUID.randomUUID());
        destShipmentForTriangulation.setSourceGuid(shipmentDetails1.getGuid());
        destShipmentForTriangulation.setTenantId(231);

        ShipmentDetails originShipment = new ShipmentDetails();
        originShipment.setGuid(shipmentDetailsImp.getSourceGuid());
        originShipment.setTenantId(432);
        ConsolidationDetails originShipConsole = new ConsolidationDetails();
        originShipConsole.setGuid(UUID.randomUUID());
        originShipConsole.setShipmentType(Constants.DIRECTION_IMP);
        originShipConsole.setReceivingBranch(33L);
        originShipConsole.setTriangulationPartner(33L);
        originShipment.setConsolidationList(new HashSet<>(List.of(originShipConsole)));

        ShipmentDetails originShipment1 = new ShipmentDetails();
        originShipment1.setGuid(shipmentDetailsImp1.getSourceGuid());
        originShipment1.setTenantId(432);
        ConsolidationDetails originShipConsole1 = new ConsolidationDetails();
        originShipConsole1.setGuid(UUID.randomUUID());
        originShipConsole1.setShipmentType(Constants.DIRECTION_IMP);
        originShipConsole1.setReceivingBranch(null);
        originShipConsole1.setTriangulationPartner(33L);
        originShipment1.setConsolidationList(new HashSet<>(List.of(originShipConsole1)));

        ShipmentDetails originShipment2 = new ShipmentDetails();
        originShipment2.setGuid(shipmentDetailsImp2.getSourceGuid());
        originShipment2.setTenantId(432);
        ConsolidationDetails originShipConsole2 = new ConsolidationDetails();
        originShipConsole2.setGuid(UUID.randomUUID());
        originShipConsole2.setShipmentType(Constants.DIRECTION_EXP);
        originShipConsole2.setReceivingBranch(33L);
        originShipConsole1.setTriangulationPartner(33L);
        originShipment2.setConsolidationList(new HashSet<>(List.of(originShipConsole2)));

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
        originShipConsole3.setTriangulationPartner(33L);
        originShipment3.setConsolidationList(new HashSet<>(List.of(originShipConsole3)));

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

        LogHistoryResponse logHistoryResponse = LogHistoryResponse.builder().entityGuid(shipmentDetails1.getGuid()).entityPayload(jsonTestUtility.convertToJson(shipmentDetails1)).build();
        List<UUID> shipGuids = new ArrayList<>(List.of(shipmentDetails1.getGuid(), shipmentDetailsDrt.getGuid(), shipmentDetailsImp.getGuid(), shipmentDetailsImp1.getGuid(), shipmentDetailsExp.getGuid(), shipmentDetailsImp2.getGuid(), shipmentDetailsImp3.getGuid()));
        Set<UUID> shipGuidSet = new HashSet<>(shipGuids);
        Set<UUID> shipGuidSet1 = new HashSet<>(shipGuidSet);
        shipGuidSet1.remove(shipmentDetails1.getGuid());

        when(shipmentDao.findShipmentsByGuids(shipGuidSet1)).thenReturn(List.of(shipmentDetailsDrt, shipmentDetailsImp, shipmentDetailsImp1, shipmentDetailsExp, shipmentDetailsImp2, shipmentDetailsImp3));
        when(logsHistoryService.findByEntityGuidsAndTimeStamp(shipGuidSet.stream().toList(), timeStamp)).thenReturn(List.of(logHistoryResponse));
        when(jsonHelper.readFromJson(logHistoryResponse.getEntityPayload(), ShipmentDetails.class)).thenReturn(shipmentDetails1);
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
        var request = CheckEntityExistRequest.builder().entityId(UUID.randomUUID().toString()).entityType(Constants.SHIPMENT_CAMELCASE).build();
        when(shipmentDao.findBySourceGuid(any())).thenReturn(List.of(new ShipmentDetails()));
        var responseEntity = entityTransferService.checkEntityExists(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCheckEntityExists2() {
        var request = CheckEntityExistRequest.builder().entityId(UUID.randomUUID().toString()).entityType(Constants.CONSOLIDATION_CAMELCASE).build();
        when(consolidationDetailsDao.findBySourceGuid(any())).thenReturn(List.of(new ConsolidationDetails()));
        var responseEntity = entityTransferService.checkEntityExists(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCheckEntityExists3() {
        var request = CheckEntityExistRequest.builder().entityId(UUID.randomUUID().toString()).entityType(Constants.CONSOLIDATION_CAMELCASE).build();
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

    @ParameterizedTest
    @CsvSource({
            "Branch001, <p>Branch: {GS_ConsolidationBranch}</p><p>Shipment: {SD_ShipmentNumber}</p><p>Branch: {SD_SendingBranch}</p>, <p>Branch: Branch001</p><p>Shipment: {SD_ShipmentNumber}</p><p>Branch: {SD_SendingBranch}</p>",
            "Branch001, <p>Branch: GS_ConsolidationBranch</p><p>Shipment: SD_ShipmentNumber</p>, <p>Branch: GS_ConsolidationBranch</p><p>Shipment: SD_ShipmentNumber</p>"
    })
    void testReplaceTagsValues_SomeTagsMissing(Object branch, String htmlElement, String expected) {
        // Arrange
        Map<String, Object> tagDetails = new HashMap<>();
        tagDetails.put("GS_ConsolidationBranch", branch);

        // Act
        String result = entityTransferService.replaceTagsValues(tagDetails, htmlElement);

        // Assert
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
        assertEquals("user1@example.com", result.iterator().next());
        assertEquals("user2@example.com", result.get(1));
    }


    @Test
    void testImportShipment_rejection() throws RunnerException {
        ImportShipmentRequest importShipmentRequest = ImportShipmentRequest.builder().taskId(1L).operation(TaskStatus.REJECTED.getDescription()).rejectRemarks("test rejected").build();
        mockShipmentSettings();
        var response = entityTransferService.importShipment(CommonRequestModel.buildRequest(importShipmentRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportShipment_nullPayload() {
        ImportShipmentRequest importShipmentRequest = ImportShipmentRequest.builder().taskId(1L).operation(TaskStatus.APPROVED.getDescription()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(importShipmentRequest).build();
        mockShipmentSettings();
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
        mockShipmentSettings();

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
                .assignedTo("EGYPQAP100ALEX@dpworld.com")
                .build();

        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setTenantId(12);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);

        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);

        when(modelMapper.map(any(), eq(ShipmentRequest.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentRequest.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(any(), any())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        mockShipmentSettings();

        var response = entityTransferService.importShipment(CommonRequestModel.buildRequest(importShipmentRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportShipment_UpdateTriangulationOrReceivingAccepted() throws RunnerException {
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
        shipmentDetailsResponse.setReceivingBranch(1L);
        shipmentDetailsResponse.setTriangulationPartnerList(List.of(TriangulationPartnerResponse.builder().triangulationPartner(1L).build()));
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);

        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);

        when(modelMapper.map(any(), eq(ShipmentRequest.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentRequest.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(any(), any())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
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
        mockShipmentSettings();

        var response = entityTransferService.importShipment(CommonRequestModel.buildRequest(importShipmentRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }


    @Test
    void testImportConsolidation_rejectTask() throws RunnerException {
        ImportConsolidationRequest importConsolidationRequest = ImportConsolidationRequest.builder().taskId(1L).operation(TaskStatus.REJECTED.getDescription()).rejectRemarks("test rejected").build();
        mockShipmentSettings();
        var response =  entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportConsolidation_nullPayload() {
        ImportConsolidationRequest importConsolidationRequest = ImportConsolidationRequest.builder().taskId(1L).operation(TaskStatus.APPROVED.getDescription()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(importConsolidationRequest).build();
        mockShipmentSettings();
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
        EntityTransferShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().iterator().next();
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
        mockShipmentSettings();

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
        EntityTransferShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().iterator().next();
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        ConsolidationDetailsRequest consolidationDetailsRequest = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationDetailsRequest.class);
        ContainerResponse containerResponse = objectMapperTest.convertValue(entityTransferConsolidationDetails.getContainersList().iterator().next(), ContainerResponse.class);
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

        Containers containers = objectMapperTest.convertValue(entityTransferConsolidationDetails.getContainersList().iterator().next(), Containers.class);
        Packing packing = objectMapperTest.convertValue(entityTransferShipmentDetails.getPackingList().iterator().next(), Packing.class);

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
        mockShipmentSettings();

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
        EntityTransferShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().iterator().next();
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
        mockShipmentSettings();

        var response = entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportConsolidation_Success_Create_Air_interBranch_NTE() throws RunnerException {
        UserContext.getUser().setTenantId(728);
        EntityTransferConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getImportConsolidationAir();
        Map<String, String> shipmentNumberAssignedToMap = new HashMap<>();
        shipmentNumberAssignedToMap.put("TQAA24080071", "EGYPQAP100ALEX@dpworld.com");
        ImportConsolidationRequest importConsolidationRequest = ImportConsolidationRequest.builder()
                .isFromNte(true)
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .shipmentNumberAssignedToMap(shipmentNumberAssignedToMap)
                .build();
        EntityTransferShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().iterator().next();
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
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);

        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);

        when(consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid())).thenReturn(List.of());
        when(modelMapper.map(any(), eq(ShipmentRequest.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentRequest.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consoleDetails));
        when(modelMapper.map(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(consolidationDetailsRequest);
        when(consolidationService.createConsolidationFromEntityTransfer(any())).thenReturn(consolidationDetailsResponse);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
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
        EntityTransferShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().iterator().next();
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
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(false);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(importConsolidationRequest);

        when(consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid())).thenReturn(List.of());
        when(modelMapper.map(any(), eq(ShipmentRequest.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentRequest.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);
        when(modelMapper.map(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(consolidationDetailsRequest);
        when(consolidationService.createConsolidationFromEntityTransfer(any())).thenReturn(consolidationDetailsResponse);
        mockShipmentSettings();

        assertThrows(ValidationException.class, () -> entityTransferService.importConsolidation(commonRequestModel));
    }

    @Test
    void testImportConsolidation_UpdateTriangulationOrReceivingAccepted() throws RunnerException {
        UserContext.getUser().setTenantId(728);
        EntityTransferConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getImportConsolidationAir();
        ConsolidationDetails consolidationDetails2 = jsonTestUtility.getCompleteConsolidation();
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getTestShipment();
        shipmentDetails1.setId(2L);
        shipmentDetails1.setReceivingBranch(728L);
        shipmentDetails1.setTriangulationPartnerList(List.of(TriangulationPartner.builder().triangulationPartner(728L).build()));
        consolidationDetails2.setShipmentsList(Set.of(shipmentDetails1));

        ImportConsolidationRequest importConsolidationRequest = ImportConsolidationRequest.builder()
                .operation(TaskStatus.APPROVED.getDescription())
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .isFromNte(true)
                .build();
        EntityTransferShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().iterator().next();
        entityTransferShipmentDetails.setSendToBranch(720);
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        ConsolidationDetailsRequest consolidationDetailsRequest = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationDetailsRequest.class);
        ConsolidationDetailsResponse consolidationDetailsResponse1 = new ConsolidationDetailsResponse();
        consolidationDetailsResponse1.setId(3L);
        consolidationDetailsResponse1.setGuid(UUID.randomUUID());
        consolidationDetailsResponse1.setTenantId(entityTransferConsolidationDetails.getSendToBranch());
        consolidationDetailsResponse1.setConsolidationNumber(entityTransferConsolidationDetails.getConsolidationNumber());
        consolidationDetailsResponse1.setReceivingBranch(728L);
        consolidationDetailsResponse1.setTriangulationPartnerList(List.of(TriangulationPartnerResponse.builder().triangulationPartner(728L).build()));

        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);

        when(consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid())).thenReturn(List.of());
        when(modelMapper.map(any(), eq(ShipmentRequest.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentRequest.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);
        when(modelMapper.map(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(consolidationDetailsRequest);
        when(consolidationService.createConsolidationFromEntityTransfer(any())).thenReturn(consolidationDetailsResponse1);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails2));
        doNothing().when(networkTransferService).updateStatusAndCreatedEntityId(anyLong(), anyString(), anyLong());
        when(networkTransferDao.findById(any())).thenReturn(Optional.of(NetworkTransfer.builder().entityId(1L).build()));
        when(consolidationDetailsDao.findConsolidationByIdWithQuery(any())).thenReturn(Optional.of(consolidationDetails2));
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        doNothing().when(shipmentDao).saveIsTransferredToReceivingBranch(anyLong(), anyBoolean());
        doNothing().when(consolidationDetailsDao).saveIsTransferredToReceivingBranch(anyLong(), anyBoolean());
        doNothing().when(shipmentDao).updateIsAcceptedTriangulationPartner(anyLong(), anyLong(), anyBoolean());
        doNothing().when(consolidationDetailsDao).updateIsAcceptedTriangulationPartner(anyLong(), anyLong(), anyBoolean());
        mockShipmentSettings();

        var response = entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportConsolidation_UpdateTriangulation_TenantNotMatching() throws RunnerException {
        UserContext.getUser().setTenantId(937);
        EntityTransferConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getImportConsolidationAir();
        ConsolidationDetails consolidationDetails2 = jsonTestUtility.getCompleteConsolidation();
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getTestShipment();
        shipmentDetails1.setId(2L);
        shipmentDetails1.setReceivingBranch(728L);
        shipmentDetails1.setTriangulationPartnerList(List.of(TriangulationPartner.builder().triangulationPartner(728L).build()));
        consolidationDetails2.setShipmentsList(Set.of(shipmentDetails1));

        Map<String, String> shipmentNumberAssignedToMap = new HashMap<>();
        shipmentNumberAssignedToMap.put("TQAA24080078", "EGYPQAP100ALEX@dpworld.com");

        ImportConsolidationRequest importConsolidationRequest = ImportConsolidationRequest.builder()
                .operation(TaskStatus.APPROVED.getDescription())
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .isFromNte(true)
                .shipmentNumberAssignedToMap(shipmentNumberAssignedToMap)
                .build();
        EntityTransferShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().iterator().next();
        entityTransferShipmentDetails.setSendToBranch(720);
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        ConsolidationDetailsRequest consolidationDetailsRequest = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationDetailsRequest.class);
        ConsolidationDetailsResponse consolidationDetailsResponse1 = new ConsolidationDetailsResponse();
        consolidationDetailsResponse1.setId(3L);
        consolidationDetailsResponse1.setGuid(UUID.randomUUID());
        consolidationDetailsResponse1.setTenantId(entityTransferConsolidationDetails.getSendToBranch());
        consolidationDetailsResponse1.setConsolidationNumber(entityTransferConsolidationDetails.getConsolidationNumber());
        consolidationDetailsResponse1.setReceivingBranch(728L);
        consolidationDetailsResponse1.setTriangulationPartnerList(List.of(TriangulationPartnerResponse.builder().triangulationPartner(728L).build()));

        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);

        when(consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid())).thenReturn(List.of());
        when(modelMapper.map(any(), eq(ShipmentRequest.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentRequest.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);
        when(modelMapper.map(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(consolidationDetailsRequest);
        when(consolidationService.createConsolidationFromEntityTransfer(any())).thenReturn(consolidationDetailsResponse1);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails2));
        mockShipmentSettings();

        var response = entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportConsolidation_UpdateTriangulation_ErrorLog(){
        UserContext.getUser().setTenantId(728);
        EntityTransferConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getImportConsolidationAir();
        ConsolidationDetails consolidationDetails2 = jsonTestUtility.getCompleteConsolidation();

        ImportConsolidationRequest importConsolidationRequest = ImportConsolidationRequest.builder()
                .operation(TaskStatus.APPROVED.getDescription())
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .isFromNte(true)
                .build();
        EntityTransferShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().iterator().next();
        entityTransferShipmentDetails.setSendToBranch(720);
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        ConsolidationDetailsRequest consolidationDetailsRequest = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationDetailsRequest.class);
        ConsolidationDetailsResponse consolidationDetailsResponse1 = new ConsolidationDetailsResponse();
        consolidationDetailsResponse1.setId(3L);
        consolidationDetailsResponse1.setGuid(UUID.randomUUID());
        consolidationDetailsResponse1.setTenantId(entityTransferConsolidationDetails.getSendToBranch());
        consolidationDetailsResponse1.setConsolidationNumber(entityTransferConsolidationDetails.getConsolidationNumber());
        consolidationDetailsResponse1.setReceivingBranch(728L);
        consolidationDetailsResponse1.setTriangulationPartnerList(List.of(TriangulationPartnerResponse.builder().triangulationPartner(728L).build()));

        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);

        when(consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid())).thenReturn(List.of());
        when(modelMapper.map(any(), eq(ShipmentRequest.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentRequest.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);
        when(modelMapper.map(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(consolidationDetailsRequest);
        when(consolidationService.createConsolidationFromEntityTransfer(any())).thenReturn(consolidationDetailsResponse1);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails2));
        doNothing().when(networkTransferService).updateStatusAndCreatedEntityId(anyLong(), anyString(), anyLong());
        doNothing().when(consolidationDetailsDao).saveIsTransferredToReceivingBranch(anyLong(), anyBoolean());
        doNothing().when(consolidationDetailsDao).updateIsAcceptedTriangulationPartner(anyLong(), anyLong(), anyBoolean());
        when(networkTransferDao.findById(any())).thenReturn(Optional.of(NetworkTransfer.builder().entityId(1L).build()));
        when(consolidationDetailsDao.findConsolidationByIdWithQuery(any())).thenReturn(Optional.empty());
        when(consolidationDetailsDao.findById(3L)).thenReturn(Optional.empty());
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        mockShipmentSettings();
        var commonRequest = CommonRequestModel.buildRequest(importConsolidationRequest);

        assertThrows(DataRetrievalFailureException.class, () -> entityTransferService.importConsolidation(commonRequest));
    }

    @Test
    void testSendGroupedEmailForShipmentImport_Success() {
        // Given
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        String consoleSourceBranchTenantName = "consoleBranch";
        when(shipmentData.getTenantId()).thenReturn(100);
        when(shipmentData.getSourceTenantId()).thenReturn(123L);
        when(shipmentData.getShipmentId()).thenReturn("1");
        when(shipmentData.getHouseBill()).thenReturn("hbn");
        when(shipmentData.getMasterBill()).thenReturn("mbn");
        when(shipmentData.getShipmentCreatedOn()).thenReturn(LocalDateTime.now());

        List<ShipmentDetails> shipmentDetailsList = List.of(
                shipmentData,
                shipmentData
        );

        // Mocks
        doNothing().when(commonUtils).setInterBranchContextForHub();
        doNothing().when(commonUtils).getToAndCCEmailIdsFromTenantSettings(anySet(), anyMap());
        EmailTemplatesRequest emailTemplatesRequest = mock(EmailTemplatesRequest.class);
        when(jsonHelper.convertValueToList(any(), eq(EmailTemplatesRequest.class))).thenReturn(List.of(emailTemplatesRequest));
        when(iv1Service.getEmailTemplatesWithTenantId(any())).thenReturn(V1DataResponse.builder().build());
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
        when(v1Service.getUsersWithGivenPermissions(any())).thenReturn(List.of(UsersDto.builder().Email("abcd").build()));
        mockShipmentSettings();

        // When
        entityTransferService.sendGroupedEmailForShipmentImport(shipmentDetailsList, consoleSourceBranchTenantName);

        // Then
        verify(commonUtils, times(1)).setInterBranchContextForHub();
        verify(commonUtils, times(1)).getToAndCCEmailIdsFromTenantSettings(anySet(), anyMap());
    }


    @Test
    void testSendConsolidationEmailNotification_Success() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        List<Integer> destinationBranches = List.of(1,2,3);

        when(iv1Service.getEmailTemplatesWithTenantId(any())).thenReturn(V1DataResponse.builder().build());
        Map<Integer, Object> mockV1Map = new HashMap<>();
        TenantContext.setCurrentTenant(1);
        mockV1Map.put(1, new Object());
        UsersDto mockUser = new UsersDto();
        mockUser.setEmail("test@dpworld.com");
        UserContext.setUser(mockUser);
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(mockV1Map);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockedTenantModel);
        when(consoleDetails.getConsolidationNumber()).thenReturn("1");
        when(consoleDetails.getShipmentsList()).thenReturn(Set.of(shipmentData));
        when(consoleDetails.getTransportMode()).thenReturn("SEA");
        when(consoleDetails.getBol()).thenReturn("bol");
        when(v1Service.getUsersWithGivenPermissions(any())).thenReturn(List.of(mockUser));
        mockShipmentSettings();

        Map<String, List<Integer>> shipmentGuidSendToBranch = new HashMap<>();
        shipmentGuidSendToBranch.put("1", List.of(1,2,3));
        entityTransferService.sendConsolidationEmailNotification(consoleDetails, destinationBranches, shipmentGuidSendToBranch, false);

        verify(v1ServiceUtil, times(1)).getTenantDetails(anyList());

    }
    @Test
    void testSendConsolidationEmailNotification_Success1() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        List<Integer> destinationBranches = List.of(1,2,3);

        when(iv1Service.getEmailTemplatesWithTenantId(any())).thenReturn(V1DataResponse.builder().build());

        Map<Integer, Object> mockV1Map = new HashMap<>();
        TenantContext.setCurrentTenant(1);
        mockV1Map.put(1, new Object());
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(mockV1Map);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockedTenantModel);
        when(consoleDetails.getConsolidationNumber()).thenReturn("1");
        when(consoleDetails.getShipmentsList()).thenReturn(Set.of(shipmentData));
        when(consoleDetails.getTransportMode()).thenReturn("SEA");
        when(consoleDetails.getBol()).thenReturn("bol");
        when(v1Service.getUsersWithGivenPermissions(any())).thenReturn(new ArrayList<>());
        mockShipmentSettings();

        Map<String, List<Integer>> shipmentGuidSendToBranch = null;
        entityTransferService.sendConsolidationEmailNotification(consoleDetails, destinationBranches, shipmentGuidSendToBranch, false);

        verify(v1ServiceUtil, times(1)).getTenantDetails(anyList());

    }

    @Test
    void testSendShipmentEmailNotification_Success() {
        List<Integer> destinationBranches = List.of(1,2,3);

        when(iv1Service.getEmailTemplatesWithTenantId(any())).thenReturn(V1DataResponse.builder().build());
        Map<Integer, Object> mockV1Map = new HashMap<>();
        TenantContext.setCurrentTenant(1);
        mockV1Map.put(1, new Object());
        UsersDto mockUser = new UsersDto();
        mockUser.setEmail("test@dpworld.com");
        UserContext.setUser(mockUser);
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(mockV1Map);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockedTenantModel);
        when(shipmentData.getShipmentId()).thenReturn("1");
        when(shipmentData.getHouseBill()).thenReturn("hbn");
        when(shipmentData.getMasterBill()).thenReturn("mbn");
        when(v1Service.getUsersWithGivenPermissions(any())).thenReturn(List.of(mockUser));
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        mockShipmentSettings();
        entityTransferService.sendShipmentEmailNotification(shipmentData, destinationBranches, false);

        verify(v1Service, times(3)).getUsersWithGivenPermissions(any());

    }

    @Test
    void testSendShipment2EmailNotification_Success() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        List<Integer> destinationBranches = List.of(1,2,3);

        when(iv1Service.getEmailTemplatesWithTenantId(any())).thenReturn(V1DataResponse.builder().build());
        Map<Integer, Object> mockV1Map = new HashMap<>();
        TenantContext.setCurrentTenant(1);
        mockV1Map.put(1, new Object());

        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(mockV1Map);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockedTenantModel);
        when(shipmentData.getShipmentId()).thenReturn("1");
        when(shipmentData.getHouseBill()).thenReturn("hbn");
        when(shipmentData.getMasterBill()).thenReturn("mbn");
        when(v1Service.getUsersWithGivenPermissions(any())).thenReturn(new ArrayList<>());
        mockShipmentSettings();
        entityTransferService.sendShipmentEmailNotification(shipmentData, destinationBranches, false);

        verify(v1Service, times(3)).getUsersWithGivenPermissions(any());

    }

    @Test
    void testSendShipmentForNTESuccess() throws RunnerException {
        Long shipmentId = 1L;
        int mockTenantId = 10;

        SendShipmentRequest sendShipmentRequest = new SendShipmentRequest();
        sendShipmentRequest.setSendToBranch(List.of(1,2,3));
        sendShipmentRequest.setShipId(shipmentId);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendShipmentRequest);

        ShipmentDetails mockShipmentDetails = jsonTestUtility.getCompleteShipment();
        mockShipmentDetails.setTenantId(mockTenantId);
        EntityTransferV3ShipmentDetails mockETPayload = objectMapperTest.convertValue(mockShipmentDetails, EntityTransferV3ShipmentDetails.class);
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );

        NetworkTransfer mockNetworkTransfer = jsonTestUtility.getNetworkTransfer();

        // Mocking
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(mockShipmentDetails));
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETPayload);
        when(shipmentService.fetchAllMasterDataByKey(any(), any())).thenReturn(new HashMap<String, Object>());
        when(networkTransferDao.findByTenantAndEntity(1, 155357L, Constants.SHIPMENT)).thenReturn(Optional.of(mockNetworkTransfer));
        when(networkTransferDao.findByTenantAndEntity(2, 155357L, Constants.SHIPMENT)).thenReturn(Optional.empty());
        when(networkTransferDao.findByTenantAndEntity(2, 155357L, Constants.SHIPMENT)).thenReturn(Optional.empty());
        when(notificationDao.findNotificationForEntityTransfer(anyLong(), anyString(), anyInt(), anyList())).thenReturn(new ArrayList<>());
        doNothing().when(notificationDao).deleteAll(anyList());

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(Boolean.TRUE);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(3);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        var httpResponse = entityTransferService.sendShipment(commonRequestModel);

        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        verify(shipmentDao, times(1)).saveEntityTransfer(any(), any());
    }

    @Test
    void testSendShipment_alreadyAcceptedNT() throws RunnerException {
        Long shipmentId = 1L;
        int mockTenantId = 10;

        SendShipmentRequest sendShipmentRequest = new SendShipmentRequest();
        sendShipmentRequest.setSendToBranch(List.of(1,2,3));
        sendShipmentRequest.setShipId(shipmentId);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendShipmentRequest);

        ShipmentDetails mockShipmentDetails = jsonTestUtility.getCompleteShipment();
        mockShipmentDetails.setTenantId(mockTenantId);
        EntityTransferV3ShipmentDetails mockETPayload = objectMapperTest.convertValue(mockShipmentDetails, EntityTransferV3ShipmentDetails.class);
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );

        NetworkTransfer mockNetworkTransfer = jsonTestUtility.getNetworkTransfer();
        mockNetworkTransfer.setStatus(NetworkTransferStatus.ACCEPTED);

        // Mocking
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(mockShipmentDetails));
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETPayload);
        when(shipmentService.fetchAllMasterDataByKey(any(), any())).thenReturn(new HashMap<String, Object>());
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().entities("").build());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(Boolean.TRUE);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());

        lenient().when(networkTransferDao.findByEntityAndTenantList(155357L, SHIPMENT,
            sendShipmentRequest.getSendToBranch())).thenReturn(List.of(mockNetworkTransfer));

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(3);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(4);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        ResponseEntity<IRunnerResponse> response = entityTransferService.sendShipment(
            commonRequestModel);
        assertNotNull(response);
    }

    @Test
    void testSendShipment_alreadyReTransferNT() throws RunnerException {
        Long shipmentId = 1L;
        int mockTenantId = 10;

        SendShipmentRequest sendShipmentRequest = new SendShipmentRequest();
        sendShipmentRequest.setSendToBranch(List.of(1));
        sendShipmentRequest.setShipId(shipmentId);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendShipmentRequest);

        ShipmentDetails mockShipmentDetails = jsonTestUtility.getCompleteShipment();
        mockShipmentDetails.setTenantId(mockTenantId);
        EntityTransferV3ShipmentDetails mockETPayload = objectMapperTest.convertValue(mockShipmentDetails, EntityTransferV3ShipmentDetails.class);
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
            Map.entry(mockTenantId, mockV1TenantResponse)
        );

        NetworkTransfer mockNetworkTransfer = jsonTestUtility.getNetworkTransfer();
        mockNetworkTransfer.setStatus(NetworkTransferStatus.RETRANSFERRED);

        // Mocking
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(mockShipmentDetails));
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETPayload);
        when(shipmentService.fetchAllMasterDataByKey(any(), any())).thenReturn(new HashMap<String, Object>());
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().entities("").build());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(Boolean.TRUE);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());

        when(networkTransferDao.findByTenantAndEntity(anyInt(), anyLong(), anyString()))
            .thenReturn(Optional.of(mockNetworkTransfer));
        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails1, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        ResponseEntity<IRunnerResponse> response = entityTransferService.sendShipment(
            commonRequestModel);
        assertNotNull(response);
    }

    @Test
    void testSendConsolidationForNTESuccess() throws RunnerException {
        int mockTenantId = 10;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTenantId(mockTenantId);
        consolidationDetails.getShipmentsList().forEach(i -> i.setTenantId(mockTenantId));
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();

        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().iterator().next().getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66, 11))
                .consolId(consolidationDetails.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();

        EntityTransferV3ConsolidationDetails mockETPayload = objectMapperTest.convertValue(consolidationDetails, EntityTransferV3ConsolidationDetails.class);
        ShipmentDetails mockLinkedShipment = new ShipmentDetails();
        EntityTransferV3ShipmentDetails mockETShipment = new EntityTransferV3ShipmentDetails();
        mockETShipment.setGuid(UUID.randomUUID());
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );
        NetworkTransfer mockNetworkTransfer = jsonTestUtility.getNetworkTransfer();

        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.of(consolidationDetails));
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETShipment);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));
        when(networkTransferDao.findByTenantAndEntity(66, 2258L, Constants.CONSOLIDATION)).thenReturn(Optional.of(mockNetworkTransfer));
        when(networkTransferDao.findByTenantAndEntity(11, 2258L, Constants.CONSOLIDATION)).thenReturn(Optional.empty());
        when(notificationDao.findNotificationForEntityTransfer(anyLong(), anyString(), anyInt(), anyList())).thenReturn(new ArrayList<>());
        doNothing().when(notificationDao).deleteAll(anyList());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(Boolean.TRUE);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(11);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(66);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(69);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testSendConsolidation_For_InterBranchConsole_And_NTE() throws RunnerException {
        int mockTenantId = 10;
        UUID shipGuid = UUID.randomUUID();
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setTenantId(mockTenantId);
        consolidationDetails1.getShipmentsList().forEach(i -> {
            i.setTenantId(mockTenantId);
            i.setGuid(shipGuid);
        });
        consolidationDetails1.setInterBranchConsole(true);

        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();

        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails1.getShipmentsList().iterator().next().getGuid().toString(), List.of(UUID.randomUUID().toString()));
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66))
                .consolId(consolidationDetails1.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .shipmentGuidSendToBranch(Map.ofEntries(
                        Map.entry(shipGuid.toString(), List.of(69))
                ))
                .build();

        EntityTransferV3ConsolidationDetails mockETPayload = objectMapperTest.convertValue(consolidationDetails1, EntityTransferV3ConsolidationDetails.class);
        ShipmentDetails mockLinkedShipment = consolidationDetails1.getShipmentsList().iterator().next();
        EntityTransferV3ShipmentDetails mockETShipment = objectMapperTest.convertValue(mockLinkedShipment, EntityTransferV3ShipmentDetails.class);
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

        NetworkTransfer mockNetworkTransfer = jsonTestUtility.getNetworkTransfer();

        when(consolidationDetailsDao.findById(consolidationDetails1.getId())).thenReturn(Optional.of(consolidationDetails1));
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(v1ServiceUtil.getTenantSettingsMap(anyList())).thenReturn(mockV1TenantSettingsMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETShipment);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));
        when(networkTransferDao.findByTenantAndEntity(66, 2258L, Constants.CONSOLIDATION)).thenReturn(Optional.of(mockNetworkTransfer));
        when(networkTransferDao.findByTenantAndEntity(69, 5607L, Constants.SHIPMENT)).thenReturn(Optional.of(mockNetworkTransfer));
        when(notificationDao.findNotificationForEntityTransfer(anyLong(), anyString(), anyInt(), anyList())).thenReturn(new ArrayList<>());
        doNothing().when(notificationDao).deleteAll(anyList());
        when(v1ServiceUtil.fetchCoLoadInfo(any(), any())).thenReturn(coLoadMap);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(Boolean.TRUE);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(66);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(69);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testSendConsolidationWithTriangulationPartner() throws RunnerException {
        int mockTenantId = 10;
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTenantId(mockTenantId);
        consolidationDetails.getShipmentsList().forEach(i -> i.setTenantId(mockTenantId));
        TriangulationPartner triangulationPartner = TriangulationPartner.builder().triangulationPartner(66L).build();
        consolidationDetails.setTriangulationPartnerList(List.of(triangulationPartner));  // Mock triangulation partner

        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();

        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(consolidationDetails.getShipmentsList().iterator().next().getGuid().toString(), List.of(UUID.randomUUID().toString()));

        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66)) // Send to the triangulation partner
                .consolId(consolidationDetails.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();

        EntityTransferV3ConsolidationDetails mockETPayload = objectMapperTest.convertValue(consolidationDetails, EntityTransferV3ConsolidationDetails.class);
        ShipmentDetails mockLinkedShipment = new ShipmentDetails();
        EntityTransferV3ShipmentDetails mockETShipment = new EntityTransferV3ShipmentDetails();
        mockETShipment.setGuid(UUID.randomUUID());
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );

        // Mocks
        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.of(consolidationDetails));
        when(shipmentSettingsDao.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETShipment);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(List.of(mockV1TenantResponse));

        mockShipmentSettings();

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(66);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(69);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        // Call method
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));

        // Assertions
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

        // Verify that the shipment direction for the triangulation partner was set to DIRECTION_CTS
        verify(jsonHelper, times(1)).convertValue(any(), eq(EntityTransferV3ShipmentDetails.class)); // Ensure convertValue was called

        // Check the direction for the triangulation partner shipment
        assertEquals(Constants.DIRECTION_CTS, mockETShipment.getDirection(), "Direction should be set to DIRECTION_CTS for triangulation partner.");
    }


    @Test
    void testSendConsolidation_alreadyAcceptedNT() throws RunnerException {
        int mockTenantId = 10;
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setTenantId(mockTenantId);
        consolidationDetails1.getShipmentsList().forEach(i -> i.setTenantId(mockTenantId));
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();

        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(
                consolidationDetails1.getShipmentsList().iterator().next().getGuid().toString(),
                List.of(UUID.randomUUID().toString())
        );
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                .sendToBranch(List.of(66, 11))
                .consolId(consolidationDetails1.getId())
                .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
                .additionalDocs(List.of(UUID.randomUUID().toString()))
                .shipAdditionalDocs(shipAdditionalDocs)
                .build();

        EntityTransferV3ConsolidationDetails mockETPayload = objectMapperTest.convertValue(consolidationDetails1, EntityTransferV3ConsolidationDetails.class);
        ShipmentDetails mockLinkedShipment = new ShipmentDetails();
        EntityTransferV3ShipmentDetails mockETShipment = new EntityTransferV3ShipmentDetails();
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(mockTenantId, mockV1TenantResponse)
        );
        NetworkTransfer mockNetworkTransfer = jsonTestUtility.getNetworkTransfer();
        mockNetworkTransfer.setStatus(NetworkTransferStatus.ACCEPTED);

        when(consolidationDetailsDao.findById(consolidationDetails1.getId())).thenReturn(Optional.of(consolidationDetails1));
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETShipment);
        lenient().when(networkTransferDao.findByEntityAndTenantList(2258L, CONSOLIDATION, sendConsolidationRequest.getSendToBranch())).thenReturn(List.of(mockNetworkTransfer));
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().entities("").build());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(Boolean.TRUE);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(11);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(66);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(69);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendConsolidationRequest);

        ResponseEntity<IRunnerResponse>  response = entityTransferService.sendConsolidation(commonRequestModel);
        assertNotNull(response);
    }

    @Test
    void testSendConsolidation_alreadyRetransferNT() throws RunnerException {
        int mockTenantId = 10;
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setTenantId(mockTenantId);
        consolidationDetails1.getShipmentsList().forEach(i -> i.setTenantId(mockTenantId));
        EntityTransferOrganizations organizations = jsonTestUtility.getOrganizationData();

        Map<String, List<String>> shipAdditionalDocs = new HashMap<>();
        shipAdditionalDocs.put(
            consolidationDetails1.getShipmentsList().iterator().next().getGuid().toString(),
            List.of(UUID.randomUUID().toString())
        );
        SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
            .sendToBranch(List.of(66, 11))
            .consolId(consolidationDetails1.getId())
            .sendToOrg(List.of(organizations.getWhitelistedTenantGUID()))
            .additionalDocs(List.of(UUID.randomUUID().toString()))
            .shipAdditionalDocs(shipAdditionalDocs)
            .build();

        EntityTransferV3ConsolidationDetails mockETPayload = objectMapperTest.convertValue(consolidationDetails1, EntityTransferV3ConsolidationDetails.class);
        ShipmentDetails mockLinkedShipment = new ShipmentDetails();
        EntityTransferV3ShipmentDetails mockETShipment = new EntityTransferV3ShipmentDetails();
        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();

        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
            Map.entry(mockTenantId, mockV1TenantResponse)
        );
        NetworkTransfer mockNetworkTransfer = jsonTestUtility.getNetworkTransfer();
        mockNetworkTransfer.setStatus(NetworkTransferStatus.ACCEPTED);

        when(consolidationDetailsDao.findById(consolidationDetails1.getId())).thenReturn(Optional.of(consolidationDetails1));
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETShipment);
        when(networkTransferDao.findByTenantAndEntity(anyInt(), anyLong(), anyString())).thenReturn(Optional.of(mockNetworkTransfer));
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().entities("").build());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(Boolean.TRUE);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendConsolidationRequest);

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(66);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(11);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        ResponseEntity<IRunnerResponse>  response = entityTransferService.sendConsolidation(commonRequestModel);
        assertNotNull(response);
    }

    @Test
    void testAutomaticTransferConsoleValidation_WhenConsolidationDetailsNotFound() {
        ValidateSendConsolidationRequest request = new ValidateSendConsolidationRequest();
        request.setConsoleId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, () -> entityTransferService.automaticTransferConsoleValidation(commonRequestModel));

        verify(consolidationDetailsDao).findById(anyLong());
    }

    @Test
    void testAutomaticTransferConsoleValidation_WhenTransportModeIsRail() {
        ValidateSendConsolidationRequest request = new ValidateSendConsolidationRequest();
        request.setConsoleId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_RAI);
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails1));

        assertThrows(ValidationException.class, () -> entityTransferService.automaticTransferConsoleValidation(commonRequestModel));

        verify(consolidationDetailsDao).findById(anyLong());
    }

    @Test
    void testAutomaticTransferConsoleValidation_WhenInterBranchConsoleIsTrue() {
        ValidateSendConsolidationRequest request = new ValidateSendConsolidationRequest();
        request.setConsoleId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setInterBranchConsole(true);
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_ROA);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails1));
        doNothing().when(commonUtils).setInterBranchContextForHub();

        var response = entityTransferService.automaticTransferConsoleValidation(commonRequestModel);

        assertNotNull(response);
        verify(commonUtils).setInterBranchContextForHub();
        verify(consolidationDetailsDao).findById(1L);
    }

    @Test
    void testAutomaticTransferConsoleValidation_WhenTransportModeIsAir() {
        ValidateSendConsolidationRequest request = new ValidateSendConsolidationRequest();
        request.setConsoleId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails1));

        var response = entityTransferService.automaticTransferConsoleValidation(commonRequestModel);

        assertNotNull(response);
        verify(consolidationDetailsDao).findById(1L);
    }

    @Test
    void testAutomaticTransferConsoleValidation_WhenTransportModeIsSea() {
        ValidateSendConsolidationRequest request = new ValidateSendConsolidationRequest();
        request.setConsoleId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        ConsolidationDetails consolidationDetails1 = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails1));

        var response = entityTransferService.automaticTransferConsoleValidation(commonRequestModel);

        assertNotNull(response);
        verify(consolidationDetailsDao).findById(1L);
    }

    @Test
    void testAutomaticTransferShipmentValidation_ValidationException() {
        ValidateSendShipmentRequest request = new ValidateSendShipmentRequest();
        request.setShipId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails1));

        assertThrows(ValidationException.class, () -> entityTransferService.automaticTransferShipmentValidation(commonRequestModel));

        verify(shipmentDao).findById(1L);
    }

    @Test
    void testAutomaticTransferShipmentValidation_EmptyShipment() {
        ValidateSendShipmentRequest request = new ValidateSendShipmentRequest();
        request.setShipId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(shipmentDao.findById(1L)).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, () -> entityTransferService.automaticTransferShipmentValidation(commonRequestModel));

        verify(shipmentDao).findById(1L);
    }

    @Test
    void testAutomaticTransferShipmentValidation() {
        ValidateSendShipmentRequest request = new ValidateSendShipmentRequest();
        request.setShipId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails1.setJobType(Constants.SHIPMENT_TYPE_DRT);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails1));

        var response = entityTransferService.automaticTransferShipmentValidation(commonRequestModel);

        assertNotNull(response);
        verify(shipmentDao).findById(1L);
    }

    @Test
    void testCreateBulkExportEvent() {
        entityTransferService.createBulkExportEvent(1L, "EVENT_CODE", "SHIPMENT", new ArrayList<>(), 1);

        verifyNoInteractions(v1ServiceUtil, jsonHelper, eventService);
    }

    @Test
    void testCreateBulkExportEvent2() {
        MDC.put(LoggingConstants.AUTOMATIC_TRANSFER, "true");
        List<Integer> tenantIds = List.of(101, 102);
        Map<Integer, Object> tenantMap = Map.of(
                101, new Object(),
                102, new Object()
        );

        V1TenantResponse tenantResponse1 = new V1TenantResponse();
        tenantResponse1.setCode("T101");
        V1TenantResponse tenantResponse2 = new V1TenantResponse();
        tenantResponse2.setCode("T102");

        when(v1ServiceUtil.getTenantDetails(tenantIds)).thenReturn(tenantMap);
        when(jsonHelper.convertValue(tenantMap.get(101), V1TenantResponse.class)).thenReturn(tenantResponse1);
        when(jsonHelper.convertValue(tenantMap.get(102), V1TenantResponse.class)).thenReturn(tenantResponse2);
        doNothing().when(eventService).saveAllEvent(anyList());

        entityTransferService.createBulkExportEvent(1L, "EVENT_CODE", "SHIPMENT", tenantIds, 101);

        verify(eventService, times(1)).saveAllEvent(anyList());
    }

    @Test
    void testCreateBulkExportEvent3() {
        MDC.put(LoggingConstants.AUTOMATIC_TRANSFER, "true");
        List<Integer> tenantIds = List.of(101);
        Map<Integer, Object> tenantMap = Map.of(101, V1TenantResponse.builder().Code("101").build());

        when(v1ServiceUtil.getTenantDetails(tenantIds)).thenReturn(tenantMap);
        when(jsonHelper.convertValue(tenantMap.get(101), V1TenantResponse.class)).thenReturn(V1TenantResponse.builder().Code("101").build());
        doNothing().when(eventService).saveAllEvent(anyList());

        entityTransferService.createBulkExportEvent(1L, "EVENT_CODE", "SHIPMENT", tenantIds, 101);

        verify(eventService, times(1)).saveAllEvent(anyList());
    }

    @Test
    void testCreateBulkExportEvent4() {
        List<Integer> tenantIds = List.of(101);
        Map<Integer, Object> tenantMap = Map.of(101, new Object());
        V1TenantResponse tenantResponse = new V1TenantResponse();
        tenantResponse.setCode("T101");

        when(v1ServiceUtil.getTenantDetails(tenantIds)).thenReturn(tenantMap);
        when(jsonHelper.convertValue(tenantMap.get(101), V1TenantResponse.class)).thenReturn(tenantResponse);
        doThrow(new RuntimeException("Save Event Failed")).when(eventService).saveAllEvent(anyList());

        assertThrows(RuntimeException.class, () -> entityTransferService.createBulkExportEvent(1L, "EVENT_CODE", "SHIPMENT", tenantIds, 1));

        verify(eventService, times(1)).saveAllEvent(anyList());
    }

    @Test
    void testCreateBulkExportEventForMultipleShipments1() {
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setShipmentsList(Collections.emptySet());

        entityTransferService.createBulkExportEventForMultipleShipments(consolidationDetails1, new HashMap<>());

        verifyNoInteractions(eventService);
    }

    @Test
    void testCreateBulkExportEventForMultipleShipments2() {
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        ShipmentDetails shipment1 = new ShipmentDetails();
        shipment1.setId(1L);
        shipment1.setGuid(UUID.randomUUID());
        shipment1.setTenantId(101);

        ShipmentDetails shipment2 = new ShipmentDetails();
        shipment2.setId(2L);
        shipment2.setGuid(UUID.randomUUID());
        shipment2.setTenantId(102);

        consolidationDetails1.setShipmentsList(Set.of(shipment1, shipment2));

        Map<String, List<Integer>> shipmentGuidBranchMap = Map.of(
                shipment1.getGuid().toString(), List.of(101),
                shipment2.getGuid().toString(), List.of(102)
        );
        doNothing().when(eventService).saveAllEvent(anyList());

        entityTransferService.createBulkExportEventForMultipleShipments(consolidationDetails1, shipmentGuidBranchMap);

        verify(eventService, times(2)).saveAllEvent(anyList());
    }

    @Test
    void testCreateBulkExportEventForMultipleShipments3() {
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setId(1L);
        shipment.setGuid(UUID.randomUUID());
        shipment.setTenantId(101);
        consolidationDetails1.setShipmentsList(Set.of(shipment));

        Map<String, List<Integer>> shipmentGuidBranchMap = Map.of(
                shipment.getGuid().toString(), List.of(101)
        );
        doNothing().when(eventService).saveAllEvent(anyList());

        entityTransferService.createBulkExportEventForMultipleShipments(consolidationDetails1, shipmentGuidBranchMap);

        verify(eventService, times(1)).saveAllEvent(anyList());
    }

    @Test
    void testCreateAutoEvent_Success() {
        MDC.put(LoggingConstants.AUTOMATIC_TRANSFER, "true");
        Long entityId = 1001L;
        String eventCode = "EVENT_TEST";
        String entityType = "SHIPMENT";
        List<Integer> tenantIds = List.of(1, 2);
        Map<Integer, Object> tenantDetailsMap = Map.of(
                1, new Object(),
                2, new Object()
        );

        V1TenantResponse tenantResponse1 = new V1TenantResponse();
        tenantResponse1.setCode("Tenant1");

        V1TenantResponse tenantResponse2 = new V1TenantResponse();
        tenantResponse2.setCode("Tenant2");

        when(v1ServiceUtil.getTenantDetails(tenantIds)).thenReturn(tenantDetailsMap);
        when(jsonHelper.convertValue(tenantDetailsMap.get(1), V1TenantResponse.class)).thenReturn(tenantResponse1);
        when(jsonHelper.convertValue(tenantDetailsMap.get(2), V1TenantResponse.class)).thenReturn(tenantResponse2);
        doNothing().when(eventService).saveAllEvent(anyList());

        entityTransferService.createAutoEvent(entityId, eventCode, entityType, tenantIds, 1);

        verify(eventService, times(1)).saveAllEvent(anyList());
    }

    @Test
    void testCreateAutoEvent_ShouldNotCreateEvent_WhenEntityIdIsNull() {
        List<Integer> tenantIds = List.of(1, 2);

        entityTransferService.createAutoEvent(null, "EVENT_CODE", "SHIPMENT", tenantIds, 1);

        verify(eventService, never()).saveAllEvent(anyList());
    }

    @Test
    void testCreateAutoEvent_ShouldNotCreateEvent_WhenTenantIdsAreEmpty() {
        Long entityId = 1001L;
        List<Integer> tenantIds = List.of();

        entityTransferService.createAutoEvent(entityId, "EVENT_CODE", "SHIPMENT", tenantIds, 1);

        verify(eventService, never()).saveAllEvent(anyList());
    }

    @Test
    void testCreateAutoEvent_ShouldHandleNullTenantDetails() {
        Long entityId = 1001L;
        List<Integer> tenantIds = List.of(1, 2);
        Map<Integer, Object> tenantDetailsMap = Map.of(1, new Object());

        when(v1ServiceUtil.getTenantDetails(tenantIds)).thenReturn(tenantDetailsMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(null);
        doNothing().when(eventService).saveAllEvent(anyList());

        entityTransferService.createAutoEvent(entityId, "EVENT_CODE", "SHIPMENT", tenantIds, 1);

        verify(eventService, times(1)).saveAllEvent(anyList());
    }

    @Test
    void testCheckAcceptedFiles(){

        AcceptedFileRequest acceptedFileRequest = new AcceptedFileRequest();
        acceptedFileRequest.setEntityId(101L);
        acceptedFileRequest.setEntityType("SHIPMENT");
        List<Integer> sendToBranch = List.of(1,2,3);
        acceptedFileRequest.setSendToBranch(sendToBranch);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder()
            .data(acceptedFileRequest).build();

        NetworkTransfer networkTransfer = NetworkTransfer.builder()
            .status(NetworkTransferStatus.ACCEPTED).build();
        networkTransfer.setTenantId(1);
        List<NetworkTransfer> networkTransferList = List.of(networkTransfer);

        when(networkTransferDao.findByEntityNTList(
            101L, "SHIPMENT")).thenReturn(networkTransferList);
        when(v1Service.tenantNameByTenantId(any())).thenReturn(V1DataResponse.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(V1TenantResponse.class))).thenReturn(Collections.emptyList());

        ResponseEntity<IRunnerResponse> response=  entityTransferService.checkAcceptedFiles(commonRequestModel);
        assertNotNull(response);
    }

    @Test
    void testCheckAcceptedFiles1(){

        AcceptedFileRequest acceptedFileRequest = new AcceptedFileRequest();
        acceptedFileRequest.setEntityId(101L);
        acceptedFileRequest.setEntityType("SHIPMENT");
        CommonRequestModel commonRequestModel = CommonRequestModel.builder()
            .data(acceptedFileRequest).build();

        assertThrows(ValidationException.class, () ->
            entityTransferService.checkAcceptedFiles(commonRequestModel));
    }
    @Test
    void testImportShipment_Rejection1() throws RunnerException {
        ImportShipmentRequest importShipmentRequest = ImportShipmentRequest.builder()
                .taskId(1L)
                .operation(TaskStatus.REJECTED.getDescription())
                .rejectRemarks("Test rejection msg")
                .isFromNte(true)
                .build();
        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);
        UserContext.getUser().setDisplayName("abc");
        UserContext.getUser().setEmail("abc@xyz.com");

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        mockShipmentSettings();
        NetworkTransfer networkTransfer = new NetworkTransfer();
        networkTransfer.setUpdatedBy("XYZ");
        networkTransfer.setEntityNumber("SHP123");
        networkTransfer.setStatus(NetworkTransferStatus.RETRANSFERRED);

        Map<Integer, Object> tenantModelMap = new HashMap<>(Map.of(1, new Object()));

        doAnswer(invocation -> {
            Map<String, String> mapArg = invocation.getArgument(1);
            mapArg.put("XYZ", "xyz@example.com");
            return null;
        }).when(commonUtils).getUserDetails(eq(Set.of("XYZ")), anyMap());
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(networkTransfer));
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(tenantModelMap);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(new TenantModel());
        var response = entityTransferService.importShipment(CommonRequestModel.buildRequest(importShipmentRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportShipment_Rejection2() {
        ImportShipmentRequest importShipmentRequest = ImportShipmentRequest.builder()
                .taskId(1L)
                .operation(TaskStatus.REJECTED.getDescription())
                .rejectRemarks("Test rejection msg")
                .isFromNte(true)
                .build();
        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(importShipmentRequest);

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        mockShipmentSettings();
        NetworkTransfer networkTransfer = new NetworkTransfer();
        networkTransfer.setUpdatedBy("XYZ");
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(networkTransfer));
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        assertThrows(ValidationException.class, () -> entityTransferService.importShipment(commonRequestModel));
    }

    @Test
    void testImportShipment_Rejection3() {
        ImportShipmentRequest importShipmentRequest = ImportShipmentRequest.builder()
                .taskId(1L)
                .operation(TaskStatus.REJECTED.getDescription())
                .rejectRemarks("Test rejection msg")
                .isFromNte(true)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(importShipmentRequest);

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(false);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> entityTransferService.importShipment(commonRequestModel));
    }

    @Test
    void testImportConsolidation_Rejection() throws RunnerException {
        ImportConsolidationRequest importConsolidationRequest = ImportConsolidationRequest.builder()
                .taskId(1L)
                .operation(TaskStatus.REJECTED.getDescription())
                .rejectRemarks("Test rejection msg")
                .isFromNte(true)
                .build();
        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);
        UserContext.getUser().setDisplayName("abc");
        UserContext.getUser().setEmail("abc@xyz.com");

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        mockShipmentSettings();
        NetworkTransfer networkTransfer = new NetworkTransfer();
        networkTransfer.setUpdatedBy("XYZ");
        networkTransfer.setEntityNumber("SHP123");
        networkTransfer.setStatus(NetworkTransferStatus.RETRANSFERRED);

        Map<Integer, Object> tenantModelMap = new HashMap<>(Map.of(1, new Object()));

        doAnswer(invocation -> {
            Map<String, String> mapArg = invocation.getArgument(1);
            mapArg.put("XYZ", "xyz@example.com");
            return null;
        }).when(commonUtils).getUserDetails(eq(Set.of("XYZ")), anyMap());
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(networkTransfer));
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(tenantModelMap);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(new TenantModel());
        var response = entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendFileToExternalSystem_Shipment() throws RunnerException {
        SendFileToExternalRequest request = SendFileToExternalRequest.builder()
                .entityId(123L)
                .entityType(Constants.SHIPMENT)
                .sendToBranch("XYZ")
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        EntityTransferV3ShipmentDetails entityTransferShipmentDetails = EntityTransferV3ShipmentDetails.builder()
                .shipmentId("SHP123")
                .transportMode("AIR")
                .build();

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(ShipmentDetails.builder().build()));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(entityTransferShipmentDetails);
        when(jsonHelper.convertToJson(any())).thenReturn("Example");
        when(jsonHelper.convertJsonToMap(any())).thenReturn(Map.of());
        var response = entityTransferService.sendFileToExternalSystem(commonRequestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendFileToExternalSystem_Shipment_Error() {
        SendFileToExternalRequest request = SendFileToExternalRequest.builder()
                .entityId(123L)
                .entityType(Constants.SHIPMENT)
                .sendToBranch("XYZ")
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> entityTransferService.sendFileToExternalSystem(commonRequestModel));
    }

    @Test
    void testSendFileToExternalSystem_Consolidation() throws RunnerException {
        SendFileToExternalRequest request = SendFileToExternalRequest.builder()
                .entityId(123L)
                .entityType(Constants.CONSOLIDATION)
                .sendToBranch("XYZ")
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        EntityTransferV3ConsolidationDetails entityTransferConsolidationDetails = EntityTransferV3ConsolidationDetails.builder()
                .consolidationNumber("CONS233")
                .transportMode("SEA")
                .build();

        var console = ConsolidationDetails.builder()
                .shipmentsList(Set.of())
                .build();
        console.setTenantId(12);

        V1TenantResponse mockV1TenantResponse = V1TenantResponse.builder().TenantName("mockTenant").build();
        Map<Integer, Object> mockTenantNameMap = Map.ofEntries(
                Map.entry(12, mockV1TenantResponse)
        );

        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(console));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ConsolidationDetails.class))).thenReturn(entityTransferConsolidationDetails);
        when(jsonHelper.convertToJson(any())).thenReturn("Example");
        when(jsonHelper.convertJsonToMap(any())).thenReturn(Map.of());
        var response = entityTransferService.sendFileToExternalSystem(commonRequestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendFileToExternalSystem_Consolidation_Error() {
        SendFileToExternalRequest request = SendFileToExternalRequest.builder()
                .entityId(123L)
                .entityType(Constants.CONSOLIDATION)
                .sendToBranch("XYZ")
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> entityTransferService.sendFileToExternalSystem(commonRequestModel));
    }
    @Test
    void testSetPackingVsContainerGuid() throws Exception {
        // Arrange
        EntityTransferService service = new EntityTransferService();

        EntityTransferV3ConsolidationDetails payload = new EntityTransferV3ConsolidationDetails();
        payload.setPackingVsContainerGuid(new HashMap<>()); // initialize map

        UUID packingGuid = UUID.randomUUID();
        UUID containerGuid = UUID.randomUUID();

        Map<UUID, UUID> packingVsContainerGuid = new HashMap<>();
        packingVsContainerGuid.put(packingGuid, containerGuid);

        // Use reflection to access private method
        Method method = EntityTransferService.class.getDeclaredMethod(
                "setPackingVsContainerGuid",
                EntityTransferV3ConsolidationDetails.class,
                Map.class
        );
        method.setAccessible(true);

        // Act
        method.invoke(service, payload, packingVsContainerGuid);

        // Assert
        assertNotNull(payload.getPackingVsContainerGuid());
        assertEquals(1, payload.getPackingVsContainerGuid().size());
        assertEquals(containerGuid, payload.getPackingVsContainerGuid().get(packingGuid));
    }

    @Test
    void testEmptyShipNteList_NoSaveAllCalled() throws Exception {
        Map<UUID, Long> oldVsNewShipIds = Map.of(UUID.randomUUID(), 100L);

        when(networkTransferDao.findByEntityGuids(anyList())).thenReturn(Collections.emptyList());

        Method method = EntityTransferService.class
                .getDeclaredMethod("updateInterBranchShipmentStatus", Map.class);
        method.setAccessible(true); // bypass private access

        method.invoke(entityTransferService, oldVsNewShipIds);

        verify(networkTransferDao, never()).saveAll(any());
    }

    @Test
    void testEntityNotInMap_NoUpdateButStillSaved() throws Exception {
        UUID guid1 = UUID.randomUUID();
        Map<UUID, Long> oldVsNewShipIds = Map.of(UUID.randomUUID(), 999L); // different guid

        NetworkTransfer e1 = new NetworkTransfer();
        e1.setEntityGuid(guid1);
        e1.setJobType("EXPORT");

        when(networkTransferDao.findByEntityGuids(anyList())).thenReturn(List.of(e1));

        Method method = EntityTransferService.class
                .getDeclaredMethod("updateInterBranchShipmentStatus", Map.class);
        method.setAccessible(true); // bypass private access

        method.invoke(entityTransferService, oldVsNewShipIds);

        assertNull(e1.getCreatedEntityId()); // not updated
        assertNull(e1.getStatus());          // not updated
        verify(networkTransferDao).saveAll(List.of(e1));
    }

    @Test
    void testJobTypeDirectionCts_ExcludedFromSave() throws Exception {
        UUID guid1 = UUID.randomUUID();
        Map<UUID, Long> oldVsNewShipIds = Map.of(guid1, 200L);

        NetworkTransfer e1 = new NetworkTransfer();
        e1.setEntityGuid(guid1);
        e1.setJobType("CTS"); // assume DIRECTION_CTS == "CTS"

        when(networkTransferDao.findByEntityGuids(anyList())).thenReturn(List.of(e1));
        Method method = EntityTransferService.class
                .getDeclaredMethod("updateInterBranchShipmentStatus", Map.class);
        method.setAccessible(true);

        method.invoke(entityTransferService, oldVsNewShipIds);
        // Should NOT save, because it gets filtered out
        verify(networkTransferDao).saveAll(List.of());
    }
    private Object invokePrivateMethod(String methodName, Class<?>[] parameterTypes, Object... args) throws Exception {
        Method method = entityTransferService.getClass().getDeclaredMethod(methodName, parameterTypes);
        method.setAccessible(true);
        return method.invoke(shipmentService, args);
    }

    @Test
    void shouldSaveShipment_WhenSeaTransportButNotExport_ReturnsFalse() {
        when(shipmentData.getTransportMode()).thenReturn("SEA");
        when(shipmentData.getDirection()).thenReturn("IMPORT");

        boolean result = (Boolean) ReflectionTestUtils.invokeMethod(
                entityTransferService, "shouldSaveShipment", shipmentData
        );

        assertFalse(result);
    }

    @Test
    void shouldSaveShipment_WhenSeaTransportAndExportDirection_ReturnsTrue() {
        when(shipmentData.getTransportMode()).thenReturn("SEA");
        when(shipmentData.getDirection()).thenReturn("EXP");

        boolean result = (Boolean) ReflectionTestUtils.invokeMethod(
                entityTransferService, "shouldSaveShipment", shipmentData
        );

        assertTrue(result);
    }


    @Test
    void updateNteStatus_WhenFeatureEnabledAndNtePresentAndNotExternalSource_ShouldUpdateStatusAndEntity() throws Exception {
        // Given
        boolean isNetworkTransferFeatureEnabled = true;
        ImportShipmentRequest importShipmentRequest = mock(ImportShipmentRequest.class);
        ShipmentDetailsResponse shipmentDetailsResponse = mock(ShipmentDetailsResponse.class);

        Long taskId = 123L;
        Long entityId = 456L;
        Integer tenantId = 789;
        Long receivingBranchId = 789L;

        NetworkTransfer nte = mock(NetworkTransfer.class);

        when(importShipmentRequest.getTaskId()).thenReturn(taskId);
        when(shipmentDetailsResponse.getId()).thenReturn(entityId);
        when(shipmentDetailsResponse.getReceivingBranch()).thenReturn(receivingBranchId);
        when(shipmentDetailsResponse.getTriangulationPartnerList()).thenReturn(null);

        when(networkTransferDao.findById(taskId)).thenReturn(Optional.of(nte));
        when(nte.getSource()).thenReturn(NetworkTransferSource.CARGOES_RUNNER);
        when(nte.getEntityId()).thenReturn(entityId);

        try (var mockedTenantContext = mockStatic(TenantContext.class)) {
            // Return Integer instead of String
            mockedTenantContext.when(TenantContext::getCurrentTenant).thenReturn(tenantId);

            // When - Use ReflectionTestUtils
            ReflectionTestUtils.invokeMethod(entityTransferService, "updateNteStatus",
                    isNetworkTransferFeatureEnabled, importShipmentRequest, shipmentDetailsResponse);

            // Then
            verify(networkTransferService).updateStatusAndCreatedEntityId(taskId, "ACCEPTED", entityId);
            verify(networkTransferDao).findById(taskId);
            verify(shipmentDao).saveIsTransferredToReceivingBranch(entityId, Boolean.TRUE);
        }
    }

    @Test
    void attachShipmentToContainers_WhenOldContVsOldShipGuidMapContainsGuid_ShouldCreateAndAddShipments() throws Exception {
        // Given
        Long consoleId = 1L;

        // Setup container GUID mapping
        UUID newContainerGuid = UUID.randomUUID();
        UUID oldContainerGuid = UUID.randomUUID();
        Map<UUID, UUID> newVsOldContainerGuid = new HashMap<>();
        newVsOldContainerGuid.put(newContainerGuid, oldContainerGuid);

        // Setup shipment GUID mapping - this is the key condition being tested
        UUID oldShipGuid1 = UUID.randomUUID();
        UUID oldShipGuid2 = UUID.randomUUID();
        Map<UUID, List<UUID>> oldContVsOldShipGuidMap = new HashMap<>();
        oldContVsOldShipGuidMap.put(oldContainerGuid, Arrays.asList(oldShipGuid1, oldShipGuid2));

        // Setup shipment ID mapping
        Long newShipId1 = 200L;
        Long newShipId2 = 300L;
        Map<UUID, Long> oldVsNewShipIds = new HashMap<>();
        oldVsNewShipIds.put(oldShipGuid1, newShipId1);
        oldVsNewShipIds.put(oldShipGuid2, newShipId2);

        Map<UUID, Long> oldGuidVsNewContainerId = new HashMap<>();

        // Create a real container to verify the state changes
        Containers container = new Containers();
        container.setGuid(newContainerGuid);
        container.setId(100L);

        List<Containers> containersList = Arrays.asList(container);

        when(containerDao.findByConsolidationId(consoleId)).thenReturn(containersList);
        when(containerDao.saveAll(anyList())).thenReturn(containersList);

        // When
        ReflectionTestUtils.invokeMethod(entityTransferService, "attachShipmentToContainers",
                consoleId, newVsOldContainerGuid, oldContVsOldShipGuidMap, oldVsNewShipIds, oldGuidVsNewContainerId);

        // Then - Focus on testing the specific code section
        // 1. Verify that oldContVsOldShipGuidMap contained the container GUID
        assertTrue(oldContVsOldShipGuidMap.containsKey(oldContainerGuid));

        // 2. Verify that shipment GUIDs were retrieved correctly
        List<UUID> expectedShipmentGuids = Arrays.asList(oldShipGuid1, oldShipGuid2);
        assertEquals(expectedShipmentGuids, oldContVsOldShipGuidMap.get(oldContainerGuid));

        // 3. Verify that shipments were created with correct IDs
        Set<ShipmentDetails> shipments = container.getShipmentsList();
        assertNotNull(shipments);
        assertEquals(2, shipments.size());

        // 4. Verify each shipment has the correct ID mapped from oldVsNewShipIds
        Set<Long> actualShipmentIds = shipments.stream()
                .map(ShipmentDetails::getId)
                .collect(Collectors.toSet());

        Set<Long> expectedShipmentIds = Set.of(newShipId1, newShipId2);
        assertEquals(expectedShipmentIds, actualShipmentIds);

        // 5. Verify that the shipment details were properly created and added
        assertTrue(shipments.stream().anyMatch(s -> s.getId().equals(newShipId1)));
        assertTrue(shipments.stream().anyMatch(s -> s.getId().equals(newShipId2)));

        // 6. Verify that all shipment GUIDs were processed
        assertEquals(oldContVsOldShipGuidMap.get(oldContainerGuid).size(), shipments.size());
    }

    @Test
    void isInterBranchConsoleCase_WhenAllConditionsTrue_ReturnsTrue() throws Exception {
        // Given
        ShipmentDetails shipment = mock(ShipmentDetails.class);
        ConsolidationDetails consolidationDetails = mock(ConsolidationDetails.class);
        Long receivingBranch = 123L;
        when(consolidationDetails.getInterBranchConsole()).thenReturn(Boolean.TRUE);
        when(shipment.getReceivingBranch()).thenReturn(null);

        boolean result = (Boolean) ReflectionTestUtils.invokeMethod(entityTransferService, "isInterBranchConsoleCase", shipment, consolidationDetails, receivingBranch);

        assertTrue(result);
    }
    @Test
    void isInterBranchConsoleCase_WhenShipmentHasReceivingBranch_ReturnsFalse() throws Exception {
        // Given
        ShipmentDetails shipment = mock(ShipmentDetails.class);
        ConsolidationDetails consolidationDetails = mock(ConsolidationDetails.class);
        Long receivingBranch = 123L;

        when(consolidationDetails.getInterBranchConsole()).thenReturn(Boolean.TRUE);
        when(shipment.getReceivingBranch()).thenReturn(456L); // Not null

        // When
        boolean result = (Boolean) ReflectionTestUtils.invokeMethod(entityTransferService,
                "isInterBranchConsoleCase", shipment, consolidationDetails, receivingBranch);

        // Then
        assertFalse(result);
    }

    @Test
    void validateConditionsForHAWBNumber_AllScenarios_CorrectlyValidates() throws Exception {
        // Test Case 1: Direction is CTS - should return TRUE
        assertTrue((Boolean) ReflectionTestUtils.invokeMethod(entityTransferService,
                "validateConditionsForHAWBNumber", "CTS", "ANY_TYPE", "HAWB123"));

        // Test Case 2: Direction is IMP - should return TRUE
        assertTrue((Boolean) ReflectionTestUtils.invokeMethod(entityTransferService,
                "validateConditionsForHAWBNumber", "IMP", "ANY_TYPE", "HAWB123"));

        // Test Case 3: Non-standard shipment type + empty house bill - should return TRUE
        assertTrue((Boolean) ReflectionTestUtils.invokeMethod(entityTransferService,
                "validateConditionsForHAWBNumber", "EXP", "NON_STD_TYPE", null));

        // Test Case 4: Standard shipment type - should return FALSE
        assertFalse((Boolean) ReflectionTestUtils.invokeMethod(entityTransferService,
                "validateConditionsForHAWBNumber", "EXP", "STD", null));

        // Test Case 5: DRT shipment type - should return FALSE
        assertFalse((Boolean) ReflectionTestUtils.invokeMethod(entityTransferService,
                "validateConditionsForHAWBNumber", "EXP", "DRT", null));

        // Test Case 6: Non-standard shipment type but has house bill - should return FALSE
        assertFalse((Boolean) ReflectionTestUtils.invokeMethod(entityTransferService,
                "validateConditionsForHAWBNumber", "EXP", "NON_STD_TYPE", "HAWB123"));

        // Test Case 7: Empty direction - should return FALSE (falls to third condition but shipment type is STD)
        assertFalse((Boolean) ReflectionTestUtils.invokeMethod(entityTransferService,
                "validateConditionsForHAWBNumber", null, "STD", null));
    }

    @Test
    void getIsV3TenantPresent_WhenBothV2AndV3TenantsPresent_ReturnsTrue() {
        // Given
        ShipmentDetails shipmentDetails = mock(ShipmentDetails.class);
        ConsolidationDetails consolidationDetails = mock(ConsolidationDetails.class);

        // Mock consolidation details to trigger that path
        when(consolidationDetails.getReceivingBranch()).thenReturn(123L);
        when(consolidationDetails.getTenantId()).thenReturn(456);
        when(consolidationDetails.getTriangulationPartnerList()).thenReturn(Arrays.asList(
                mock(TriangulationPartner.class), mock(TriangulationPartner.class)
        ));

        // Mock shipment settings with both V2 and V3 tenants
        ShipmentSettingsDetails v3Settings = new ShipmentSettingsDetails();
        v3Settings.setIsRunnerV3Enabled(Boolean.TRUE);

        ShipmentSettingsDetails v2Settings = new ShipmentSettingsDetails();
        v2Settings.setIsRunnerV3Enabled(Boolean.FALSE);

        List<ShipmentSettingsDetails> settingsList = Arrays.asList(v3Settings, v2Settings);
        when(shipmentSettingsDao.getSettingsByTenantIds(anyList())).thenReturn(settingsList);

        // When - Call the public method directly
        boolean result = entityTransferService.getIsV3TenantPresent(shipmentDetails, consolidationDetails);

        // Then
        assertTrue(result);
        verify(shipmentSettingsDao).getSettingsByTenantIds(anyList());
    }

    @Test
    void getIsV3TenantPresent_WhenOnlyV3TenantsPresent_ReturnsFalse() {
        // Given
        ConsolidationDetails consolidationDetails = mock(ConsolidationDetails.class);
        when(consolidationDetails.getReceivingBranch()).thenReturn(123L);

        // Only V3 tenants
        ShipmentSettingsDetails v3Settings1 = new ShipmentSettingsDetails();
        v3Settings1.setIsRunnerV3Enabled(Boolean.TRUE);

        ShipmentSettingsDetails v3Settings2 = new ShipmentSettingsDetails();
        v3Settings2.setIsRunnerV3Enabled(Boolean.TRUE);

        List<ShipmentSettingsDetails> settingsList = Arrays.asList(v3Settings1, v3Settings2);
        when(shipmentSettingsDao.getSettingsByTenantIds(anyList())).thenReturn(settingsList);

        // When
        boolean result = entityTransferService.getIsV3TenantPresent(mock(ShipmentDetails.class), consolidationDetails);

        // Then
        assertFalse(result);
    }

    @Test
    void getIsV3TenantPresent_WhenOnlyV2TenantsPresent_ReturnsFalse() {
        // Given
        ConsolidationDetails consolidationDetails = mock(ConsolidationDetails.class);
        when(consolidationDetails.getReceivingBranch()).thenReturn(123L);
        ShipmentSettingsDetails v2Settings1 = new ShipmentSettingsDetails();
        v2Settings1.setIsRunnerV3Enabled(Boolean.FALSE);

        ShipmentSettingsDetails v2Settings2 = new ShipmentSettingsDetails();
        v2Settings2.setIsRunnerV3Enabled(Boolean.FALSE);

        List<ShipmentSettingsDetails> settingsList = Arrays.asList(v2Settings1, v2Settings2);
        when(shipmentSettingsDao.getSettingsByTenantIds(anyList())).thenReturn(settingsList);
        boolean result = entityTransferService.getIsV3TenantPresent(mock(ShipmentDetails.class), consolidationDetails);
        assertFalse(result);
    }

    @Test
    void getIsV3TenantPresent_WhenNoSettingsFound_ReturnsFalse() {
        // Given
        ConsolidationDetails consolidationDetails = mock(ConsolidationDetails.class);
        when(consolidationDetails.getReceivingBranch()).thenReturn(123L);

        // Empty or null settings list
        when(shipmentSettingsDao.getSettingsByTenantIds(anyList())).thenReturn(Collections.emptyList());

        // When
        boolean result = entityTransferService.getIsV3TenantPresent(mock(ShipmentDetails.class), consolidationDetails);

        // Then
        assertFalse(result);
    }

    @Test
    void getIsV3TenantPresent_WhenConsolidationDetailsNull_UsesShipmentDetails() {
        // Given
        ShipmentDetails shipmentDetails = mock(ShipmentDetails.class);
        when(shipmentDetails.getReceivingBranch()).thenReturn(123L);
        when(shipmentDetails.getTenantId()).thenReturn(456);

        ConsolidationDetails consolidationDetails = null; // This should trigger shipmentDetails path

        ShipmentSettingsDetails v3Settings = new ShipmentSettingsDetails();
        v3Settings.setIsRunnerV3Enabled(Boolean.TRUE);

        ShipmentSettingsDetails v2Settings = new ShipmentSettingsDetails();
        v2Settings.setIsRunnerV3Enabled(Boolean.FALSE);

        List<ShipmentSettingsDetails> settingsList = Arrays.asList(v3Settings, v2Settings);
        when(shipmentSettingsDao.getSettingsByTenantIds(anyList())).thenReturn(settingsList);

        // When
        boolean result = entityTransferService.getIsV3TenantPresent(shipmentDetails, consolidationDetails);

        // Then
        assertTrue(result);
        verify(shipmentSettingsDao).getSettingsByTenantIds(anyList());
    }

    @Test
    void getIsV3TenantPresent_WhenSettingsListIsNull_ReturnsFalse() {
        // Given
        ConsolidationDetails consolidationDetails = mock(ConsolidationDetails.class);
        when(consolidationDetails.getReceivingBranch()).thenReturn(123L);

        // Null settings list
        when(shipmentSettingsDao.getSettingsByTenantIds(anyList())).thenReturn(null);

        // When
        boolean result = entityTransferService.getIsV3TenantPresent(mock(ShipmentDetails.class), consolidationDetails);

        // Then
        assertFalse(result);
    }

    @Test
    void mapShipmentDataToProfitShare_WithTransferStatusAndBranchId_ShouldReturnCorrectProfitShareShipmentData() {
        // Arrange
        TransferStatus transferStatus = TransferStatus.NOT_TRANSFERRED;
        int branchId = 123;

        // Act
        ArValidationResponse.ProfitShareShipmentData result = ReflectionTestUtils.invokeMethod(
                entityTransferService, "mapShipmentDataToProfitShare", transferStatus, branchId
        );

        // Assert
        assertNotNull(result);
        assertEquals(branchId, result.getBranchId());
        assertEquals(transferStatus, result.getTransferStatus());
    }

    @Test
    void testSendConsolidationValidation_Air_AllConsoleFieldsMissing() {
        // Arrange
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setConsolidationType("AGT"); // Not STD or DRT
        consolidationDetails.setShipmentType(Constants.IMP);
        consolidationDetails.setBol(null); // MAWB is null
        consolidationDetails.getCarrierDetails().setFlightNumber(null);
        consolidationDetails.getCarrierDetails().setEta(null);
        consolidationDetails.getCarrierDetails().setEtd(null);
        consolidationDetails.setReceivingBranch(null);
        consolidationDetails.setTriangulationPartner(null);
        consolidationDetails.setTriangulationPartnerList(null);
        consolidationDetails.getShipmentsList().clear(); // No shipments to avoid HAWB errors

        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));

        // Act
        Exception exception = assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));

        // Assert
        // The code adds "MAWB Number" twice because two conditions are met. The test must reflect this.
        String expectedMessage = "Please enter the Flight Number, Eta, Etd, one of the branches in the entity transfer details section, MAWB Number, MAWB Number for the consolidation to transfer the files.";
        assertEquals(expectedMessage, exception.getMessage());
    }

    @Test
    void testSendConsolidationValidation_Air_MawbGenerationError() {
        // Arrange
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setConsolidationType(Constants.SHIPMENT_TYPE_STD);
        consolidationDetails.setShipmentType(Constants.DIRECTION_EXP);
        consolidationDetails.getCarrierDetails().setFlightNumber("FL123");
        consolidationDetails.getShipmentsList().clear(); // No shipments to avoid HAWB errors

        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(awbDao.findByConsolidationId(consolidationDetails.getId())).thenReturn(Collections.emptyList());

        // Act
        Exception exception = assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));

        // Assert
        String expectedMessage = "Please generate MAWB for the consolidation to transfer the files.";
        assertEquals(expectedMessage, exception.getMessage());
    }

    @Test
    void testSendConsolidationValidation_Air_HawbErrors() {
        // Arrange
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setConsolidationType(Constants.SHIPMENT_TYPE_STD);
        consolidationDetails.setShipmentType(Constants.DIRECTION_EXP);
        consolidationDetails.getCarrierDetails().setFlightNumber("FL123"); // Satisfy console-level validation

        // Setup Shipment 1: Fails HAWB generation check ONLY
        ShipmentDetails shipment1 = new ShipmentDetails();
        shipment1.setId(1L);
        shipment1.setShipmentId("SHIP001");
        shipment1.setJobType(Constants.SHIPMENT_TYPE_STD);
        shipment1.setDirection(Constants.DIRECTION_EXP);
        shipment1.setHouseBill("HAWB001"); // Has a number, so it should pass the number check

        // Setup Shipment 2: Fails HAWB number check ONLY
        ShipmentDetails shipment2 = new ShipmentDetails();
        shipment2.setId(2L);
        shipment2.setShipmentId("SHIP002");
        shipment2.setJobType(Constants.SHIPMENT_TYPE_STD);
        shipment2.setDirection(Constants.DIRECTION_IMP);
        shipment2.setHouseBill(null); // Missing HAWB number, so it should fail the number check

        consolidationDetails.setShipmentsList(new LinkedHashSet<>(Arrays.asList(shipment1, shipment2)));

        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(awbDao.findByConsolidationId(consolidationDetails.getId())).thenReturn(List.of(new Awb())); // MAWB is generated

        // Mocks to isolate the two error conditions
        when(awbDao.findByShipmentId(shipment1.getId())).thenReturn(Collections.emptyList()); // Fails generation check
        when(awbDao.findByShipmentId(shipment2.getId())).thenReturn(List.of(new Awb())); // Passes generation check

        // Act
        Exception exception = assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));

        // Assert
        String expectedMessage = "Please generate HAWB for the shipment/s SHIP001, SHIP002 and enter the HAWB number for the shipment/s SHIP001, SHIP002 to transfer the files.";
        assertEquals(expectedMessage, exception.getMessage());
    }


    @Test
    void testSendConsolidationValidation_Air_CombinedConsoleAndShipmentErrors() {
        // Arrange
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setConsolidationType(Constants.SHIPMENT_TYPE_STD);
        consolidationDetails.setShipmentType(Constants.DIRECTION_EXP);
        consolidationDetails.getCarrierDetails().setFlightNumber(null); // Missing console field

        // Setup Shipment for HAWB generation error
        ShipmentDetails shipment1 = new ShipmentDetails();
        shipment1.setId(1L);
        shipment1.setShipmentId("SHIP001");
        shipment1.setJobType(Constants.SHIPMENT_TYPE_STD);
        shipment1.setDirection(Constants.DIRECTION_EXP);
        consolidationDetails.setShipmentsList(new HashSet<>(Collections.singletonList(shipment1)));

        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));
        when(awbDao.findByConsolidationId(consolidationDetails.getId())).thenReturn(List.of(new Awb())); // MAWB is generated
        when(awbDao.findByShipmentId(shipment1.getId())).thenReturn(Collections.emptyList()); // HAWB not generated

        // Act
        Exception exception = assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));

        // Assert
        String expectedMessage = "Please enter the Flight Number for the consolidation and generate HAWB for the shipment/s SHIP001 to transfer the files.";
        assertEquals(expectedMessage, exception.getMessage());
    }

    @Test
    void testSendConsolidationValidation_Sea_HblNumberError() {
        // Arrange
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        // Setup a shipment that will fail the HBL number check
        ShipmentDetails impShipment = new ShipmentDetails();
        impShipment.setId(1L);
        impShipment.setShipmentId("SHIP_IMP_001");
        impShipment.setDirection(Constants.DIRECTION_IMP); // Non-EXP direction
        impShipment.setHouseBill(null); // Missing HBL number
        impShipment.setJobType(Constants.SHIPMENT_TYPE_STD);

        consolidationDetails.setShipmentsList(new HashSet<>(Collections.singletonList(impShipment)));

        ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(consolidationDetails.getId()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(request.getConsoleId())).thenReturn(Optional.of(consolidationDetails));

        // Act
        Exception exception = assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));

        // Assert - This covers the 'else' branch where errorMsg is initially empty
        String expectedMessage = "Please enter the HBL number for the shipment/s SHIP_IMP_001 to transfer the files.";
        assertEquals(expectedMessage, exception.getMessage());

        // Arrange for the second part: make a console field invalid
        consolidationDetails.setBol(null); // Missing MBL

        // Act for the second part
        Exception exceptionWithConsoleError = assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidationValidation(commonRequestModel));

        // Assert for the second part
        String expectedCombinedMessage = "Please enter the MBL for the consolidation and enter the HBL number for the shipment/s SHIP_IMP_001 to transfer the files.";
        assertEquals(expectedCombinedMessage, exceptionWithConsoleError.getMessage());
    }

    // In EntityTransferServiceTest.java, replace the previous postArValidation tests with these:

    @Test
    void postArValidation_whenGuidsAreEmpty_shouldThrowRunnerException() {
        PostArValidationRequest request = new PostArValidationRequest(Collections.emptyList(), LocalDateTime.now());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        RunnerException exception = assertThrows(RunnerException.class, () -> entityTransferService.postArValidation(commonRequestModel));
        assertEquals("GUID can't be null. Please provide any one !", exception.getMessage());
    }

    @Test
    void postArValidation_whenInitialShipmentNotFound_shouldReturnEmptyResponseList() throws RunnerException {
        // Arrange
        UUID requestedGuid = UUID.randomUUID();
        PostArValidationRequest request = new PostArValidationRequest(List.of(requestedGuid), LocalDateTime.now());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(logsHistoryService.findByEntityGuidsAndTimeStamp(anyList(), any(LocalDateTime.class))).thenReturn(Collections.emptyList());
        when(shipmentDao.findShipmentsByGuids(anySet())).thenReturn(Collections.emptyList());

        // Act
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.postArValidation(commonRequestModel);

        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        List<ArValidationResponse> responseList = (List<ArValidationResponse>) ((RunnerListResponse) responseEntity.getBody()).getData();
        assertTrue(responseList.isEmpty(), "Response list should be empty when no initial shipments are found.");
    }

    @Test
    void postArValidation_whenOriginShipmentNotFound_shouldReturnMinimalResponse() throws RunnerException {
        // Arrange
        UUID requestedGuid = UUID.randomUUID();
        UUID sourceGuid = UUID.randomUUID(); // A different source GUID that won't be found
        PostArValidationRequest request = new PostArValidationRequest(List.of(requestedGuid), LocalDateTime.now());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        // This is the initial shipment found from logs
        ShipmentDetails initialShipment = new ShipmentDetails();
        initialShipment.setGuid(requestedGuid);
        initialShipment.setSourceGuid(sourceGuid);
        initialShipment.setJobType("STD");
        initialShipment.setTenantId(10);
        initialShipment.setConsolidationList(Collections.emptySet());

        String initialShipmentJson = jsonTestUtility.convertToJson(initialShipment);
        when(jsonHelper.readFromJson(initialShipmentJson, ShipmentDetails.class)).thenReturn(initialShipment);

        // Mock finding the initial shipment
        when(logsHistoryService.findByEntityGuidsAndTimeStamp(eq(List.of(requestedGuid)), any(LocalDateTime.class)))
                .thenReturn(List.of(LogHistoryResponse.builder().entityPayload(initialShipmentJson).build()));

        // Mock finding NO origin shipment (the one with sourceGuid)
        when(logsHistoryService.findByEntityGuidsAndTimeStamp(eq(List.of(sourceGuid)), any(LocalDateTime.class)))
                .thenReturn(Collections.emptyList());

        when(shipmentDao.findShipmentsByGuids(Set.of(requestedGuid))).thenReturn(Collections.emptyList());


        // Act
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.postArValidation(commonRequestModel);

        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        List<ArValidationResponse> responseList = (List<ArValidationResponse>) ((RunnerListResponse) responseEntity.getBody()).getData();
        assertEquals(1, responseList.size());
        ArValidationResponse arResponse = responseList.get(0);

        assertEquals(requestedGuid, arResponse.getShipmentGuid());
        assertEquals("STD", arResponse.getConsolidationType());
        assertEquals(10, arResponse.getSourceBranch());
        assertNull(arResponse.getOriginShipment(), "Origin shipment should be null as it was not found");
        assertNull(arResponse.getReceivingAgent(), "Receiving agent should be null");
    }

    @Test
    void postArValidation_simpleShipment_notTransferred_ntePath() throws RunnerException {
        // Arrange
        UUID shipmentGuid = UUID.randomUUID();
        LocalDateTime timestamp = LocalDateTime.now();
        PostArValidationRequest request = new PostArValidationRequest(List.of(shipmentGuid), timestamp);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        ShipmentDetails originShipment = new ShipmentDetails();
        originShipment.setGuid(shipmentGuid);
        originShipment.setId(0L);
        originShipment.setSourceGuid(shipmentGuid); // It's an origin shipment
        originShipment.setTenantId(10);
        originShipment.setReceivingBranch(20L);
        originShipment.setConsolidationList(Collections.emptySet());

        String originShipmentJson = jsonTestUtility.convertToJson(originShipment);
        when(jsonHelper.readFromJson(originShipmentJson, ShipmentDetails.class)).thenReturn(originShipment);

        mockShipmentSettings();
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isNetworkTransferEntityEnabled(true).build());
        when(logsHistoryService.findByEntityGuidsAndTimeStamp(List.of(shipmentGuid), timestamp)).thenReturn(List.of(LogHistoryResponse.builder().entityPayload(originShipmentJson).build()));
        when(shipmentDao.findShipmentsByParentGuids(Set.of(shipmentGuid))).thenReturn(Collections.emptyList());
        when(networkTransferDao.findByEntityAndTenantList(anyLong(), eq(SHIPMENT), anyList())).thenReturn(Collections.emptyList());

        // Act
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.postArValidation(commonRequestModel);

        // Assert
        List<ArValidationResponse> responseList = (List<ArValidationResponse>) ((RunnerListResponse) responseEntity.getBody()).getData();
        ArValidationResponse arResponse = responseList.get(0);

        assertNull(arResponse.getTransferToReceivingAgent());
        assertNotNull(arResponse.getReceivingShipment());
        assertEquals(TransferStatus.NOT_TRANSFERRED, arResponse.getReceivingShipment().getTransferStatus());
        assertEquals(20, arResponse.getReceivingShipment().getBranchId());
    }

    @Test
    void postArValidation_consoleShipment_transferredToReceivingAndTriangulation_taskPath() throws RunnerException {
        // Arrange
        UUID shipmentGuid = UUID.randomUUID();
        UUID consoleGuid = UUID.randomUUID();
        LocalDateTime timestamp = LocalDateTime.now();
        PostArValidationRequest request = new PostArValidationRequest(List.of(shipmentGuid), timestamp);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        // --- Origin Entities ---
        ConsolidationDetails originConsole = new ConsolidationDetails();
        originConsole.setGuid(consoleGuid);
        originConsole.setReceivingBranch(20L);
        originConsole.setTriangulationPartner(30L);

        ShipmentDetails originShipment = new ShipmentDetails();
        originShipment.setGuid(shipmentGuid);
        originShipment.setSourceGuid(shipmentGuid);
        originShipment.setTenantId(10);
        originShipment.setConsolidationList(Set.of(originConsole));

        // --- Destination Entity ---
        ShipmentDetails destShipment = new ShipmentDetails();
        destShipment.setGuid(UUID.randomUUID());
        destShipment.setParentGuid(shipmentGuid);
        destShipment.setTenantId(20); // Belongs to receiving agent

        // --- JSON and Mocking ---
        String originShipmentJson = jsonTestUtility.convertToJson(originShipment);
        String originConsoleJson = jsonTestUtility.convertToJson(originConsole);
        String destShipmentJson = jsonTestUtility.convertToJson(destShipment);

        when(jsonHelper.readFromJson(originShipmentJson, ShipmentDetails.class)).thenReturn(originShipment);
        when(jsonHelper.readFromJson(originConsoleJson, ConsolidationDetails.class)).thenReturn(originConsole);
        when(jsonHelper.readFromJson(destShipmentJson, ShipmentDetails.class)).thenReturn(destShipment);

        mockShipmentSettings();
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isNetworkTransferEntityEnabled(false).build()); // Task path

        // Mock historical data fetching
        when(logsHistoryService.findByEntityGuidsAndTimeStamp(List.of(shipmentGuid), timestamp))
                .thenReturn(List.of(LogHistoryResponse.builder().entityPayload(originShipmentJson).build()));
        when(logsHistoryService.findByEntityGuidsAndTimeStamp(List.of(consoleGuid), timestamp))
                .thenReturn(List.of(LogHistoryResponse.builder().entityPayload(originConsoleJson).build()));

        // Mock destination data fetching
        when(shipmentDao.findShipmentsByParentGuids(Set.of(shipmentGuid))).thenReturn(List.of(destShipment));
        when(logsHistoryService.findByEntityGuidsAndTimeStamp(List.of(destShipment.getGuid()), timestamp))
                .thenReturn(List.of(LogHistoryResponse.builder().entityPayload(destShipmentJson).build()));

        // Mock task service for pending triangulation transfer
        TaskCreateRequest pendingTask = new TaskCreateRequest();
        pendingTask.setTenantId("30");
        when(v1Service.listTask(any())).thenReturn(V1DataResponse.builder().entities(List.of(pendingTask)).build());
        when(jsonHelper.convertValueToList(any(), eq(TaskCreateRequest.class))).thenReturn(List.of(pendingTask));

        // Act
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.postArValidation(commonRequestModel);

        // Assert
        List<ArValidationResponse> responseList = (List<ArValidationResponse>) ((RunnerListResponse) responseEntity.getBody()).getData();
        ArValidationResponse arResponse = responseList.get(0);

        // Assert Receiving Agent
        assertTrue(arResponse.getTransferToReceivingAgent());
        assertNotNull(arResponse.getReceivingShipment());
        assertEquals(TransferStatus.ACCEPTED, arResponse.getReceivingShipment().getTransferStatus());
        assertEquals(20, arResponse.getReceivingShipment().getBranchId());

        // Assert Triangulation Partner
        assertEquals(1, arResponse.getTriangulationShipmentList().size());
        ArValidationResponse.ProfitShareShipmentData triangulationShipment = arResponse.getTriangulationShipmentList().get(0);
        assertEquals(TransferStatus.TRANSFERRED, triangulationShipment.getTransferStatus());
        assertEquals(30, triangulationShipment.getBranchId());
    }

    @Test
    void postArValidation_historicalDataIsUsed() throws RunnerException {
        // Arrange
        UUID shipmentGuid = UUID.randomUUID();
        LocalDateTime timestamp = LocalDateTime.now().minusDays(1); // A time in the past
        PostArValidationRequest request = new PostArValidationRequest(List.of(shipmentGuid), timestamp);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        // Historical version of the shipment
        ShipmentDetails historicalShipment = new ShipmentDetails();
        historicalShipment.setGuid(shipmentGuid);
        historicalShipment.setSourceGuid(shipmentGuid);
        historicalShipment.setTenantId(10);
        historicalShipment.setId(0L);
        historicalShipment.setReceivingBranch(20L); // Historical receiving branch
        historicalShipment.setConsolidationList(Collections.emptySet());

        // Current version of the shipment (different receiving branch)
        ShipmentDetails currentShipment = new ShipmentDetails();
        currentShipment.setGuid(shipmentGuid);
        currentShipment.setSourceGuid(shipmentGuid);
        currentShipment.setTenantId(10);
        currentShipment.setReceivingBranch(99L); // Current receiving branch
        currentShipment.setConsolidationList(Collections.emptySet());

        String historicalShipmentJson = jsonTestUtility.convertToJson(historicalShipment);
        when(jsonHelper.readFromJson(historicalShipmentJson, ShipmentDetails.class)).thenReturn(historicalShipment);

        mockShipmentSettings();
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isNetworkTransferEntityEnabled(true).build());

        // Mock logs history to return the historical version
        when(logsHistoryService.findByEntityGuidsAndTimeStamp(List.of(shipmentGuid), timestamp))
                .thenReturn(List.of(LogHistoryResponse.builder().entityGuid(shipmentGuid).entityPayload(historicalShipmentJson).build()));

        // Mock other dependencies to return empty to simplify the test
        when(shipmentDao.findShipmentsByParentGuids(anySet())).thenReturn(Collections.emptyList());
        when(networkTransferDao.findByEntityAndTenantList(anyLong(), anyString(), anyList())).thenReturn(Collections.emptyList());

        // Act
        ResponseEntity<IRunnerResponse> responseEntity = entityTransferService.postArValidation(commonRequestModel);

        // Assert
        List<ArValidationResponse> responseList = (List<ArValidationResponse>) ((RunnerListResponse) responseEntity.getBody()).getData();
        ArValidationResponse arResponse = responseList.get(0);

        // Assert that the historical receiving agent (20L) was used, not the current one (99L)
        assertEquals(20L, arResponse.getReceivingAgent());
        assertNotNull(arResponse.getReceivingShipment());
        assertEquals(TransferStatus.NOT_TRANSFERRED, arResponse.getReceivingShipment().getTransferStatus());
        assertEquals(20, arResponse.getReceivingShipment().getBranchId());
    }

}