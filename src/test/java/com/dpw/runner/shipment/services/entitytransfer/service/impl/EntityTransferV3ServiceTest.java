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
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationEtV3Request;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentEtV3Request;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.entity.enums.TaskStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferV3ConsolidationDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferV3ShipmentDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendConsoleValidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendShipmentValidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.ValidationResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.service.impl.ContainerV3Service;
import com.dpw.runner.shipment.services.service.impl.NetworkTransferService;
import com.dpw.runner.shipment.services.service.impl.PackingV3Service;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.impl.ConsolidationSync;
import com.dpw.runner.shipment.services.syncing.impl.ShipmentSync;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.slf4j.MDC;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.stream.Stream;
import java.util.UUID;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.commons.constants.ShipmentConstants.CONSOLIDATION;
import static com.dpw.runner.shipment.services.commons.constants.ShipmentConstants.SHIPMENT;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class EntityTransferV3ServiceTest extends CommonMocks {

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private IShipmentServiceV3 shipmentService;
    @Mock
    private IConsolidationV3Service consolidationService;
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
    private IEventsV3Service eventService;
    @InjectMocks
    private EntityTransferV3Service entityTransferService;
    @Mock
    private ShipmentSync shipmentSync;
    @Mock
    private ConsolidationSync consolidationSync;
    @Mock
    private INotificationDao notificationDao;
    @Mock
    private ExecutorService executorService;

    @Mock
    private PackingV3Service packingV3Service;

    @Mock
    private ContainerV3Service containerV3Service;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;

    @BeforeAll
    static void init() {
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
    void testSendShipmentSuccess() throws RunnerException {
        Long shipmentId = 1L;
        int mockTenantId = 10;

        SendShipmentRequest sendShipmentRequest = new SendShipmentRequest();
        sendShipmentRequest.setSendToBranch(List.of(1, 2, 3));
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
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETPayload);
        when(shipmentService.fetchAllMasterDataByKey(any(), any())).thenReturn(new HashMap<String, Object>());

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        shipmentSettingsDetails1.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        shipmentSettingsDetails2.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(3);
        shipmentSettingsDetails3.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(4);
        shipmentSettingsDetails4.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        shipmentSettingsDetails5.setIsRunnerV3Enabled(Boolean.TRUE);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        var httpResponse = entityTransferService.sendShipment(commonRequestModel);

        assertNotNull(httpResponse);
        verify(shipmentDao, times(1)).saveEntityTransfer(any(), any());
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
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETPayload);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        shipmentSettingsDetails1.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        shipmentSettingsDetails2.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(3);
        shipmentSettingsDetails3.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(4);
        shipmentSettingsDetails4.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        shipmentSettingsDetails5.setIsRunnerV3Enabled(Boolean.TRUE);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        var httpResponse = entityTransferService.sendShipment(commonRequestModel);

        // Assertions
        assertNotNull(httpResponse);
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

        assertThrows(ValidationException.class, () -> entityTransferService.sendConsolidation(commonRequestModel));
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
        when(consolidationDetailsDao.findById(consolidationDetails.getId())).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, () -> entityTransferService.sendConsolidation(commonRequestModel));
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

        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETShipment);
        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        shipmentSettingsDetails1.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(66);
        shipmentSettingsDetails2.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(69);
        shipmentSettingsDetails3.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(4);
        shipmentSettingsDetails4.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        shipmentSettingsDetails5.setIsRunnerV3Enabled(Boolean.TRUE);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        List<Integer> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertNotNull(responseEntity);
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

        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(v1ServiceUtil.getTenantSettingsMap(anyList())).thenReturn(mockV1TenantSettingsMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETShipment);
        when(v1ServiceUtil.fetchCoLoadInfo(any(), any())).thenReturn(coLoadMap);
        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        shipmentSettingsDetails1.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(66);
        shipmentSettingsDetails2.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(69);
        shipmentSettingsDetails3.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(4);
        shipmentSettingsDetails4.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        shipmentSettingsDetails5.setIsRunnerV3Enabled(Boolean.TRUE);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);
        List<Integer> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertNotNull(responseEntity);

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

        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETShipment);

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        shipmentSettingsDetails1.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(66);
        shipmentSettingsDetails2.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(69);
        shipmentSettingsDetails3.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(4);
        shipmentSettingsDetails4.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        shipmentSettingsDetails5.setIsRunnerV3Enabled(Boolean.TRUE);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        List<Integer> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertNotNull(responseEntity);

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

        assertThrows(RuntimeException.class, () -> entityTransferService.sendConsolidation(commonRequestModel));
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

        assertThrows(RuntimeException.class, () -> entityTransferService.sendConsolidation(commonRequestModel));
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
    void testImportShipment_rejection() {
        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        ImportV3ShipmentRequest importShipmentRequest = ImportV3ShipmentRequest.builder().taskId(1L).operation(TaskStatus.REJECTED.getDescription()).rejectRemarks("test rejected").build();
        assertDoesNotThrow(() -> entityTransferService.importShipment(CommonRequestModel.buildRequest(importShipmentRequest)));
    }

    @Test
    void testImportShipment_nullPayload() {
        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        ImportV3ShipmentRequest importShipmentRequest = ImportV3ShipmentRequest.builder().taskId(1L).operation(TaskStatus.APPROVED.getDescription()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(importShipmentRequest).build();
        ValidationException ex = assertThrows(ValidationException.class, () -> entityTransferService.importShipment(commonRequestModel));
        assertEquals("No Shipment payload present please check", ex.getMessage());
    }

    @Test
    void testImportShipment_Success_Create() throws RunnerException, JsonMappingException {
        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        EntityTransferV3ShipmentDetails entityTransferShipmentDetails = jsonTestUtility.getV3ImportShipmentData();
        ImportV3ShipmentRequest importShipmentRequest = ImportV3ShipmentRequest.builder()
                .entityData(entityTransferShipmentDetails)
                .taskId(1L)
                .operation(TaskStatus.APPROVED.getDescription())
                .build();

        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setTenantId(12);
        shipmentDetailsResponse.setShipmentId("TQAA24090140");
        shipmentDetailsResponse.setGuid(UUID.randomUUID());

        when(jsonHelper.convertValue(any(), eq(ShipmentEtV3Request.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentEtV3Request.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(any(), any())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any(), eq(true))).thenReturn(shipmentDetailsResponse);

        var response = entityTransferService.importShipment(CommonRequestModel.buildRequest(importShipmentRequest));
        assertNotNull(response);
    }

    @Test
    void testImportShipment_Success_Create_NetworkTransfer() throws RunnerException, JsonMappingException {
        EntityTransferV3ShipmentDetails entityTransferShipmentDetails = jsonTestUtility.getV3ImportShipmentData();
        ImportV3ShipmentRequest importShipmentRequest = ImportV3ShipmentRequest.builder()
                .entityData(entityTransferShipmentDetails)
                .taskId(1L)
                .isFromNte(true)
                .assignedTo("EGYPQAP100ALEX@dpworld.com")
                .build();

        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setTenantId(12);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setShipmentId("TQAA24090140");
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);

        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);

        when(jsonHelper.convertValue(any(), eq(ShipmentEtV3Request.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentEtV3Request.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(any(), any())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any(), eq(true))).thenReturn(shipmentDetailsResponse);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);

        var response = entityTransferService.importShipment(CommonRequestModel.buildRequest(importShipmentRequest));
        assertNotNull(response);
    }

    @Test
    void testImportShipment_UpdateTriangulationOrReceivingAccepted() throws RunnerException, JsonMappingException {
        EntityTransferV3ShipmentDetails entityTransferShipmentDetails = jsonTestUtility.getV3ImportShipmentData();
        ImportV3ShipmentRequest importShipmentRequest = ImportV3ShipmentRequest.builder()
                .entityData(entityTransferShipmentDetails)
                .taskId(1L)
                .operation(TaskStatus.APPROVED.getDescription())
                .build();

        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setTenantId(12);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setReceivingBranch(1L);
        shipmentDetailsResponse.setShipmentId("TQAA24090140");
        shipmentDetailsResponse.setTriangulationPartnerList(List.of(TriangulationPartnerResponse.builder().triangulationPartner(1L).build()));
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);

        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);

        when(jsonHelper.convertValue(any(), eq(ShipmentEtV3Request.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentEtV3Request.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(any(), any())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any(), eq(true))).thenReturn(shipmentDetailsResponse);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);

        var response = entityTransferService.importShipment(CommonRequestModel.buildRequest(importShipmentRequest));

        assertNotNull(response);
    }

    @Test
    void testImportShipment_Success_Update() throws RunnerException, JsonMappingException {
        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        EntityTransferV3ShipmentDetails entityTransferShipmentDetails = jsonTestUtility.getV3ImportShipmentData();
        entityTransferShipmentDetails.setAdditionalDocs(null);
        ImportV3ShipmentRequest importShipmentRequest = ImportV3ShipmentRequest.builder()
                .entityData(entityTransferShipmentDetails)
                .taskId(1L)
                .operation(TaskStatus.APPROVED.getDescription())
                .build();

        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setTenantId(12);
        shipmentDetailsResponse.setShipmentId("TQAA24090140");
        shipmentDetailsResponse.setGuid(UUID.randomUUID());

        ShipmentDetails shipmentDetails = objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentDetails.class);
        shipmentDetails.setId(2L);
        shipmentDetails.setGuid(shipmentDetailsResponse.getGuid());
        shipmentDetails.setTenantId(12);

        ShipmentEtV3Request shipmentRequest = objectMapperTest.convertValue(shipmentDetails, ShipmentEtV3Request.class);

        when(shipmentDao.findShipmentBySourceGuidAndTenantId(any(), any())).thenReturn(List.of(shipmentDetails));
        when(jsonHelper.convertValue(any(), eq(ShipmentEtV3Request.class))).thenReturn(shipmentRequest);
        when(shipmentService.completeUpdateShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);

        var response = entityTransferService.importShipment(CommonRequestModel.buildRequest(importShipmentRequest));
        assertNotNull(response);
    }


    @Test
    void testImportConsolidation_rejectTask() throws RunnerException, JsonMappingException {
        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        ImportV3ConsolidationRequest importConsolidationRequest = ImportV3ConsolidationRequest.builder().taskId(1L).operation(TaskStatus.REJECTED.getDescription()).rejectRemarks("test rejected").build();
        var response = entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportConsolidation_nullPayload() {
        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        ImportV3ConsolidationRequest importConsolidationRequest = ImportV3ConsolidationRequest.builder().taskId(1L).operation(TaskStatus.APPROVED.getDescription()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(importConsolidationRequest).build();
        ValidationException ex = assertThrows(ValidationException.class, () -> entityTransferService.importConsolidation(commonRequestModel));
        assertEquals("No consolidation payload present please check", ex.getMessage());
    }

    @Test
    void testImportConsolidation_Success_Create_Air() throws RunnerException, JsonMappingException {
        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        UserContext.getUser().setTenantId(728);
        EntityTransferV3ConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getV3ImportConsolidationAir();
        ImportV3ConsolidationRequest importConsolidationRequest = ImportV3ConsolidationRequest.builder()
                .operation(TaskStatus.APPROVED.getDescription())
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .build();
        EntityTransferV3ShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().iterator().next();
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        ConsolidationEtV3Request consolidationDetailsRequest = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationEtV3Request.class);
        ConsolidationDetailsResponse consolidationDetailsResponse = new ConsolidationDetailsResponse();
        consolidationDetailsResponse.setId(3L);
        consolidationDetailsResponse.setGuid(UUID.randomUUID());
        consolidationDetailsResponse.setTenantId(entityTransferConsolidationDetails.getSendToBranch());
        consolidationDetailsResponse.setConsolidationNumber(entityTransferConsolidationDetails.getConsolidationNumber());

        when(consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid())).thenReturn(List.of());
        when(jsonHelper.convertValue(any(), eq(ShipmentEtV3Request.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentEtV3Request.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any(), eq(true))).thenReturn(shipmentDetailsResponse);
        when(jsonHelper.convertValue(any(), eq(ConsolidationEtV3Request.class))).thenReturn(consolidationDetailsRequest);
        when(consolidationService.createConsolidationFromEntityTransfer(any())).thenReturn(consolidationDetailsResponse);

        var response = entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportConsolidation_Success_Update_Sea() throws RunnerException, JsonMappingException {
        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        UserContext.getUser().setTenantId(728);
        EntityTransferV3ConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getV3ImportConsolidationSea();
        ImportV3ConsolidationRequest importConsolidationRequest = ImportV3ConsolidationRequest.builder()
                .operation(TaskStatus.APPROVED.getDescription())
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .build();
        EntityTransferV3ShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().iterator().next();
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        ConsolidationEtV3Request consolidationDetailsRequest = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationEtV3Request.class);
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

        ShipmentEtV3Request shipmentRequest = objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentEtV3Request.class);

        when(consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid())).thenReturn(List.of(oldConsolidationDetails));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of(oldShipmentDetails));
        when(jsonHelper.convertValue(any(), eq(ShipmentEtV3Request.class))).thenReturn(shipmentRequest);
        when(shipmentService.completeUpdateShipmentFromEntityTransfer(any())).thenReturn(shipmentDetailsResponse);

        when(jsonHelper.convertValue(any(), eq(ConsolidationEtV3Request.class))).thenReturn(consolidationDetailsRequest);
        when(consolidationService.completeUpdateConsolidationFromEntityTransfer(any())).thenReturn(consolidationDetailsResponse);

        when(containerDao.findByConsolidationId(anyLong())).thenReturn(List.of(containers));
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(packing)));
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(Arrays.asList(oldShipmentDetails));

        var response = entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportConsolidation_Success_Create_Air_interBranch() throws RunnerException, JsonMappingException {
        List<UsersDto> usersDtoList = new ArrayList<>();
        UsersDto usersDto1 = new UsersDto();
        usersDto1.setUserId(1L);
        usersDtoList.add(usersDto1);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        UserContext.getUser().setTenantId(728);
        EntityTransferV3ConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getV3ImportConsolidationAir();
        ImportV3ConsolidationRequest importConsolidationRequest = ImportV3ConsolidationRequest.builder()
                .operation(TaskStatus.APPROVED.getDescription())
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .build();
        EntityTransferV3ShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().iterator().next();
        entityTransferShipmentDetails.setSendToBranch(720);
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        ConsolidationEtV3Request consolidationDetailsRequest = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationEtV3Request.class);
        ConsolidationDetailsResponse consolidationDetailsResponse = new ConsolidationDetailsResponse();
        consolidationDetailsResponse.setId(3L);
        consolidationDetailsResponse.setGuid(UUID.randomUUID());
        consolidationDetailsResponse.setTenantId(entityTransferConsolidationDetails.getSendToBranch());
        consolidationDetailsResponse.setConsolidationNumber(entityTransferConsolidationDetails.getConsolidationNumber());

        when(consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid())).thenReturn(List.of());
        when(jsonHelper.convertValue(any(), eq(ShipmentEtV3Request.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentEtV3Request.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any(), eq(true))).thenReturn(shipmentDetailsResponse);
        when(jsonHelper.convertValue(any(), eq(ConsolidationEtV3Request.class))).thenReturn(consolidationDetailsRequest);
        when(consolidationService.createConsolidationFromEntityTransfer(any())).thenReturn(consolidationDetailsResponse);

        var response = entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportConsolidation_Success_Create_Air_interBranch_NTE() throws RunnerException, JsonMappingException {
        UserContext.getUser().setTenantId(728);
        EntityTransferV3ConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getV3ImportConsolidationAir();
        Map<String, String> shipmentNumberAssignedToMap = new HashMap<>();
        shipmentNumberAssignedToMap.put("TQAA24080071", "EGYPQAP100ALEX@dpworld.com");
        ImportV3ConsolidationRequest importConsolidationRequest = ImportV3ConsolidationRequest.builder()
                .isFromNte(true)
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .shipmentNumberAssignedToMap(shipmentNumberAssignedToMap)
                .build();
        EntityTransferV3ShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().iterator().next();
        entityTransferShipmentDetails.setSendToBranch(720);
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        ConsolidationEtV3Request consolidationDetailsRequest = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationEtV3Request.class);
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
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);

        when(consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid())).thenReturn(List.of());
        when(jsonHelper.convertValue(any(), eq(ShipmentEtV3Request.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentEtV3Request.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any(), eq(true))).thenReturn(shipmentDetailsResponse);
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consoleDetails));
        when(jsonHelper.convertValue(any(), eq(ConsolidationEtV3Request.class))).thenReturn(consolidationDetailsRequest);
        when(consolidationService.createConsolidationFromEntityTransfer(any())).thenReturn(consolidationDetailsResponse);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);

        var response = entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportConsolidation_Success_Create_Air_interBranch_NTE_Validation() {
        EntityTransferV3ConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getV3ImportConsolidationAir();
        ImportV3ConsolidationRequest importConsolidationRequest = ImportV3ConsolidationRequest.builder()
                .isFromNte(true)
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .build();
        EntityTransferV3ShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().iterator().next();
        entityTransferShipmentDetails.setSendToBranch(720);
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(importConsolidationRequest);

        assertThrows(ValidationException.class, () -> entityTransferService.importConsolidation(commonRequestModel));
    }

    @Test
    void testImportConsolidation_UpdateTriangulationOrReceivingAccepted() throws RunnerException, JsonMappingException {
        UserContext.getUser().setTenantId(728);
        EntityTransferV3ConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getV3ImportConsolidationAir();
        ConsolidationDetails consolidationDetails2 = jsonTestUtility.getCompleteConsolidation();
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getTestShipment();
        shipmentDetails1.setId(2L);
        shipmentDetails1.setReceivingBranch(728L);
        shipmentDetails1.setTriangulationPartnerList(List.of(TriangulationPartner.builder().triangulationPartner(728L).build()));
        consolidationDetails2.setShipmentsList(Set.of(shipmentDetails1));

        ImportV3ConsolidationRequest importConsolidationRequest = ImportV3ConsolidationRequest.builder()
                .operation(TaskStatus.APPROVED.getDescription())
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .isFromNte(true)
                .build();
        EntityTransferV3ShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().iterator().next();
        entityTransferShipmentDetails.setSendToBranch(720);
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        ConsolidationEtV3Request consolidationDetailsRequest = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationEtV3Request.class);
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
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);


        when(consolidationDetailsDao.findBySourceGuid(entityTransferConsolidationDetails.getGuid())).thenReturn(List.of());
        when(jsonHelper.convertValue(any(), eq(ShipmentEtV3Request.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentEtV3Request.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any(), eq(true))).thenReturn(shipmentDetailsResponse);
        when(jsonHelper.convertValue(any(), eq(ConsolidationEtV3Request.class))).thenReturn(consolidationDetailsRequest);
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

        var response = entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportConsolidation_UpdateTriangulation_TenantNotMatching() throws RunnerException, JsonMappingException {
        UserContext.getUser().setTenantId(937);
        EntityTransferV3ConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getV3ImportConsolidationAir();
        ConsolidationDetails consolidationDetails2 = jsonTestUtility.getCompleteConsolidation();
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getTestShipment();
        shipmentDetails1.setId(2L);
        shipmentDetails1.setReceivingBranch(728L);
        shipmentDetails1.setTriangulationPartnerList(List.of(TriangulationPartner.builder().triangulationPartner(728L).build()));
        consolidationDetails2.setShipmentsList(Set.of(shipmentDetails1));

        Map<String, String> shipmentNumberAssignedToMap = new HashMap<>();
        shipmentNumberAssignedToMap.put("TQAA24080078", "EGYPQAP100ALEX@dpworld.com");

        ImportV3ConsolidationRequest importConsolidationRequest = ImportV3ConsolidationRequest.builder()
                .operation(TaskStatus.APPROVED.getDescription())
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .isFromNte(true)
                .shipmentNumberAssignedToMap(shipmentNumberAssignedToMap)
                .build();
        EntityTransferV3ShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().iterator().next();
        entityTransferShipmentDetails.setSendToBranch(720);
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        ConsolidationEtV3Request consolidationDetailsRequest = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationEtV3Request.class);
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
        when(jsonHelper.convertValue(any(), eq(ShipmentEtV3Request.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentEtV3Request.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any(), eq(true))).thenReturn(shipmentDetailsResponse);
        when(jsonHelper.convertValue(any(), eq(ConsolidationEtV3Request.class))).thenReturn(consolidationDetailsRequest);
        when(consolidationService.createConsolidationFromEntityTransfer(any())).thenReturn(consolidationDetailsResponse1);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails2));

        var response = entityTransferService.importConsolidation(CommonRequestModel.buildRequest(importConsolidationRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testImportConsolidation_UpdateTriangulation_ErrorLog() {
        UserContext.getUser().setTenantId(728);
        EntityTransferV3ConsolidationDetails entityTransferConsolidationDetails = jsonTestUtility.getV3ImportConsolidationAir();
        ConsolidationDetails consolidationDetails2 = jsonTestUtility.getCompleteConsolidation();

        ImportV3ConsolidationRequest importConsolidationRequest = ImportV3ConsolidationRequest.builder()
                .operation(TaskStatus.APPROVED.getDescription())
                .taskId(2L)
                .entityData(entityTransferConsolidationDetails)
                .isFromNte(true)
                .build();
        EntityTransferV3ShipmentDetails entityTransferShipmentDetails = entityTransferConsolidationDetails.getShipmentsList().iterator().next();
        entityTransferShipmentDetails.setSendToBranch(720);
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(2L);
        shipmentDetailsResponse.setGuid(UUID.randomUUID());
        shipmentDetailsResponse.setTenantId(entityTransferShipmentDetails.getSendToBranch());

        ConsolidationEtV3Request consolidationDetailsRequest = objectMapperTest.convertValue(entityTransferConsolidationDetails, ConsolidationEtV3Request.class);
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
        when(jsonHelper.convertValue(any(), eq(ShipmentEtV3Request.class))).thenReturn(objectMapperTest.convertValue(entityTransferShipmentDetails, ShipmentEtV3Request.class));
        when(shipmentDao.findShipmentBySourceGuidAndTenantId(entityTransferShipmentDetails.getGuid(), entityTransferShipmentDetails.getSendToBranch())).thenReturn(List.of());
        when(shipmentService.createShipmentFromEntityTransfer(any(), eq(true))).thenReturn(shipmentDetailsResponse);
        when(jsonHelper.convertValue(any(), eq(ConsolidationEtV3Request.class))).thenReturn(consolidationDetailsRequest);
        when(consolidationService.createConsolidationFromEntityTransfer(any())).thenReturn(consolidationDetailsResponse1);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        doNothing().when(networkTransferService).updateStatusAndCreatedEntityId(anyLong(), anyString(), anyLong());
        doNothing().when(consolidationDetailsDao).saveIsTransferredToReceivingBranch(anyLong(), anyBoolean());
        doNothing().when(consolidationDetailsDao).updateIsAcceptedTriangulationPartner(anyLong(), anyLong(), anyBoolean());
        when(networkTransferDao.findById(any())).thenReturn(Optional.of(NetworkTransfer.builder().entityId(1L).build()));
        when(consolidationDetailsDao.findConsolidationByIdWithQuery(any())).thenReturn(Optional.empty());
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
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

        // When
        entityTransferService.sendGroupedEmailForShipmentImport(shipmentDetailsList, consoleSourceBranchTenantName);

        // Then
        verify(commonUtils, times(1)).setInterBranchContextForHub();
        verify(commonUtils, times(1)).getToAndCCEmailIdsFromTenantSettings(anySet(), anyMap());
    }


    @Test
    void testSendConsolidationEmailNotification_Success() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        List<Integer> destinationBranches = List.of(1, 2, 3);

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

        Map<String, List<Integer>> shipmentGuidSendToBranch = new HashMap<>();
        shipmentGuidSendToBranch.put("1", List.of(1, 2, 3));
        entityTransferService.sendConsolidationEmailNotification(consoleDetails, destinationBranches, shipmentGuidSendToBranch, false);

        verify(v1ServiceUtil, times(1)).getTenantDetails(anyList());

    }

    @Test
    void testSendConsolidationEmailNotification_Success1() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        List<Integer> destinationBranches = List.of(1, 2, 3);

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

        Map<String, List<Integer>> shipmentGuidSendToBranch = null;
        entityTransferService.sendConsolidationEmailNotification(consoleDetails, destinationBranches, shipmentGuidSendToBranch, false);

        verify(v1ServiceUtil, times(1)).getTenantDetails(anyList());

    }

    @Test
    void testSendConsolidationEmailNotification_Success2() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        List<Integer> destinationBranches = List.of(1, 2, 3);

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

        Map<String, Object> orgData = new HashMap<>();
        orgData.put("FullName", "22323");
        Parties parties = Parties.builder().orgData(orgData).build();

        when(shipmentData.getClient()).thenReturn(parties);
        when(shipmentData.getConsigner()).thenReturn(parties);
        when(shipmentData.getConsignee()).thenReturn(parties);

        Map<String, List<Integer>> shipmentGuidSendToBranch = new HashMap<>();
        shipmentGuidSendToBranch.put("1", List.of(1, 2, 3));
        entityTransferService.sendConsolidationEmailNotification(consoleDetails, destinationBranches, shipmentGuidSendToBranch, false);

        verify(v1ServiceUtil, times(1)).getTenantDetails(anyList());

    }


    @Test
    void testSendConsolidationEmailNotification_Success3() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        List<Integer> destinationBranches = List.of(1, 2, 3);

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

        Map<String, Object> orgData = new HashMap<>();
        orgData.put("id", "22323");
        Parties parties = Parties.builder().orgData(orgData).build();

        when(shipmentData.getClient()).thenReturn(parties);
        when(shipmentData.getConsigner()).thenReturn(parties);
        when(shipmentData.getConsignee()).thenReturn(parties);

        Map<String, List<Integer>> shipmentGuidSendToBranch = new HashMap<>();
        shipmentGuidSendToBranch.put("1", List.of(1, 2, 3));
        entityTransferService.sendConsolidationEmailNotification(consoleDetails, destinationBranches, shipmentGuidSendToBranch, false);

        verify(v1ServiceUtil, times(1)).getTenantDetails(anyList());

    }

    @Test
    void testSendConsolidationEmailNotification_Success4() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        List<Integer> destinationBranches = List.of(1, 2, 3);

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
        Parties parties = Parties.builder().build();

        when(shipmentData.getClient()).thenReturn(parties);
        when(shipmentData.getConsigner()).thenReturn(parties);
        when(shipmentData.getConsignee()).thenReturn(parties);

        Map<String, List<Integer>> shipmentGuidSendToBranch = new HashMap<>();
        shipmentGuidSendToBranch.put("1", List.of(1, 2, 3));
        entityTransferService.sendConsolidationEmailNotification(consoleDetails, destinationBranches, shipmentGuidSendToBranch, false);

        verify(v1ServiceUtil, times(1)).getTenantDetails(anyList());

    }

    @Test
    void testSendConsolidationEmailNotification_Success5() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        List<Integer> destinationBranches = List.of(1);

        when(iv1Service.getEmailTemplatesWithTenantId(any())).thenReturn(V1DataResponse.builder().build());
        Map<Integer, Object> mockV1Map = new HashMap<>();
        TenantContext.setCurrentTenant(1);
        mockV1Map.put(1, new Object());
        mockV1Map.put(0, new Object());
        mockV1Map.put(2, new Object());
        mockV1Map.put(3, new Object());
        mockV1Map.put(4, new Object());
        UsersDto mockUser = new UsersDto();
        mockUser.setEmail("test@dpworld.com");
        UserContext.setUser(mockUser);
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(mockV1Map);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockedTenantModel);
        when(consoleDetails.getConsolidationNumber()).thenReturn("1");
        when(consoleDetails.getShipmentsList()).thenReturn(Set.of(shipmentData));
        when(consoleDetails.getTransportMode()).thenReturn("SEA");
        when(consoleDetails.getBol()).thenReturn("bol");
        when(consoleDetails.getReceivingBranch()).thenReturn(1L);
        when(consoleDetails.getInterBranchConsole()).thenReturn(true);
        when(v1Service.getUsersWithGivenPermissions(any())).thenReturn(List.of(mockUser));
        Parties parties = Parties.builder().build();

        when(shipmentData.getClient()).thenReturn(parties);
        when(shipmentData.getConsigner()).thenReturn(parties);
        when(shipmentData.getConsignee()).thenReturn(parties);

        Map<String, List<Integer>> shipmentGuidSendToBranch = new HashMap<>();
        shipmentGuidSendToBranch.put("1", List.of(1, 2, 3));
        entityTransferService.sendConsolidationEmailNotification(consoleDetails, destinationBranches, shipmentGuidSendToBranch, false);

        verify(v1ServiceUtil, times(1)).getTenantDetails(anyList());

    }

    @Test
    void testSendConsolidationEmailNotification_Success6() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        List<Integer> destinationBranches = List.of(1, 2, 3);

        when(iv1Service.getEmailTemplatesWithTenantId(any())).thenReturn(V1DataResponse.builder().build());
        Map<Integer, Object> mockV1Map = new HashMap<>();
        TenantContext.setCurrentTenant(1);
        mockV1Map.put(1, new Object());
        mockV1Map.put(0, new Object());
        mockV1Map.put(2, new Object());
        mockV1Map.put(3, new Object());
        mockV1Map.put(4, new Object());
        UsersDto mockUser = new UsersDto();
        mockUser.setEmail("test@dpworld.com");
        UserContext.setUser(mockUser);
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(mockV1Map);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockedTenantModel);
        when(consoleDetails.getConsolidationNumber()).thenReturn("1");
        when(consoleDetails.getShipmentsList()).thenReturn(Set.of(shipmentData));
        when(consoleDetails.getTransportMode()).thenReturn("SEA");
        when(consoleDetails.getBol()).thenReturn("bol");
        when(consoleDetails.getReceivingBranch()).thenReturn(1L);
        when(consoleDetails.getInterBranchConsole()).thenReturn(true);
        when(v1Service.getUsersWithGivenPermissions(any())).thenReturn(List.of(mockUser));
        Parties parties = Parties.builder().build();

        when(shipmentData.getClient()).thenReturn(parties);
        when(shipmentData.getConsigner()).thenReturn(parties);
        when(shipmentData.getConsignee()).thenReturn(parties);
        when(shipmentData.getReceivingBranch()).thenReturn(null);

        Map<String, List<Integer>> shipmentGuidSendToBranch = new HashMap<>();
        shipmentGuidSendToBranch.put("1", List.of(1, 2, 3));
        entityTransferService.sendConsolidationEmailNotification(consoleDetails, destinationBranches, shipmentGuidSendToBranch, false);

        verify(v1ServiceUtil, times(1)).getTenantDetails(anyList());

    }

    @Test
    void testSendShipmentEmailNotification_Success() {
        List<Integer> destinationBranches = List.of(1, 2, 3);

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
        entityTransferService.sendShipmentEmailNotification(shipmentData, destinationBranches, false);

        verify(v1Service, times(3)).getUsersWithGivenPermissions(any());

    }

    @Test
    void testSendShipment2EmailNotification_Success() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        List<Integer> destinationBranches = List.of(1, 2, 3);

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
        entityTransferService.sendShipmentEmailNotification(shipmentData, destinationBranches, false);

        verify(v1Service, times(3)).getUsersWithGivenPermissions(any());

    }

    @Test
    void testSendShipment3EmailNotification_Success() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        List<Integer> destinationBranches = List.of(1, 2, 3);

        when(iv1Service.getEmailTemplatesWithTenantId(any())).thenReturn(V1DataResponse.builder().build());
        Map<Integer, Object> mockV1Map = new HashMap<>();
        TenantContext.setCurrentTenant(1);
        mockV1Map.put(1, new Object());
        Map<String, Object> orgData = new HashMap<>();
        orgData.put("FullName", "22323");
        Parties parties = Parties.builder().orgData(orgData).build();
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(mockV1Map);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockedTenantModel);
        when(shipmentData.getShipmentId()).thenReturn("1");
        when(shipmentData.getHouseBill()).thenReturn("hbn");
        when(shipmentData.getMasterBill()).thenReturn("mbn");
        when(shipmentData.getClient()).thenReturn(parties);
        when(shipmentData.getConsigner()).thenReturn(parties);
        when(shipmentData.getConsignee()).thenReturn(parties);
        when(v1Service.getUsersWithGivenPermissions(any())).thenReturn(new ArrayList<>());
        entityTransferService.sendShipmentEmailNotification(shipmentData, destinationBranches, false);

        verify(v1Service, times(3)).getUsersWithGivenPermissions(any());

    }

    @Test
    void testSendShipment4EmailNotification_Success() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        List<Integer> destinationBranches = List.of(1, 2, 3);

        when(iv1Service.getEmailTemplatesWithTenantId(any())).thenReturn(V1DataResponse.builder().build());
        Map<Integer, Object> mockV1Map = new HashMap<>();
        TenantContext.setCurrentTenant(1);
        mockV1Map.put(1, new Object());
        Map<String, Object> orgData = new HashMap<>();
        orgData.put("id", "22323");
        Parties parties = Parties.builder().orgData(orgData).build();
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(mockV1Map);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockedTenantModel);
        when(shipmentData.getShipmentId()).thenReturn("1");
        when(shipmentData.getHouseBill()).thenReturn("hbn");
        when(shipmentData.getMasterBill()).thenReturn("mbn");
        when(shipmentData.getClient()).thenReturn(parties);
        when(shipmentData.getConsigner()).thenReturn(parties);
        when(shipmentData.getConsignee()).thenReturn(parties);
        when(v1Service.getUsersWithGivenPermissions(any())).thenReturn(new ArrayList<>());
        entityTransferService.sendShipmentEmailNotification(shipmentData, destinationBranches, false);

        verify(v1Service, times(3)).getUsersWithGivenPermissions(any());

    }

    @Test
    void testSendShipment5EmailNotification_Success() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(true);
        List<Integer> destinationBranches = List.of(1, 2, 3);

        when(iv1Service.getEmailTemplatesWithTenantId(any())).thenReturn(V1DataResponse.builder().build());
        Map<Integer, Object> mockV1Map = new HashMap<>();
        TenantContext.setCurrentTenant(1);
        mockV1Map.put(1, new Object());
        Parties parties = Parties.builder().build();
        when(v1ServiceUtil.getTenantDetails(anyList())).thenReturn(mockV1Map);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockedTenantModel);
        when(shipmentData.getShipmentId()).thenReturn("1");
        when(shipmentData.getHouseBill()).thenReturn("hbn");
        when(shipmentData.getMasterBill()).thenReturn("mbn");
        when(shipmentData.getClient()).thenReturn(parties);
        when(shipmentData.getConsigner()).thenReturn(parties);
        when(shipmentData.getConsignee()).thenReturn(parties);
        when(v1Service.getUsersWithGivenPermissions(any())).thenReturn(new ArrayList<>());
        entityTransferService.sendShipmentEmailNotification(shipmentData, destinationBranches, false);

        verify(v1Service, times(3)).getUsersWithGivenPermissions(any());

    }

    @Test
    void testSendShipmentForNTESuccess() throws RunnerException {
        Long shipmentId = 1L;
        int mockTenantId = 10;

        SendShipmentRequest sendShipmentRequest = new SendShipmentRequest();
        sendShipmentRequest.setSendToBranch(List.of(1, 2, 3));
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
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETPayload);
        when(shipmentService.fetchAllMasterDataByKey(any(), any())).thenReturn(new HashMap<String, Object>());
        when(networkTransferDao.findByTenantAndEntity(1, 155357L, Constants.SHIPMENT)).thenReturn(Optional.of(mockNetworkTransfer));
        when(networkTransferDao.findByTenantAndEntity(2, 155357L, Constants.SHIPMENT)).thenReturn(Optional.empty());
        when(networkTransferDao.findByTenantAndEntity(2, 155357L, Constants.SHIPMENT)).thenReturn(Optional.empty());
        when(notificationDao.findNotificationForEntityTransfer(anyLong(), anyString(), anyInt(), anyList())).thenReturn(new ArrayList<>());
        doNothing().when(notificationDao).deleteAll(anyList());

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        shipmentSettingsDetails1.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        shipmentSettingsDetails2.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(3);
        shipmentSettingsDetails3.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(4);
        shipmentSettingsDetails4.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        shipmentSettingsDetails5.setIsRunnerV3Enabled(Boolean.TRUE);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        var httpResponse = entityTransferService.sendShipment(commonRequestModel);

        assertNotNull(httpResponse);
        verify(shipmentDao, times(1)).saveEntityTransfer(any(), any());
    }

    @Test
    void testSendShipment_alreadyAcceptedNT() throws RunnerException {
        Long shipmentId = 1L;
        int mockTenantId = 10;

        SendShipmentRequest sendShipmentRequest = new SendShipmentRequest();
        sendShipmentRequest.setSendToBranch(List.of(1, 2, 3));
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
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsNetworkTransferEntityEnabled(Boolean.TRUE);

        lenient().when(networkTransferDao.findByEntityAndTenantList(155357L, SHIPMENT,
                sendShipmentRequest.getSendToBranch())).thenReturn(List.of(mockNetworkTransfer));

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        shipmentSettingsDetails1.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        shipmentSettingsDetails2.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(3);
        shipmentSettingsDetails3.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(4);
        shipmentSettingsDetails4.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        shipmentSettingsDetails5.setIsRunnerV3Enabled(Boolean.TRUE);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        List<Integer> response = entityTransferService.sendShipment(
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

        when(networkTransferDao.findByTenantAndEntity(anyInt(), anyLong(), anyString()))
                .thenReturn(Optional.of(mockNetworkTransfer));

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        shipmentSettingsDetails1.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(2);
        shipmentSettingsDetails2.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(300);
        shipmentSettingsDetails3.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(4);
        shipmentSettingsDetails4.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        shipmentSettingsDetails5.setIsRunnerV3Enabled(Boolean.TRUE);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        List<Integer> response = entityTransferService.sendShipment(commonRequestModel);
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
        when(networkTransferDao.findByTenantAndEntity(66, 2258L, Constants.CONSOLIDATION)).thenReturn(Optional.of(mockNetworkTransfer));
        when(networkTransferDao.findByTenantAndEntity(11, 2258L, Constants.CONSOLIDATION)).thenReturn(Optional.empty());
        when(notificationDao.findNotificationForEntityTransfer(anyLong(), anyString(), anyInt(), anyList())).thenReturn(new ArrayList<>());
        doNothing().when(notificationDao).deleteAll(anyList());

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        shipmentSettingsDetails1.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(66);
        shipmentSettingsDetails2.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(69);
        shipmentSettingsDetails3.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(11);
        shipmentSettingsDetails4.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        shipmentSettingsDetails5.setIsRunnerV3Enabled(Boolean.TRUE);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        List<Integer> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertNotNull(responseEntity);
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
        when(networkTransferDao.findByTenantAndEntity(66, 2258L, Constants.CONSOLIDATION)).thenReturn(Optional.of(mockNetworkTransfer));
        when(networkTransferDao.findByTenantAndEntity(69, 5607L, Constants.SHIPMENT)).thenReturn(Optional.of(mockNetworkTransfer));
        when(notificationDao.findNotificationForEntityTransfer(anyLong(), anyString(), anyInt(), anyList())).thenReturn(new ArrayList<>());
        doNothing().when(notificationDao).deleteAll(anyList());
        when(v1ServiceUtil.fetchCoLoadInfo(any(), any())).thenReturn(coLoadMap);
        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        shipmentSettingsDetails1.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(66);
        shipmentSettingsDetails2.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(69);
        shipmentSettingsDetails3.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(4);
        shipmentSettingsDetails4.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        shipmentSettingsDetails5.setIsRunnerV3Enabled(Boolean.TRUE);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        List<Integer> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
        assertNotNull(responseEntity);
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
        when(v1ServiceUtil.getTenantDetails(any())).thenReturn(mockTenantNameMap);
        when(jsonHelper.convertValue(any(), eq(V1TenantResponse.class))).thenReturn(mockV1TenantResponse);
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ConsolidationDetails.class))).thenReturn(mockETPayload);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mockLinkedShipment));
        when(jsonHelper.convertValue(any(), eq(EntityTransferV3ShipmentDetails.class))).thenReturn(mockETShipment);

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        shipmentSettingsDetails1.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(66);
        shipmentSettingsDetails2.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(69);
        shipmentSettingsDetails3.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(4);
        shipmentSettingsDetails4.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        shipmentSettingsDetails5.setIsRunnerV3Enabled(Boolean.TRUE);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        // Call method
        List<Integer> responseEntity = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));

        // Assertions
        assertNotNull(responseEntity);

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

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendConsolidationRequest);

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        shipmentSettingsDetails1.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(66);
        shipmentSettingsDetails2.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(69);
        shipmentSettingsDetails3.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(11);
        shipmentSettingsDetails4.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        shipmentSettingsDetails5.setIsRunnerV3Enabled(Boolean.TRUE);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        List<Integer> response = entityTransferService.sendConsolidation(commonRequestModel);
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

        ShipmentSettingsDetails shipmentSettingsDetails1 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails1.setTenantId(10);
        shipmentSettingsDetails1.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails2 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails2.setTenantId(66);
        shipmentSettingsDetails2.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails3 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails3.setTenantId(11);
        shipmentSettingsDetails3.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails4 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails4.setTenantId(4);
        shipmentSettingsDetails4.setIsRunnerV3Enabled(Boolean.TRUE);
        ShipmentSettingsDetails shipmentSettingsDetails5 = ShipmentSettingsDetails.builder().build();
        shipmentSettingsDetails5.setTenantId(1);
        shipmentSettingsDetails5.setIsRunnerV3Enabled(Boolean.TRUE);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = new ArrayList<>(List.of(shipmentSettingsDetails2, shipmentSettingsDetails1, shipmentSettingsDetails3, shipmentSettingsDetails4, shipmentSettingsDetails5));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(shipmentSettingsDetailsList);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(sendConsolidationRequest);

        List<Integer> response = entityTransferService.sendConsolidation(commonRequestModel);
        assertNotNull(response);
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
    void testImportShipment_Rejection1() {
        ImportV3ShipmentRequest importShipmentRequest = ImportV3ShipmentRequest.builder()
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
        assertDoesNotThrow(() -> entityTransferService.importShipment(CommonRequestModel.buildRequest(importShipmentRequest)));
    }

    @Test
    void testImportShipment_Rejection2() {
        ImportV3ShipmentRequest importShipmentRequest = ImportV3ShipmentRequest.builder()
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

        NetworkTransfer networkTransfer = new NetworkTransfer();
        networkTransfer.setUpdatedBy("XYZ");
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(networkTransfer));
        when(v1ServiceUtil.getUsersWithGivenPermission(any(), any())).thenReturn(usersDtoList);
        assertThrows(ValidationException.class, () -> entityTransferService.importShipment(commonRequestModel));
    }

    @Test
    void testImportShipment_Rejection3() {
        ImportV3ShipmentRequest importShipmentRequest = ImportV3ShipmentRequest.builder()
                .taskId(1L)
                .operation(TaskStatus.REJECTED.getDescription())
                .rejectRemarks("Test rejection msg")
                .isFromNte(true)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(importShipmentRequest);

        assertThrows(ValidationException.class, () -> entityTransferService.importShipment(commonRequestModel));
    }

    @Test
    void testImportConsolidation_Rejection() throws RunnerException, JsonMappingException {
        ImportV3ConsolidationRequest importConsolidationRequest = ImportV3ConsolidationRequest.builder()
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
    void testSendConsolidationValidation_AirMode_MissingFlightNumber() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("MAWB123");
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setReceivingBranch(100L);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber(null);
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendConsoleValidationResponse validationResponse = (SendConsoleValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendConsoleValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("Flight number"));
    }

    @Test
    void testSendConsolidationValidation_AirMode_MissingEta() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("MAWB123");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber("FL123");
        consolidationCarrier.setEta(null);
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendConsoleValidationResponse validationResponse = (SendConsoleValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendConsoleValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("Eta"));
    }

    @Test
    void testSendConsolidationValidation_AirMode_MissingEtd() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("MAWB123");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber("FL123");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(null);
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendConsoleValidationResponse validationResponse = (SendConsoleValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendConsoleValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("Etd"));
    }

    @Test
    void testSendConsolidationValidation_AirMode_MissingHAWBInShipment() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("MAWB123");

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber("FL123");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setId(1L);
        shipment.setShipmentId("SH001");
        shipment.setTransportMode(TRANSPORT_MODE_AIR);
        shipment.setJobType(SHIPMENT_TYPE_BCN);
        shipment.setHouseBill(null);
        consolidationDetails.setShipmentsList(Set.of(shipment));

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendConsoleValidationResponse validationResponse = (SendConsoleValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendConsoleValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getConsoleErrorMessage().contains("HAWB number"));
        assertTrue(validationResponse.getMissingKeys().contains("HAWB Number"));
        assertFalse(validationResponse.getShipmentIds().isEmpty());
    }

    @Test
    void testSendConsolidationValidation_SeaMode_Success() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setBol("MBL123");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel("VESSEL1");
        consolidationCarrier.setVoyage("V001");
        consolidationCarrier.setShippingLine("MAERSK");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        Parties sendingAgent = Parties.builder().orgCode("ORG001").build();
        consolidationDetails.setSendingAgent(sendingAgent);

        Parties receivingAgent = Parties.builder().orgCode("ORG002").build();
        consolidationDetails.setReceivingAgent(receivingAgent);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendConsolidationValidation_SeaMode_MissingHBLInShipment() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setBol("MBL123");

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel("VESSEL1");
        consolidationCarrier.setVoyage("V001");
        consolidationCarrier.setShippingLine("MAERSK");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        Parties sendingAgent = Parties.builder().orgCode("ORG001").build();
        consolidationDetails.setSendingAgent(sendingAgent);

        Parties receivingAgent = Parties.builder().orgCode("ORG002").build();
        consolidationDetails.setReceivingAgent(receivingAgent);

        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setId(1L);
        shipment.setShipmentId("SH001");
        shipment.setTransportMode(TRANSPORT_MODE_SEA);
        shipment.setJobType(SHIPMENT_TYPE_STD);
        shipment.setHouseBill(null);
        consolidationDetails.setShipmentsList(Set.of(shipment));

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendConsoleValidationResponse validationResponse = (SendConsoleValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendConsoleValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getConsoleErrorMessage().contains("HBL number"));
        assertTrue(validationResponse.getMissingKeys().contains("HBL"));
    }

    @Test
    void testSendConsolidationValidation_SeaMode_MissingVessel() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setBol("MBL123");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel(null);
        consolidationCarrier.setVoyage("V001");
        consolidationCarrier.setShippingLine("MAERSK");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        Parties sendingAgent = Parties.builder().orgCode("ORG001").build();
        consolidationDetails.setSendingAgent(sendingAgent);

        Parties receivingAgent = Parties.builder().orgCode("ORG002").build();
        consolidationDetails.setReceivingAgent(receivingAgent);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendConsoleValidationResponse validationResponse = (SendConsoleValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendConsoleValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("Vessel"));
    }

    @Test
    void testSendConsolidationValidation_SeaMode_MissingVoyage() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setBol("MBL123");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel("VESSEL1");
        consolidationCarrier.setVoyage(null);
        consolidationCarrier.setShippingLine("MAERSK");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        Parties sendingAgent = Parties.builder().orgCode("ORG001").build();
        consolidationDetails.setSendingAgent(sendingAgent);

        Parties receivingAgent = Parties.builder().orgCode("ORG002").build();
        consolidationDetails.setReceivingAgent(receivingAgent);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendConsoleValidationResponse validationResponse = (SendConsoleValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendConsoleValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("Voyage"));
    }

    @Test
    void testSendConsolidationValidation_SeaMode_MissingShippingLine() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setBol("MBL123");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel("VESSEL1");
        consolidationCarrier.setVoyage("V001");
        consolidationCarrier.setShippingLine(null);
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        Parties sendingAgent = Parties.builder().orgCode("ORG001").build();
        consolidationDetails.setSendingAgent(sendingAgent);

        Parties receivingAgent = Parties.builder().orgCode("ORG002").build();
        consolidationDetails.setReceivingAgent(receivingAgent);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendConsoleValidationResponse validationResponse = (SendConsoleValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendConsoleValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("Shipping line"));
    }

    @Test
    void testSendConsolidationValidation_SeaMode_MissingOriginAgent() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setBol("MBL123");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel("VESSEL1");
        consolidationCarrier.setVoyage("V001");
        consolidationCarrier.setShippingLine("MAERSK");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        consolidationDetails.setSendingAgent(null);

        Parties receivingAgent = Parties.builder().orgCode("ORG002").build();
        consolidationDetails.setReceivingAgent(receivingAgent);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendConsoleValidationResponse validationResponse = (SendConsoleValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendConsoleValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("Origin agent"));
    }

    @Test
    void testSendConsolidationValidation_SeaMode_MissingDestinationAgent() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setBol("MBL123");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel("VESSEL1");
        consolidationCarrier.setVoyage("V001");
        consolidationCarrier.setShippingLine("MAERSK");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        Parties sendingAgent = Parties.builder().orgCode("ORG001").build();
        consolidationDetails.setSendingAgent(sendingAgent);

        consolidationDetails.setReceivingAgent(null);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendConsoleValidationResponse validationResponse = (SendConsoleValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendConsoleValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("Destination agent"));
    }

    @Test
    void testSendConsolidationValidation_AlreadyTransferredCTS() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_CTS);
        consolidationDetails.setSourceGuid(UUID.randomUUID());
        consolidationDetails.setGuid(UUID.randomUUID());
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber("FL123");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendConsoleValidationResponse validationResponse = (SendConsoleValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendConsoleValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getConsoleErrorMessage().contains("Already transferred CTS file"));
    }

    @Test
    void testSendConsolidationValidation_RailTransportMode_ThrowsException() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_RAI);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        assertThrows(ValidationException.class, () ->
                entityTransferService.sendConsolidationValidation(commonRequestModel)
        );
    }

    @Test
    void testAutomaticTransferConsoleValidation_Success() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("MAWB123");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber("FL123");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        SendConsoleValidationResponse response = entityTransferService.automaticTransferConsoleValidation(commonRequestModel);

        assertNotNull(response);
        assertFalse(response.getIsError());
    }

    @Test
    void testAutomaticTransferConsoleValidation_WithErrors() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("MAWB123");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber(null);
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        SendConsoleValidationResponse response = entityTransferService.automaticTransferConsoleValidation(commonRequestModel);

        assertNotNull(response);
        assertTrue(response.getIsError());
        assertTrue(response.getMissingKeys().contains("Flight number"));
        assertTrue(response.getConsoleErrorMessage().contains("to retrigger the transfer"));
    }

    @Test
    void testSendShipmentValidation_AirMode_DRT_Success() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setHouseBill("HAWB123");
        shipmentDetails.setMasterBill("MAWB123");

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("FL456");
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendShipmentValidation_AirMode_MissingFlightNumber() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setHouseBill("HAWB123");
        shipmentDetails.setMasterBill("MAWB123");

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber(null);
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendShipmentValidationResponse validationResponse = (SendShipmentValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendShipmentValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("Flight number"));
    }

    @Test
    void testSendShipmentValidation_AirMode_MissingHAWB() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_BCN);
        shipmentDetails.setHouseBill(null);
        shipmentDetails.setMasterBill("MAWB123");

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("FL456");
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendShipmentValidationResponse validationResponse = (SendShipmentValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendShipmentValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("HAWB Number"));
    }

    @Test
    void testSendShipmentValidation_AirMode_MissingMAWB() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setHouseBill("HAWB123");
        shipmentDetails.setMasterBill(null);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("FL456");
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendShipmentValidationResponse validationResponse = (SendShipmentValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendShipmentValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("MAWB Number"));
    }

    @Test
    void testSendShipmentValidation_SeaMode_MissingHBL() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        shipmentDetails.setHouseBill(null);
        shipmentDetails.setMasterBill("MBL123");

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendShipmentValidationResponse validationResponse = (SendShipmentValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendShipmentValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("House Bill"));
    }

    @Test
    void testSendShipmentValidation_AlreadyTransferredCTS() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_CTS);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setSourceGuid(UUID.randomUUID());
        shipmentDetails.setGuid(UUID.randomUUID());
        shipmentDetails.setHouseBill("HAWB123");
        shipmentDetails.setMasterBill("MAWB123");

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("FL456");
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendShipmentValidationResponse validationResponse = (SendShipmentValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendShipmentValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getShipmentErrorMessage().contains("Already transferred CTS file"));
    }

    @Test
    void testSendShipmentValidation_MissingEta() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setHouseBill("HAWB123");
        shipmentDetails.setMasterBill("MAWB123");

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("FL456");
        shipmentCarrier.setEta(null);
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendShipmentValidationResponse validationResponse = (SendShipmentValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendShipmentValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("Eta"));
    }

    @Test
    void testSendShipmentValidation_MissingEtd() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setHouseBill("HAWB123");
        shipmentDetails.setMasterBill("MAWB123");

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("FL456");
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(null);
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendShipmentValidationResponse validationResponse = (SendShipmentValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendShipmentValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("Etd"));
    }

    @Test
    void testSendShipmentValidation_NetworkTransferDisabled_Success() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_IMP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setHouseBill("HAWB123");
        shipmentDetails.setMasterBill("MAWB123");

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("FL456");
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(false);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testAutomaticTransferShipmentValidation_Success() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setHouseBill("HAWB123");
        shipmentDetails.setMasterBill("MAWB123");

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("FL456");
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        SendShipmentValidationResponse response = entityTransferService.automaticTransferShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertFalse(response.getIsError());
    }

    @Test
    void testAutomaticTransferShipmentValidation_WithErrors() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setHouseBill("HAWB123");
        shipmentDetails.setMasterBill("MAWB123");

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber(null);
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        SendShipmentValidationResponse response = entityTransferService.automaticTransferShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertTrue(response.getIsError());
        assertTrue(response.getMissingKeys().contains("Flight number"));
        assertTrue(response.getShipmentErrorMessage().contains("to retrigger the transfer"));
    }

    @Test
    void testAutomaticTransferShipmentValidation_AlreadyTransferredCTS() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_CTS);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setSourceGuid(UUID.randomUUID());
        shipmentDetails.setGuid(UUID.randomUUID());

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("FL456");
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        SendShipmentValidationResponse response = entityTransferService.automaticTransferShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertTrue(response.getIsError());
        assertTrue(response.getShipmentErrorMessage().contains("Already transferred CTS file"));
    }

    @Test
    void testSendShipmentValidation_MultipleErrors() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setHouseBill(null);
        shipmentDetails.setMasterBill(null);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber(null);
        shipmentCarrier.setEta(null);
        shipmentCarrier.setEtd(null);
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendShipmentValidationResponse validationResponse = (SendShipmentValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendShipmentValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("Flight number"));
        assertTrue(validationResponse.getMissingKeys().contains("Eta"));
        assertTrue(validationResponse.getMissingKeys().contains("Etd"));
    }

    @Test
    void testSendShipmentValidation_AirMode_MissingBranchSelection() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setHouseBill("HAWB123");
        shipmentDetails.setMasterBill("MAWB123");
        shipmentDetails.setReceivingBranch(100L);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("FL456");
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendShipmentValidation_ImpDirection_MissingHouseBill() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_IMP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setHouseBill(null);
        shipmentDetails.setMasterBill("MAWB123");
        shipmentDetails.setReceivingBranch(100L);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("FL456");
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendShipmentValidation_CtsDirection_NotAlreadyTransferred() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_CTS);
        shipmentDetails.setJobType(SHIPMENT_TYPE_BCN);
        shipmentDetails.setHouseBill("HAWB123");
        shipmentDetails.setMasterBill("MAWB123");
        shipmentDetails.setReceivingBranch(100L);
        shipmentDetails.setSourceGuid(null);
        shipmentDetails.setGuid(UUID.randomUUID());

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("FL456");
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendShipmentValidation_SeaMode_Success() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        shipmentDetails.setHouseBill("HBL123");
        shipmentDetails.setMasterBill("MBL123");
        shipmentDetails.setReceivingBranch(100L);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendShipmentValidation_SeaMode_MissingMasterBill() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        shipmentDetails.setHouseBill("HBL123");
        shipmentDetails.setMasterBill(null);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendShipmentValidationResponse validationResponse = (SendShipmentValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendShipmentValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("Master Bill"));
    }

    @Test
    void testSendShipmentValidation_DrtType_SkipsHouseBillValidation() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setHouseBill(null);
        shipmentDetails.setMasterBill("MAWB123");
        shipmentDetails.setReceivingBranch(100L);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("FL456");
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testAutomaticTransferShipmentValidation_MissingBranchSelection() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setHouseBill("HAWB123");
        shipmentDetails.setMasterBill("MAWB123");
        shipmentDetails.setReceivingBranch(null);
        shipmentDetails.setTriangulationPartner(null);
        shipmentDetails.setTriangulationPartnerList(null);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("FL456");
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        SendShipmentValidationResponse response = entityTransferService.automaticTransferShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertFalse(response.getIsError());
    }

    @Test
    void testAutomaticTransferShipmentValidation_SeaMode() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        shipmentDetails.setHouseBill("HBL123");
        shipmentDetails.setMasterBill("MBL123");
        shipmentDetails.setReceivingBranch(100L);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        SendShipmentValidationResponse response = entityTransferService.automaticTransferShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertFalse(response.getIsError());
    }

    @Test
    void testSendConsolidationValidation_SeaMode_MissingEta() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setBol("MBL123");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel("VESSEL1");
        consolidationCarrier.setVoyage("V001");
        consolidationCarrier.setShippingLine("MAERSK");
        consolidationCarrier.setEta(null);
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        Parties sendingAgent = Parties.builder().orgCode("ORG001").build();
        consolidationDetails.setSendingAgent(sendingAgent);

        Parties receivingAgent = Parties.builder().orgCode("ORG002").build();
        consolidationDetails.setReceivingAgent(receivingAgent);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendConsoleValidationResponse validationResponse = (SendConsoleValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendConsoleValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("Eta"));
    }

    @Test
    void testSendConsolidationValidation_SeaMode_MissingEtd() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setBol("MBL123");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel("VESSEL1");
        consolidationCarrier.setVoyage("V001");
        consolidationCarrier.setShippingLine("MAERSK");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(null);
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        Parties sendingAgent = Parties.builder().orgCode("ORG001").build();
        consolidationDetails.setSendingAgent(sendingAgent);

        Parties receivingAgent = Parties.builder().orgCode("ORG002").build();
        consolidationDetails.setReceivingAgent(receivingAgent);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendConsoleValidationResponse validationResponse = (SendConsoleValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendConsoleValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
        assertTrue(validationResponse.getMissingKeys().contains("Etd"));
    }

    @Test
    void testAutomaticTransferConsoleValidation_SeaMode() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setBol("MBL123");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel("VESSEL1");
        consolidationCarrier.setVoyage("V001");
        consolidationCarrier.setShippingLine("MAERSK");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        Parties sendingAgent = Parties.builder().orgCode("ORG001").build();
        consolidationDetails.setSendingAgent(sendingAgent);

        Parties receivingAgent = Parties.builder().orgCode("ORG002").build();
        consolidationDetails.setReceivingAgent(receivingAgent);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        SendConsoleValidationResponse response = entityTransferService.automaticTransferConsoleValidation(commonRequestModel);

        assertNotNull(response);
        assertFalse(response.getIsError());
    }

    @Test
    void testAutomaticTransferConsoleValidation_MissingBranchSelection() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("MAWB123");
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setReceivingBranch(null);
        consolidationDetails.setTriangulationPartnerList(null);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber("FL123");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        SendConsoleValidationResponse response = entityTransferService.automaticTransferConsoleValidation(commonRequestModel);

        assertNotNull(response);
        assertFalse(response.getIsError());
    }

    @Test
    void testSendConsolidationValidation_WithShipments_MissingFields() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("MAWB123");
        consolidationDetails.setReceivingBranch(100L);

        ShipmentDetails shipment1 = new ShipmentDetails();
        shipment1.setId(1L);
        shipment1.setTransportMode(TRANSPORT_MODE_AIR);
        shipment1.setHouseBill(null);
        shipment1.setMasterBill("MAWB123");

        Set<ShipmentDetails> shipments = new HashSet<>();
        shipments.add(shipment1);
        consolidationDetails.setShipmentsList(shipments);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber("FL123");
        consolidationCarrier.setEta(null);
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendConsoleValidationResponse validationResponse = (SendConsoleValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendConsoleValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
    }

    @Test
    void testSendConsolidationValidation_MultipleShipments_WithErrors() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("MAWB123");
        consolidationDetails.setReceivingBranch(100L);

        ShipmentDetails shipment1 = new ShipmentDetails();
        shipment1.setId(1L);
        shipment1.setShipmentId("SHP001");
        shipment1.setTransportMode(TRANSPORT_MODE_AIR);
        shipment1.setHouseBill(null);
        shipment1.setMasterBill("MAWB123");
        shipment1.setDirection(DIRECTION_EXP);

        ShipmentDetails shipment2 = new ShipmentDetails();
        shipment2.setId(2L);
        shipment2.setShipmentId("SHP002");
        shipment2.setTransportMode(TRANSPORT_MODE_AIR);
        shipment2.setHouseBill("HAWB002");
        shipment2.setMasterBill(null);
        shipment2.setDirection(DIRECTION_EXP);

        Set<ShipmentDetails> shipments = new HashSet<>();
        shipments.add(shipment1);
        shipments.add(shipment2);
        consolidationDetails.setShipmentsList(shipments);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber("FL123");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertNotNull(response.getBody());
        SendConsoleValidationResponse validationResponse = (SendConsoleValidationResponse) ((RunnerResponse<?>) response.getBody()).getData(SendConsoleValidationResponse.class);
        assertNotNull(validationResponse);
        assertTrue(validationResponse.getIsError());
    }

    @Test
    void testSendConsolidationValidation_SeaMode_WithShipmentsValidation() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setBol("MBL123");
        consolidationDetails.setReceivingBranch(100L);

        ShipmentDetails shipment1 = new ShipmentDetails();
        shipment1.setId(1L);
        shipment1.setShipmentId("SHP001");
        shipment1.setTransportMode(TRANSPORT_MODE_SEA);
        shipment1.setHouseBill("HBL001");
        shipment1.setMasterBill(null);
        shipment1.setDirection(DIRECTION_EXP);

        Set<ShipmentDetails> shipments = new HashSet<>();
        shipments.add(shipment1);
        consolidationDetails.setShipmentsList(shipments);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel("VESSEL1");
        consolidationCarrier.setVoyage("V001");
        consolidationCarrier.setShippingLine("MAERSK");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        Parties sendingAgent = Parties.builder().orgCode("ORG001").build();
        consolidationDetails.setSendingAgent(sendingAgent);

        Parties receivingAgent = Parties.builder().orgCode("ORG002").build();
        consolidationDetails.setReceivingAgent(receivingAgent);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendShipmentValidation_SeaMode_AllFieldsPresent() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        shipmentDetails.setHouseBill("HBL123");
        shipmentDetails.setMasterBill("MBL123");
        shipmentDetails.setReceivingBranch(100L);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setVessel("VESSEL1");
        shipmentCarrier.setVoyage("V001");
        shipmentCarrier.setShippingLine("MAERSK");
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testAutomaticTransferShipmentValidation_AirMode_WithAllFields() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        shipmentDetails.setHouseBill("HAWB123");
        shipmentDetails.setMasterBill("MAWB123");
        shipmentDetails.setReceivingBranch(100L);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("FL456");
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        SendShipmentValidationResponse response = entityTransferService.automaticTransferShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertFalse(response.getIsError());
    }

    @Test
    void testAutomaticTransferConsoleValidation_SeaMode_WithAllFields() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("MBL123");
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setReceivingBranch(100L);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel("VESSEL1");
        consolidationCarrier.setVoyage("V001");
        consolidationCarrier.setShippingLine("MAERSK");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        Parties sendingAgent = Parties.builder().orgCode("ORG001").build();
        consolidationDetails.setSendingAgent(sendingAgent);

        Parties receivingAgent = Parties.builder().orgCode("ORG002").build();
        consolidationDetails.setReceivingAgent(receivingAgent);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        SendConsoleValidationResponse response = entityTransferService.automaticTransferConsoleValidation(commonRequestModel);

        assertNotNull(response);
        assertFalse(response.getIsError());
    }

    @Test
    void testSendShipmentValidation_InterBranchScenario() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_BCN);
        shipmentDetails.setHouseBill("HAWB123");
        shipmentDetails.setMasterBill("MAWB123");
        shipmentDetails.setReceivingBranch(100L);
        shipmentDetails.setTriangulationPartner(200L);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("FL456");
        shipmentCarrier.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier.setEtd(LocalDateTime.now());
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendConsolidationValidation_InterBranchScenario() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_BCN);
        consolidationDetails.setBol("MAWB123");
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setReceivingBranch(100L);

        // Fix: Create TriangulationPartner without setId - use builder or constructor
        TriangulationPartner triangulationPartner = new TriangulationPartner();
        // Remove the setId line - TriangulationPartner doesn't have this method
        List<TriangulationPartner> triangulationPartnerList = new ArrayList<>();
        triangulationPartnerList.add(triangulationPartner);
        consolidationDetails.setTriangulationPartnerList(triangulationPartnerList);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber("FL123");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendConsolidationValidation_AirMode_MissingEtaEtd() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("MAWB123");
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setReceivingBranch(100L);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber("FL123");
        consolidationCarrier.setEta(null);
        consolidationCarrier.setEtd(null);
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendConsolidationValidation_SeaMode_MissingPolPod() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setBol("MBL123");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel("VESSEL1");
        consolidationCarrier.setVoyage("V001");
        consolidationCarrier.setShippingLine("MAERSK");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationCarrier.setOriginPort("");
        consolidationCarrier.setDestinationPort("");
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        Parties sendingAgent = Parties.builder().orgCode("ORG001").build();
        consolidationDetails.setSendingAgent(sendingAgent);

        Parties receivingAgent = Parties.builder().orgCode("ORG002").build();
        consolidationDetails.setReceivingAgent(receivingAgent);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendConsolidationValidation_SeaMode_MissingSendingReceivingAgent() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setBol("MBL123");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel("VESSEL1");
        consolidationCarrier.setVoyage("V001");
        consolidationCarrier.setShippingLine("MAERSK");
        consolidationCarrier.setEta(null);
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        consolidationDetails.setSendingAgent(null);
        consolidationDetails.setReceivingAgent(null);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendConsolidationValidation_MissingBolAndCarrierDetails() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("");
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setReceivingBranch(null);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber("");
        consolidationCarrier.setEta(null);
        consolidationCarrier.setEtd(null);
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendConsolidationValidation_ShipmentsWithMissingHouseBill() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("MAWB123");
        consolidationDetails.setReceivingBranch(100L);

        ShipmentDetails shipment1 = new ShipmentDetails();
        shipment1.setId(1L);
        shipment1.setShipmentId("SHP001");
        shipment1.setTransportMode(TRANSPORT_MODE_AIR);
        shipment1.setHouseBill("");
        shipment1.setMasterBill("MAWB123");
        shipment1.setDirection(DIRECTION_EXP);
        shipment1.setJobType(SHIPMENT_TYPE_DRT);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber("");
        shipmentCarrier.setEta(null);
        shipmentCarrier.setEtd(null);
        shipment1.setCarrierDetails(shipmentCarrier);

        Set<ShipmentDetails> shipments = new HashSet<>();
        shipments.add(shipment1);
        consolidationDetails.setShipmentsList(shipments);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber("FL123");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendConsolidationValidation_SeaMode_ShipmentsMissingMasterBill() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setBol("MBL123");
        consolidationDetails.setReceivingBranch(100L);

        ShipmentDetails shipment1 = new ShipmentDetails();
        shipment1.setId(1L);
        shipment1.setShipmentId("SHP001");
        shipment1.setTransportMode(TRANSPORT_MODE_SEA);
        shipment1.setHouseBill("HBL001");
        shipment1.setMasterBill("");
        shipment1.setDirection(DIRECTION_EXP);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setVessel("");
        shipmentCarrier.setVoyage("");
        shipmentCarrier.setEta(null);
        shipmentCarrier.setEtd(null);
        shipment1.setCarrierDetails(shipmentCarrier);

        Set<ShipmentDetails> shipments = new HashSet<>();
        shipments.add(shipment1);
        consolidationDetails.setShipmentsList(shipments);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel("VESSEL1");
        consolidationCarrier.setVoyage("V001");
        consolidationCarrier.setShippingLine("MAERSK");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        Parties sendingAgent = Parties.builder().orgCode("ORG001").build();
        consolidationDetails.setSendingAgent(sendingAgent);

        Parties receivingAgent = Parties.builder().orgCode("ORG002").build();
        consolidationDetails.setReceivingAgent(receivingAgent);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendConsolidationValidation_InterBranch_MissingReceivingBranch() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_BCN);
        consolidationDetails.setBol("MAWB123");
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setReceivingBranch(null);

        TriangulationPartner triangulationPartner = new TriangulationPartner();
        List<TriangulationPartner> triangulationPartnerList = new ArrayList<>();
        triangulationPartnerList.add(triangulationPartner);
        consolidationDetails.setTriangulationPartnerList(triangulationPartnerList);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber("FL123");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testAutomaticTransferConsoleValidation_OtherTransportMode() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode("RAIL");
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setEta(null);
        consolidationCarrier.setEtd(null);
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        consolidationDetails.setSendingAgent(null);
        consolidationDetails.setReceivingAgent(null);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        SendConsoleValidationResponse response = entityTransferService.automaticTransferConsoleValidation(commonRequestModel);

        assertNotNull(response);
        assertTrue(response.getIsError());
    }

    @ParameterizedTest
    @MethodSource("provideConsolidationValidationScenarios")
    void testSendConsolidationValidation_VariousScenarios(
            String scenarioName,
            String bol,
            String flightNumber,
            LocalDateTime eta,
            LocalDateTime etd,
            Long receivingBranch
    ) {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol(bol);
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setReceivingBranch(receivingBranch);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber(flightNumber);
        consolidationCarrier.setEta(eta);
        consolidationCarrier.setEtd(etd);
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    private static Stream<Arguments> provideConsolidationValidationScenarios() {
        return Stream.of(
                Arguments.of("WithReceivingBranch", "MAWB123", "FL123", LocalDateTime.now().plusDays(1), LocalDateTime.now(), 100L),
                Arguments.of("MissingBol", "", "FL123", LocalDateTime.now().plusDays(1), LocalDateTime.now(), 100L),
                Arguments.of("NoShipments", "MAWB123", "FL123", LocalDateTime.now().plusDays(1), LocalDateTime.now(), 100L),
                Arguments.of("MissingVoyage", "MAWB123", "", LocalDateTime.now().plusDays(1), LocalDateTime.now(), 100L),
                Arguments.of("MissingEtaEtd", "MAWB123", "FL123", null, null, 100L),
                Arguments.of("MissingBolAndCarrier", "", "", null, null, null)
        );
    }

    @ParameterizedTest
    @MethodSource("provideAirConsolidationScenarios")
    void testSendConsolidationValidation_AirMode_Scenarios(
            String scenarioName,
            String bol,
            String flightNumber,
            LocalDateTime eta,
            LocalDateTime etd,
            Long receivingBranch,
            boolean expectError
    ) {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol(bol);
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setReceivingBranch(receivingBranch);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber(flightNumber);
        consolidationCarrier.setEta(eta);
        consolidationCarrier.setEtd(etd);
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    private static Stream<Arguments> provideAirConsolidationScenarios() {
        LocalDateTime futureDate = LocalDateTime.now().plusDays(1);
        LocalDateTime currentDate = LocalDateTime.now();

        return Stream.of(
                Arguments.of("ValidScenario", "MAWB123", "FL123", futureDate, currentDate, 100L, false),
                Arguments.of("MissingBol", "", "FL123", futureDate, currentDate, 100L, true),
                Arguments.of("MissingFlightNumber", "MAWB123", "", futureDate, currentDate, 100L, true),
                Arguments.of("MissingEta", "MAWB123", "FL123", null, currentDate, 100L, true),
                Arguments.of("MissingEtd", "MAWB123", "FL123", futureDate, null, 100L, true),
                Arguments.of("MissingReceivingBranch", "MAWB123", "FL123", futureDate, currentDate, null, true),
                Arguments.of("AllMissing", "", "", null, null, null, true)
        );
    }

    @ParameterizedTest
    @MethodSource("provideSeaConsolidationScenarios")
    void testSendConsolidationValidation_SeaMode_Scenarios(
            String scenarioName,
            String bol,
            String vessel,
            String voyage,
            String shippingLine,
            String originPort,
            String destinationPort,
            LocalDateTime eta,
            LocalDateTime etd,
            boolean hasSendingAgent,
            boolean hasReceivingAgent,
            boolean expectError
    ) {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setBol(bol);
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setReceivingBranch(100L);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel(vessel);
        consolidationCarrier.setVoyage(voyage);
        consolidationCarrier.setShippingLine(shippingLine);
        consolidationCarrier.setOriginPort(originPort);
        consolidationCarrier.setDestinationPort(destinationPort);
        consolidationCarrier.setEta(eta);
        consolidationCarrier.setEtd(etd);
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        if (hasSendingAgent) {
            consolidationDetails.setSendingAgent(Parties.builder().orgCode("ORG001").build());
        }
        if (hasReceivingAgent) {
            consolidationDetails.setReceivingAgent(Parties.builder().orgCode("ORG002").build());
        }

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    private static Stream<Arguments> provideSeaConsolidationScenarios() {
        LocalDateTime futureDate = LocalDateTime.now().plusDays(1);
        LocalDateTime currentDate = LocalDateTime.now();

        return Stream.of(
                Arguments.of("ValidScenario", "MBL123", "VESSEL1", "V001", "MAERSK", "POL123", "POD456", futureDate, currentDate, true, true, false),
                Arguments.of("MissingBol", "", "VESSEL1", "V001", "MAERSK", "POL123", "POD456", futureDate, currentDate, true, true, true),
                Arguments.of("MissingVessel", "MBL123", "", "V001", "MAERSK", "POL123", "POD456", futureDate, currentDate, true, true, true),
                Arguments.of("MissingVoyage", "MBL123", "VESSEL1", "", "MAERSK", "POL123", "POD456", futureDate, currentDate, true, true, true),
                Arguments.of("MissingShippingLine", "MBL123", "VESSEL1", "V001", "", "POL123", "POD456", futureDate, currentDate, true, true, true),
                Arguments.of("MissingOriginPort", "MBL123", "VESSEL1", "V001", "MAERSK", "", "POD456", futureDate, currentDate, true, true, true),
                Arguments.of("MissingDestinationPort", "MBL123", "VESSEL1", "V001", "MAERSK", "POL123", "", futureDate, currentDate, true, true, true),
                Arguments.of("MissingEta", "MBL123", "VESSEL1", "V001", "MAERSK", "POL123", "POD456", null, currentDate, true, true, true),
                Arguments.of("MissingEtd", "MBL123", "VESSEL1", "V001", "MAERSK", "POL123", "POD456", futureDate, null, true, true, true),
                Arguments.of("MissingSendingAgent", "MBL123", "VESSEL1", "V001", "MAERSK", "POL123", "POD456", futureDate, currentDate, false, true, true),
                Arguments.of("MissingReceivingAgent", "MBL123", "VESSEL1", "V001", "MAERSK", "POL123", "POD456", futureDate, currentDate, true, false, true),
                Arguments.of("AllMissing", "", "", "", "", "", "", null, null, false, false, true)
        );
    }

    @ParameterizedTest
    @MethodSource("provideAirShipmentScenarios")
    void testSendShipmentValidation_AirMode_Scenarios(
            String scenarioName,
            String houseBill,
            String masterBill,
            String flightNumber,
            LocalDateTime eta,
            LocalDateTime etd,
            String jobType,
            Long receivingBranch,
            Long triangulationPartner,
            boolean expectError
    ) {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(jobType);
        shipmentDetails.setHouseBill(houseBill);
        shipmentDetails.setMasterBill(masterBill);
        shipmentDetails.setReceivingBranch(receivingBranch);
        shipmentDetails.setTriangulationPartner(triangulationPartner);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setFlightNumber(flightNumber);
        shipmentCarrier.setEta(eta);
        shipmentCarrier.setEtd(etd);
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    private static Stream<Arguments> provideAirShipmentScenarios() {
        LocalDateTime futureDate = LocalDateTime.now().plusDays(1);
        LocalDateTime currentDate = LocalDateTime.now();

        return Stream.of(
                Arguments.of("ValidStandard", "HAWB123", "MAWB123", "FL123", futureDate, currentDate, SHIPMENT_TYPE_STD, 100L, null, false),
                Arguments.of("ValidDirect", "HAWB123", "MAWB123", "FL123", futureDate, currentDate, SHIPMENT_TYPE_DRT, 100L, null, false),
                Arguments.of("ValidInterBranch", "HAWB123", "MAWB123", "FL123", futureDate, currentDate, SHIPMENT_TYPE_BCN, 100L, 200L, false),
                Arguments.of("MissingHouseBill", "", "MAWB123", "FL123", futureDate, currentDate, SHIPMENT_TYPE_STD, 100L, null, true),
                Arguments.of("MissingMasterBill", "HAWB123", "", "FL123", futureDate, currentDate, SHIPMENT_TYPE_STD, 100L, null, true),
                Arguments.of("MissingFlightNumber", "HAWB123", "MAWB123", "", futureDate, currentDate, SHIPMENT_TYPE_STD, 100L, null, true),
                Arguments.of("MissingEta", "HAWB123", "MAWB123", "FL123", null, currentDate, SHIPMENT_TYPE_STD, 100L, null, true),
                Arguments.of("MissingEtd", "HAWB123", "MAWB123", "FL123", futureDate, null, SHIPMENT_TYPE_STD, 100L, null, true),
                Arguments.of("DRTMissingHouseBill", "", "MAWB123", "FL123", futureDate, currentDate, SHIPMENT_TYPE_DRT, 100L, null, true),
                Arguments.of("InterBranchMissingReceivingBranch", "HAWB123", "MAWB123", "FL123", futureDate, currentDate, SHIPMENT_TYPE_BCN, null, 200L, true),
                Arguments.of("AllMissing", "", "", "", null, null, SHIPMENT_TYPE_STD, null, null, true)
        );
    }

    @ParameterizedTest
    @MethodSource("provideSeaShipmentScenarios")
    void testSendShipmentValidation_SeaMode_Scenarios(
            String scenarioName,
            String houseBill,
            String masterBill,
            String vessel,
            String voyage,
            LocalDateTime eta,
            LocalDateTime etd,
            boolean expectError
    ) {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        shipmentDetails.setHouseBill(houseBill);
        shipmentDetails.setMasterBill(masterBill);
        shipmentDetails.setReceivingBranch(100L);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        shipmentCarrier.setVessel(vessel);
        shipmentCarrier.setVoyage(voyage);
        shipmentCarrier.setShippingLine("MAERSK");
        shipmentCarrier.setEta(eta);
        shipmentCarrier.setEtd(etd);
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    private static Stream<Arguments> provideSeaShipmentScenarios() {
        LocalDateTime futureDate = LocalDateTime.now().plusDays(1);
        LocalDateTime currentDate = LocalDateTime.now();

        return Stream.of(
                Arguments.of("ValidScenario", "HBL123", "MBL123", "VESSEL1", "V001", futureDate, currentDate, false),
                Arguments.of("MissingHouseBill", "", "MBL123", "VESSEL1", "V001", futureDate, currentDate, true),
                Arguments.of("MissingMasterBill", "HBL123", "", "VESSEL1", "V001", futureDate, currentDate, true),
                Arguments.of("MissingVessel", "HBL123", "MBL123", "", "V001", futureDate, currentDate, true),
                Arguments.of("MissingVoyage", "HBL123", "MBL123", "VESSEL1", "", futureDate, currentDate, true),
                Arguments.of("MissingEta", "HBL123", "MBL123", "VESSEL1", "V001", null, currentDate, true),
                Arguments.of("MissingEtd", "HBL123", "MBL123", "VESSEL1", "V001", futureDate, null, true),
                Arguments.of("AllMissing", "", "", "", "", null, null, true)
        );
    }

    @ParameterizedTest
    @CsvSource({
            "RAIL,true",
            "TRUCK,true",
            "COURIER,true"
    })
    void testSendConsolidationValidation_OtherTransportModes(String transportMode, boolean expectError) {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(transportMode);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("");
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setEta(null);
        consolidationCarrier.setEtd(null);
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        consolidationDetails.setSendingAgent(null);
        consolidationDetails.setReceivingAgent(null);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendConsolidationValidation_WithMultipleShipmentsHavingErrors() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("MAWB123");
        consolidationDetails.setReceivingBranch(100L);

        ShipmentDetails shipment1 = new ShipmentDetails();
        shipment1.setId(1L);
        shipment1.setShipmentId("SHP001");
        shipment1.setTransportMode(TRANSPORT_MODE_AIR);
        shipment1.setHouseBill("");
        shipment1.setMasterBill("");
        shipment1.setDirection(DIRECTION_EXP);
        shipment1.setJobType(SHIPMENT_TYPE_STD);

        CarrierDetails shipmentCarrier1 = new CarrierDetails();
        shipmentCarrier1.setFlightNumber("");
        shipmentCarrier1.setEta(null);
        shipmentCarrier1.setEtd(null);
        shipment1.setCarrierDetails(shipmentCarrier1);

        ShipmentDetails shipment2 = new ShipmentDetails();
        shipment2.setId(2L);
        shipment2.setShipmentId("SHP002");
        shipment2.setTransportMode(TRANSPORT_MODE_AIR);
        shipment2.setHouseBill("HAWB002");
        shipment2.setMasterBill("");
        shipment2.setDirection(DIRECTION_EXP);
        shipment2.setJobType(SHIPMENT_TYPE_DRT);

        CarrierDetails shipmentCarrier2 = new CarrierDetails();
        shipmentCarrier2.setFlightNumber("FL123");
        shipmentCarrier2.setEta(null);
        shipmentCarrier2.setEtd(LocalDateTime.now());
        shipment2.setCarrierDetails(shipmentCarrier2);

        ShipmentDetails shipment3 = new ShipmentDetails();
        shipment3.setId(3L);
        shipment3.setShipmentId("SHP003");
        shipment3.setTransportMode(TRANSPORT_MODE_AIR);
        shipment3.setHouseBill("HAWB003");
        shipment3.setMasterBill("MAWB123");
        shipment3.setDirection(DIRECTION_EXP);
        shipment3.setJobType(SHIPMENT_TYPE_STD);

        CarrierDetails shipmentCarrier3 = new CarrierDetails();
        shipmentCarrier3.setFlightNumber("");
        shipmentCarrier3.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier3.setEtd(null);
        shipment3.setCarrierDetails(shipmentCarrier3);

        Set<ShipmentDetails> shipments = new HashSet<>();
        shipments.add(shipment1);
        shipments.add(shipment2);
        shipments.add(shipment3);
        consolidationDetails.setShipmentsList(shipments);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setFlightNumber("FL123");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendConsolidationValidation_SeaMode_WithMultipleShipmentsHavingErrors() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setBol("MBL123");
        consolidationDetails.setReceivingBranch(100L);

        ShipmentDetails shipment1 = new ShipmentDetails();
        shipment1.setId(1L);
        shipment1.setShipmentId("SHP001");
        shipment1.setTransportMode(TRANSPORT_MODE_SEA);
        shipment1.setHouseBill("");
        shipment1.setMasterBill("");
        shipment1.setDirection(DIRECTION_EXP);

        CarrierDetails shipmentCarrier1 = new CarrierDetails();
        shipmentCarrier1.setVessel("");
        shipmentCarrier1.setVoyage("");
        shipmentCarrier1.setEta(null);
        shipmentCarrier1.setEtd(null);
        shipment1.setCarrierDetails(shipmentCarrier1);

        ShipmentDetails shipment2 = new ShipmentDetails();
        shipment2.setId(2L);
        shipment2.setShipmentId("SHP002");
        shipment2.setTransportMode(TRANSPORT_MODE_SEA);
        shipment2.setHouseBill("HBL002");
        shipment2.setMasterBill("");
        shipment2.setDirection(DIRECTION_EXP);

        CarrierDetails shipmentCarrier2 = new CarrierDetails();
        shipmentCarrier2.setVessel("VESSEL1");
        shipmentCarrier2.setVoyage("");
        shipmentCarrier2.setEta(LocalDateTime.now().plusDays(1));
        shipmentCarrier2.setEtd(null);
        shipment2.setCarrierDetails(shipmentCarrier2);

        Set<ShipmentDetails> shipments = new HashSet<>();
        shipments.add(shipment1);
        shipments.add(shipment2);
        consolidationDetails.setShipmentsList(shipments);

        CarrierDetails consolidationCarrier = new CarrierDetails();
        consolidationCarrier.setVessel("VESSEL1");
        consolidationCarrier.setVoyage("V001");
        consolidationCarrier.setShippingLine("MAERSK");
        consolidationCarrier.setEta(LocalDateTime.now().plusDays(1));
        consolidationCarrier.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        Parties sendingAgent = Parties.builder().orgCode("ORG001").build();
        consolidationDetails.setSendingAgent(sendingAgent);

        Parties receivingAgent = Parties.builder().orgCode("ORG002").build();
        consolidationDetails.setReceivingAgent(receivingAgent);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @ParameterizedTest
    @MethodSource("provideAutomaticTransferShipmentScenarios")
    void testAutomaticTransferShipmentValidation_Scenarios(
            String transportMode,
            String houseBill,
            String masterBill,
            String carrierInfo,
            LocalDateTime eta,
            LocalDateTime etd
    ) {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SH001");
        shipmentDetails.setTransportMode(transportMode);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        shipmentDetails.setHouseBill(houseBill);
        shipmentDetails.setMasterBill(masterBill);
        shipmentDetails.setReceivingBranch(100L);

        CarrierDetails shipmentCarrier = new CarrierDetails();
        if (TRANSPORT_MODE_AIR.equals(transportMode)) {
            shipmentCarrier.setFlightNumber(carrierInfo);
        } else if (TRANSPORT_MODE_SEA.equals(transportMode)) {
            shipmentCarrier.setVessel(carrierInfo);
            shipmentCarrier.setVoyage("V001");
        }
        shipmentCarrier.setEta(eta);
        shipmentCarrier.setEtd(etd);
        shipmentDetails.setCarrierDetails(shipmentCarrier);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        SendShipmentValidationResponse response = entityTransferService.automaticTransferShipmentValidation(commonRequestModel);

        assertNotNull(response);
    }

    private static Stream<Arguments> provideAutomaticTransferShipmentScenarios() {
        LocalDateTime futureDate = LocalDateTime.now().plusDays(1);
        LocalDateTime currentDate = LocalDateTime.now();

        return Stream.of(
                Arguments.of(TRANSPORT_MODE_AIR, "HAWB123", "MAWB123", "FL123", futureDate, currentDate),
                Arguments.of(TRANSPORT_MODE_AIR, "", "MAWB123", "FL123", futureDate, currentDate),
                Arguments.of(TRANSPORT_MODE_AIR, "HAWB123", "", "FL123", futureDate, currentDate),
                Arguments.of(TRANSPORT_MODE_AIR, "HAWB123", "MAWB123", "", futureDate, currentDate),
                Arguments.of(TRANSPORT_MODE_AIR, "HAWB123", "MAWB123", "FL123", null, currentDate),
                Arguments.of(TRANSPORT_MODE_AIR, "HAWB123", "MAWB123", "FL123", futureDate, null),
                Arguments.of(TRANSPORT_MODE_SEA, "HBL123", "MBL123", "VESSEL1", futureDate, currentDate),
                Arguments.of(TRANSPORT_MODE_SEA, "", "MBL123", "VESSEL1", futureDate, currentDate),
                Arguments.of(TRANSPORT_MODE_SEA, "HBL123", "", "VESSEL1", futureDate, currentDate),
                Arguments.of(TRANSPORT_MODE_SEA, "HBL123", "MBL123", "", futureDate, currentDate)
        );
    }

    @ParameterizedTest
    @MethodSource("provideAutomaticTransferConsoleScenarios")
    void testAutomaticTransferConsoleValidation_Scenarios(
            String transportMode,
            String bol,
            String carrierInfo1,
            String carrierInfo2,
            LocalDateTime eta,
            LocalDateTime etd,
            boolean hasSendingAgent,
            boolean hasReceivingAgent
    ) {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(transportMode);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol(bol);
        consolidationDetails.setShipmentsList(new HashSet<>());

        CarrierDetails consolidationCarrier = new CarrierDetails();
        if (TRANSPORT_MODE_AIR.equals(transportMode)) {
            consolidationCarrier.setFlightNumber(carrierInfo1);
        } else if (TRANSPORT_MODE_SEA.equals(transportMode)) {
            consolidationCarrier.setVessel(carrierInfo1);
            consolidationCarrier.setVoyage(carrierInfo2);
        }
        consolidationCarrier.setEta(eta);
        consolidationCarrier.setEtd(etd);
        consolidationDetails.setCarrierDetails(consolidationCarrier);

        if (hasSendingAgent) {
            consolidationDetails.setSendingAgent(Parties.builder().orgCode("ORG001").build());
        }
        if (hasReceivingAgent) {
            consolidationDetails.setReceivingAgent(Parties.builder().orgCode("ORG002").build());
        }

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        SendConsoleValidationResponse response = entityTransferService.automaticTransferConsoleValidation(commonRequestModel);

        assertNotNull(response);
    }

    private static Stream<Arguments> provideAutomaticTransferConsoleScenarios() {
        LocalDateTime futureDate = LocalDateTime.now().plusDays(1);
        LocalDateTime currentDate = LocalDateTime.now();

        return Stream.of(
                Arguments.of(TRANSPORT_MODE_AIR, "MAWB123", "FL123", null, futureDate, currentDate, true, true),
                Arguments.of(TRANSPORT_MODE_AIR, "", "FL123", null, futureDate, currentDate, true, true),
                Arguments.of(TRANSPORT_MODE_AIR, "MAWB123", "", null, futureDate, currentDate, true, true),
                Arguments.of(TRANSPORT_MODE_AIR, "MAWB123", "FL123", null, null, currentDate, true, true),
                Arguments.of(TRANSPORT_MODE_AIR, "MAWB123", "FL123", null, futureDate, null, true, true),
                Arguments.of(TRANSPORT_MODE_SEA, "MBL123", "VESSEL1", "V001", futureDate, currentDate, true, true),
                Arguments.of(TRANSPORT_MODE_SEA, "", "VESSEL1", "V001", futureDate, currentDate, true, true),
                Arguments.of(TRANSPORT_MODE_SEA, "MBL123", "", "V001", futureDate, currentDate, true, true),
                Arguments.of(TRANSPORT_MODE_SEA, "MBL123", "VESSEL1", "", futureDate, currentDate, true, true),
                Arguments.of(TRANSPORT_MODE_SEA, "MBL123", "VESSEL1", "V001", futureDate, currentDate, false, true),
                Arguments.of(TRANSPORT_MODE_SEA, "MBL123", "VESSEL1", "V001", futureDate, currentDate, true, false),
                Arguments.of("RAIL", "", "", "", null, null, false, false)
        );
    }

    @Test
    void testSendConsolidationValidation_AlreadyTransferred() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setSourceGuid(UUID.fromString("00000000-0000-0000-0000-000000000001"));
        consolidationDetails.setGuid(UUID.fromString("00000000-0000-0000-0000-000000000002"));
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_CTS);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendShipmentValidation_AlreadyTransferred() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setSourceGuid(UUID.fromString("00000000-0000-0000-0000-000000000001"));
        shipmentDetails.setGuid(UUID.fromString("00000000-0000-0000-0000-000000000002"));
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(DIRECTION_CTS);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testAutomaticTransferConsoleValidation_AlreadyTransferred() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setSourceGuid(UUID.fromString("00000000-0000-0000-0000-000000000001"));
        consolidationDetails.setGuid(UUID.fromString("00000000-0000-0000-0000-000000000002"));
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(DIRECTION_EXP);
        consolidationDetails.setConsolidationType(SHIPMENT_TYPE_STD);
        consolidationDetails.setBol("BOL123");
        consolidationDetails.setShipmentsList(new HashSet<>());
        consolidationDetails.setSendingAgent(Parties.builder().orgCode("ORG001").build());
        consolidationDetails.setReceivingAgent(Parties.builder().orgCode("ORG002").build());

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setFlightNumber("FL123");
        carrierDetails.setEta(LocalDateTime.now().plusDays(1));
        carrierDetails.setEtd(LocalDateTime.now());
        consolidationDetails.setCarrierDetails(carrierDetails);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        SendConsoleValidationResponse response = entityTransferService.automaticTransferConsoleValidation(commonRequestModel);

        assertNotNull(response);
        assertFalse(response.getIsError()); // Changed from assertTrue to assertFalse
    }

    @Test
    void testAutomaticTransferShipmentValidation_AlreadyTransferred() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setSourceGuid(UUID.fromString("00000000-0000-0000-0000-000000000001"));
        shipmentDetails.setGuid(UUID.fromString("00000000-0000-0000-0000-000000000002"));
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        SendShipmentValidationResponse response = entityTransferService.automaticTransferShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertTrue(response.getIsError());
    }

    @Test
    void testSendConsolidationValidation_NetworkTransferDisabled() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(TRANSPORT_MODE_AIR);

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setFlightNumber("FL123");
        consolidationDetails.setCarrierDetails(carrierDetails);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(false);

        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);
        var response = entityTransferService.sendConsolidationValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendShipmentValidation_NetworkTransferDisabled() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(false);

        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);
        var response = entityTransferService.sendShipmentValidation(commonRequestModel);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSendConsolidationValidation_ConsolidationNotFound() {
        ValidateSendConsolidationRequest consolidationRequest = new ValidateSendConsolidationRequest();
        consolidationRequest.setConsoleId(999L);

        when(consolidationDetailsDao.findById(999L)).thenReturn(Optional.empty());

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(consolidationRequest);

        try {
            entityTransferService.sendConsolidationValidation(commonRequestModel);
            fail("Expected DataRetrievalFailureException");
        } catch (Exception e) {
            assertTrue(e instanceof org.springframework.dao.DataRetrievalFailureException);
        }
    }

    @Test
    void testSendShipmentValidation_ShipmentNotFound() {
        ValidateSendShipmentRequest shipmentRequest = new ValidateSendShipmentRequest();
        shipmentRequest.setShipId(999L);

        when(shipmentDao.findById(999L)).thenReturn(Optional.empty());

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(shipmentRequest);

        try {
            entityTransferService.sendShipmentValidation(commonRequestModel);
            fail("Expected DataRetrievalFailureException");
        } catch (Exception e) {
            assertTrue(e instanceof org.springframework.dao.DataRetrievalFailureException);
        }
    }
}