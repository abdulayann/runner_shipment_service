package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.INPMServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.commons.requests.AibActionShipment;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IReferenceNumbersDao;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentOrderDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerUpdateFileEntitiesRequest;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.service.IDocumentManagerService;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.AutoUpdateWtVolRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.AutoUpdateWtVolResponse;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.mapper.AttachListShipmentMapper;
import com.dpw.runner.shipment.services.dto.mapper.ShipmentMapper;
import com.dpw.runner.shipment.services.dto.request.AttachListShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.CarrierDetailRequest;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingV3Request;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.LogHistoryRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.TruckDriverDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.awb.AwbGoodsDescriptionInfo;
import com.dpw.runner.shipment.services.dto.request.notification.AibNotificationRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGApprovalRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequestV3;
import com.dpw.runner.shipment.services.dto.response.AttachListShipmentResponse;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.ListContractResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsListResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentListResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentRetrieveLiteResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingShipmentActionsResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ConsoleShipmentData;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksUnAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentWtVolResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TaskCreateResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentEtV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentSailingScheduleRequest;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkPackingResponse;
import com.dpw.runner.shipment.services.dto.v3.response.BulkRoutingResponse;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentSailingScheduleResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.MblDuplicatedLog;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentOrder;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.entity.TriangulationPartner;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.entity.enums.TaskStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.MasterDataHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.helpers.ShipmentMasterDataHelperV3;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.mapper.ShipmentDetailsMapper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.projection.ContainerInfoProjection;
import com.dpw.runner.shipment.services.projection.ShipmentDetailsProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.service.interfaces.IEventsV3Service;
import com.dpw.runner.shipment.services.service.interfaces.ILogsHistoryService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsV3Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.Entity.PartyRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.ContainerV3Util;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.http.auth.AuthenticationException;
import org.apache.poi.ss.formula.functions.T;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.dao.InvalidDataAccessApiUsageException;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;

import static com.dpw.runner.shipment.services.commons.constants.Constants.DG_OCEAN_APPROVAL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.PENDING_ACTION_TASK;
import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.anyList;
import static org.mockito.Mockito.anyLong;
import static org.mockito.Mockito.anyMap;
import static org.mockito.Mockito.anySet;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.argThat;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.isNull;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

@SuppressWarnings("java:S6068")
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentServiceImplV3Test extends CommonMocks {

    @InjectMocks
    private ShipmentServiceImplV3 shipmentServiceImplV3;
    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Mock
    private INotificationDao notificationDao;
    @Mock
    private IShipmentRepository shipmentRepository;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private IContainerDao containerDao;
    @Mock
    private ShipmentMasterDataHelperV3 shipmentMasterDataHelper;
    @Mock
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private ConsolidationV3Service consolidationV3Service;
    @Mock
    private INotesDao notesDao;
    @Mock
    private IPackingDao packingDao;
    @Mock
    private IReferenceNumbersDao referenceNumbersDao;
    @Mock
    private IShipmentOrderDao shipmentOrderDao;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private IRoutingsDao routingsDao;
    @Mock
    private IOrderManagementAdapter orderManagementAdapter;
    @Mock
    private IPackingService packingService;
    @Mock
    private DependentServiceHelper dependentServiceHelper;
    @Mock
    private IShipmentSync shipmentSync;
    @Mock
    private IAuditLogService auditLogService;
    @Mock
    private HblService hblService;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock
    private IRoutingsV3Service routingsV3Service;
    @Mock
    private ShipmentValidationV3Util shipmentValidationV3Util;
    @Mock
    private IDateTimeChangeLogService dateTimeChangeLogService;
    @Mock
    private EventsV3Util eventsV3Util;
    @Mock
    private PackingV3Util packingV3Util;
    @Mock
    private IEventDao eventDao;
    @Mock
    private IEventsV3Service eventsV3Service;
    @Mock
    private ShipmentsV3Util shipmentsV3Util;
    @Mock
    private IPartiesDao partiesDao;
    @Mock
    private ITruckDriverDetailsDao truckDriverDetailsDao;
    @Mock
    private IAwbDao awbDao;
    @Mock
    private V1ServiceUtil v1ServiceUtil;
    @Mock
    private IDocumentManagerService documentManagerService;
    @Mock
    private ILogsHistoryService logsHistoryService;
    @Mock
    private MasterDataHelper masterDataHelper;
    @Mock
    private IHblDao hblDao;
    @Mock
    private IPackingV3Service packingV3Service;
    @Mock
    private IContainerV3Service containerV3Service;
    @Mock
    private ContainerV3Util containerV3Util;
    @Mock
    private CacheManager cacheManager;
    @Mock
    private CustomKeyGenerator keyGenerator;
    @Mock
    private IDpsEventService dpsEventService;
    @Mock
    private ICarrierDetailsDao carrierDetailsDao;
    @Mock
    private ObjectMapper objectMapperMocked;

    @Mock
    private ShipmentDetailsMapper shipmentDetailsMapper;
    @Mock
    private INPMServiceAdapter npmServiceAdapter;
    @Mock
    private NpmContractV3Util npmContractV3Util;
    @Mock
    private IMDMServiceAdapter mdmServiceAdapter;
    @Mock
    private INotificationService notificationService;

    private ShipmentDetails shipmentDetailsEntity;
    private ConsolidationDetails consolidationDetailsEntity;
    private static JsonTestUtility jsonTestUtility;
    private static ShipmentDetails testShipment;
    private static ObjectMapper objectMapper;
    private static ConsolidationDetails testConsol;
    private AibActionShipment aibActionShipment;

    @BeforeAll
    static void init() throws IOException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);

        TenantContext.setCurrentTenant(5);
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
    }

    @AfterEach
    void tearDown() {
        shipmentServiceImplV3.executorService.shutdown();
        shipmentServiceImplV3.executorServiceMasterData.shutdown();
    }

    @BeforeEach
    void setup() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(false).build());
        shipmentServiceImplV3.executorService = Executors.newFixedThreadPool(2);
        shipmentServiceImplV3.executorServiceMasterData = Executors.newFixedThreadPool(2);
        shipmentDetailsEntity = jsonTestUtility.getTestShipment();
        consolidationDetailsEntity = new ConsolidationDetails();
        testShipment = jsonTestUtility.getTestShipment();
        testConsol = jsonTestUtility.getJson("MAWB_CONSOLIDATION", ConsolidationDetails.class);
        aibActionShipment = AibActionShipment.builder().build();
    }

    @Test
    void testGetPendingNotificationCount() {

        when(consoleShipmentMappingDao.pendingStateCountBasedOnRequestType(any(), any())).thenReturn(1);
        when(notificationDao.findAllPendingNotificationCount(any(), any())).thenReturn(1);

        var response = shipmentServiceImplV3.getPendingNotificationCount();
        assertEquals(2, response.getCount());
    }

    @Test
    void listRequestNull() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(null).build();
        ResponseEntity<IRunnerResponse> responseEntity = shipmentServiceImplV3.listShipment(commonRequestModel, true);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listNullIncludeColumns() {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(new ArrayList<>()).build();
        listCommonRequest.setNotificationFlag(true);
        listCommonRequest.setIncludeColumns(List.of());
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();
        ResponseEntity<IRunnerResponse> responseEntity = shipmentServiceImplV3.listShipment(commonRequestModel, true);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listShipmentsWithNotifications() {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(new ArrayList<>()).build();
        listCommonRequest.setNotificationFlag(true);
        listCommonRequest.setIncludeColumns(List.of("ordersCount", "pickupDetails.shipperRef"));
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        ShipmentDetails details = new ShipmentDetails();
        details.setWeight(BigDecimal.valueOf(1l));
        details.setVolume(BigDecimal.valueOf(1l));
        details.setVolumetricWeight(BigDecimal.valueOf(1l));
        details.setChargable(BigDecimal.valueOf(1l));
        ShipmentListResponse shipmentListResponse = new ShipmentListResponse();
        shipmentListResponse.setContainsHazardous(false);
        shipmentListResponse.setWeight(BigDecimal.valueOf(1l));
        shipmentListResponse.setVolume(BigDecimal.valueOf(1l));
        shipmentListResponse.setVolumetricWeight(BigDecimal.valueOf(1l));
        shipmentListResponse.setChargable(BigDecimal.valueOf(1l));
        shipmentDetailsList.add(details);
        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        PageImpl<Long> shipmentIdPage = new PageImpl<>(List.of(1L));
        when(shipmentRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(), any())).thenReturn(shipmentListResponse);
        when(shipmentDao.getIdWithPendingActions(eq(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED), any())).thenReturn(shipmentIdPage);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(V1TenantSettingsResponse.builder().VolumeDecimalPlace(2).WeightDecimalPlace(2).transportModeConfig(true).build());

        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );

        ResponseEntity<IRunnerResponse> httpResponse = shipmentServiceImplV3.listShipment(commonRequestModel, true);
        assertEquals(expectedResponse, httpResponse);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<ShipmentListResponse> shipmentListResponses = ShipmentMapper.INSTANCE.toShipmentListResponses(lst);
        for (var i : shipmentListResponses) {
            if (i.getStatus() != null && i.getStatus() < ShipmentStatus.values().length)
                i.setShipmentStatus(ShipmentStatus.values()[i.getStatus()].toString());
            if (ObjectUtils.isNotEmpty(i.getShipmentOrders()))
                i.setOrdersCount(i.getShipmentOrders().size());
            i.setWeightFormatted("1");
            i.setVolumeFormatted("1");
            i.setVolumetricWeightFormatted("1");
            i.setChargableFormatted("1");
            responseList.add(i);
        }

        return responseList;
    }

    @Test
    void listShipments_AddedReferenceNumber() {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(new ArrayList<>()).build();
        listCommonRequest.setNotificationFlag(true);
        listCommonRequest.setIncludeColumns(List.of("ordersCount", "pickupDetails.shipperRef"));
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        ShipmentListResponse shipmentListResponse = new ShipmentListResponse();
        shipmentListResponse.setContainsHazardous(false);
        shipmentListResponse.setPickupDetails(new PickupDeliveryDetailsListResponse());
        shipmentListResponse.setId(1L);

        ShipmentDetails ship = new ShipmentDetails();
        ship.setId(1L);
        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        referenceNumbers.setType(ReportConstants.SRN);
        referenceNumbers.setReferenceNumber("123");
        ship.setReferenceNumbersList(List.of(referenceNumbers));
        shipmentDetailsList.add(ship);

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        PageImpl<Long> shipmentIdPage = new PageImpl<>(List.of(1L));
        when(shipmentRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(), any())).thenReturn(shipmentListResponse);
        when(shipmentDao.getIdWithPendingActions(eq(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED), any())).thenReturn(shipmentIdPage);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentServiceImplV3.listShipment(commonRequestModel, true);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testGetShipmentAndPacksForConsolidationAssignContainerTray() {
        shipmentDetailsEntity.setId(4L);
        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetailsEntity);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(shipmentDetailsList));
        ShipmentPacksAssignContainerTrayDto.Shipments shipments = ShipmentPacksAssignContainerTrayDto.Shipments.builder().id(4L).packsList(new ArrayList<>()).build();
        when(jsonHelper.convertValueToList(any(), eq(ShipmentPacksAssignContainerTrayDto.Shipments.class))).thenReturn(List.of(shipments));
        when(jsonHelper.convertValueToList(any(), eq(ShipmentPacksAssignContainerTrayDto.Shipments.Packages.class))).thenReturn(new ArrayList<>());
        when(shipmentsContainersMappingDao.findByContainerId(any())).thenReturn(new ArrayList<>());
        ShipmentPacksAssignContainerTrayDto response =
                shipmentServiceImplV3.getShipmentAndPacksForConsolidationAssignContainerTray(1L, 2L);
        assertNotNull(response);
    }

    @Test
    void testGetShipmentAndPacksForConsolidationAssignContainerTrayAssignedFCL() {
        shipmentDetailsEntity.setId(4L);
        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetailsEntity);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(shipmentDetailsList));
        ShipmentPacksAssignContainerTrayDto.Shipments shipments = ShipmentPacksAssignContainerTrayDto.Shipments.builder()
                .id(4L)
                .shipmentType(Constants.CARGO_TYPE_FCL)
                .packsList(new ArrayList<>())
                .build();
        ShipmentPacksAssignContainerTrayDto.Shipments shipments1 = ShipmentPacksAssignContainerTrayDto.Shipments.builder()
                .id(5L)
                .packsList(new ArrayList<>())
                .build();
        when(jsonHelper.convertValueToList(any(), eq(ShipmentPacksAssignContainerTrayDto.Shipments.class))).thenReturn(List.of(shipments1, shipments));
        when(jsonHelper.convertValueToList(any(), eq(ShipmentPacksAssignContainerTrayDto.Shipments.Packages.class))).thenReturn(new ArrayList<>());
        when(shipmentsContainersMappingDao.findByContainerId(any())).thenReturn(List.of(ShipmentsContainersMapping.builder().shipmentId(4L).build()));
        ShipmentPacksAssignContainerTrayDto response =
                shipmentServiceImplV3.getShipmentAndPacksForConsolidationAssignContainerTray(1L, 2L);
        assertNotNull(response);
    }

    @Test
    void testGetShipmentAndPacksForConsolidationAssignContainerTrayAssigned() {
        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetailsEntity);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(shipmentDetailsList));
        ShipmentPacksAssignContainerTrayDto.Shipments shipments = ShipmentPacksAssignContainerTrayDto.Shipments.builder()
                .id(4L)
                .packsList(new ArrayList<>())
                .build();
        ShipmentPacksAssignContainerTrayDto.Shipments shipments1 = ShipmentPacksAssignContainerTrayDto.Shipments.builder()
                .id(5L)
                .packsList(new ArrayList<>())
                .build();
        when(jsonHelper.convertValueToList(any(), eq(ShipmentPacksAssignContainerTrayDto.Shipments.class))).thenReturn(List.of(shipments1, shipments));
        when(jsonHelper.convertValueToList(any(), eq(ShipmentPacksAssignContainerTrayDto.Shipments.Packages.class))).thenReturn(new ArrayList<>());
        when(shipmentsContainersMappingDao.findByContainerId(any())).thenReturn(List.of(ShipmentsContainersMapping.builder().shipmentId(4L).build()));
        ShipmentPacksAssignContainerTrayDto response =
                shipmentServiceImplV3.getShipmentAndPacksForConsolidationAssignContainerTray(1L, 2L);
        assertNotNull(response);
    }

    @Test
    void testAttachConsolidation() throws RunnerException {
        when(consolidationV3Service.attachShipments(any())).thenReturn(null);
        String res = shipmentServiceImplV3.attachConsolidation(any());
        assertNull(res);
    }

    @Test
    void testAttachConsolidation2() throws RunnerException {
        when(consolidationV3Service.attachShipments(any())).thenReturn("null");
        String res = shipmentServiceImplV3.attachConsolidation(any());
        assertNotNull(res);
    }

    @Test
    void testGetPendingNotificationData() {
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.ofNullable(ShipmentDetails.builder().build()));
        when(commonUtils.setIncludedFieldsToResponse(any(), any(), any())).thenReturn(new ShipmentPendingNotificationResponse());
        ShipmentPendingNotificationResponse shipmentPendingNotificationResponse = shipmentServiceImplV3.getPendingNotificationData(request);
        assertDoesNotThrow(() -> shipmentPendingNotificationResponse);
    }

    @Test
    void testGetPendingNotificationData_ThrowsException() {
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> shipmentServiceImplV3.getPendingNotificationData(request));
    }

    @Test
    void createShipmentInV3Test() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        PackingV3Request packingV3Request = PackingV3Request.builder().packs("2").packsType("BAG").commodity("FAK").build();
        packingV3Request.setWeight(BigDecimal.valueOf(11.5));
        packingV3Request.setVolume(BigDecimal.valueOf(11));
        packingV3Request.setLengthUnit("M");
        ReferenceNumbersRequest referenceNumbersRequest = ReferenceNumbersRequest.builder().build();
        referenceNumbersRequest.setReferenceNumber("SHP2411-A1PG00784");
        referenceNumbersRequest.setType("CRR");
        referenceNumbersRequest.setShipmentId(16787L);
        referenceNumbersRequest.setCountryOfIssue("IND");
        CustomerBookingV3Request customerBookingV3Request = CustomerBookingV3Request.builder().id(1L).transportType(Constants.TRANSPORT_MODE_SEA).cargoType(Constants.CARGO_TYPE_FCL).carrierDetails(CarrierDetailRequest.builder().build()).orderManagementId("eaf227f3-de85-42b4-8180-cf48ccf568f9").build();
        customerBookingV3Request.setPackingList(Collections.singletonList(packingV3Request));
        customerBookingV3Request.setReferenceNumbersList(Collections.singletonList(referenceNumbersRequest));
        customerBookingV3Request.setGrossWeight(BigDecimal.valueOf(13222211));
        customerBookingV3Request.setVolume(BigDecimal.valueOf(6565576));
        customerBookingV3Request.setAdditionalParties(List.of(PartiesRequest.builder().orgCode("asdf").addressCode("afgd").orgId("1234").addressId("1234").build()));

        ShipmentOrder shipmentOrder = ShipmentOrder.builder().shipmentId(1L).orderGuid(UUID.fromString("eaf227f3-de85-42b4-8180-cf48ccf568f9")).build();
        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        Parties importBroker = Parties.builder().orgCode("1223").build();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setImportBroker(importBroker);
        additionalDetails.setExportBroker(importBroker);
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").build().setReferenceNumbersList(Collections.singletonList(referenceNumbers)).setAdditionalDetails(additionalDetails).setGoodsDescription("Abcd");
        shipmentDetails1.setGuid(UUID.randomUUID());
        shipmentDetails1.setId(1L);
        shipmentDetails1.setShipmentOrders(Collections.singletonList(shipmentOrder));
        shipmentDetails1.setAdditionalDetails(new AdditionalDetails());
        shipmentDetails1.setCarrierDetails(CarrierDetails.builder().build());

        when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(ConsolidationDetailsRequest.builder().id(123L).containersList(List.of(ContainerRequest.builder().id(1L).containerCount(2L).commodityGroup("FAK").build())).build());
        doReturn(Pair.of(ConsolidationDetails.builder().build(), null)).when(consolidationV3Service).createConsolidationForBooking(any(), any());

        ReferenceNumbersRequest referenceNumberObj2 = ReferenceNumbersRequest.builder().build();

        when(jsonHelper.convertValue(anyList(), any(TypeReference.class))).thenReturn(Collections.singletonList(referenceNumberObj2));

        when(jsonHelper.convertValueToList(any(), eq(Packing.class))).thenReturn(Collections.singletonList(new Packing()));
        when(jsonHelper.convertValueToList(any(), eq(ReferenceNumbers.class))).thenReturn(Collections.singletonList(referenceNumbers));
        when(jsonHelper.convertValueToList(any(), eq(Containers.class))).thenReturn(List.of(Containers.builder().build()));
        when(referenceNumbersDao.saveEntityFromShipment(any(), any())).thenReturn(Collections.singletonList(referenceNumbers));
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails1);
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        when(shipmentDao.save(any(), eq(false))).thenReturn(shipmentDetails1);
        ShipmentDetailsV3Response shipmentDetailsV3Response = jsonHelper.convertValue(shipmentDetails1, ShipmentDetailsV3Response.class);
        when(jsonHelper.convertValue(shipmentDetails1, ShipmentDetailsV3Response.class)).thenReturn(shipmentDetailsV3Response);
        VolumeWeightChargeable volumeWeightChargeable = new VolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(BigDecimal.TEN);
        volumeWeightChargeable.setChargeableUnit("Kg");
        volumeWeightChargeable.setVolumeWeight(BigDecimal.TEN);
        volumeWeightChargeable.setVolumeWeightUnit("m3");

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setEnableRouteMaster(true);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());

        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setDefaultPackUnit("BAG");
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);

        when(orderManagementAdapter.getOrderByGuid(any())).thenReturn(shipmentDetails1);
        when(packingV3Service.updateBulk(any(), any())).thenReturn(BulkPackingResponse.builder().packingResponseList(Collections.singletonList(new PackingResponse())).build());
        ShipmentDetailsV3Response response = shipmentServiceImplV3.createShipmentInV3(customerBookingV3Request);
        assertNull(response);
    }

    private void mockRunnable() {
        Runnable runnable = () -> System.out.println("Mock runnable executed");
        runnable.run();
    }

    @Test
    void retrieveByIdOrGuidTest() throws RunnerException {
        ShipmentV3Request request = new ShipmentV3Request();
        request.setId(1L);
        Routings routings = new Routings();
        routings.setId(1L);
        routings.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routings.setVesselName("vesel");
        routings.setVoyage("0123");
        routings.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        shipmentDetailsEntity.setRoutingsList(List.of(routings));
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetailsEntity));

        Optional<ShipmentDetails> result = shipmentServiceImplV3.retrieveByIdOrGuid(request);

        assertTrue(result.isPresent());
        assertEquals(shipmentDetailsEntity, result.get());
    }

    @Test
    void retrieveByIdOrGuidTest2() {
        ShipmentV3Request request = new ShipmentV3Request();
        request.setId(1L);

        when(shipmentDao.findById(1L)).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, () -> shipmentServiceImplV3.retrieveByIdOrGuid(request));
    }

    @Test
    void retrieveByIdOrGuidTest3() throws RunnerException {
        ShipmentV3Request request = new ShipmentV3Request();
        UUID guid = UUID.randomUUID();
        request.setGuid(guid);

        when(shipmentDao.findByGuid(guid)).thenReturn(Optional.of(shipmentDetailsEntity));

        Optional<ShipmentDetails> result = shipmentServiceImplV3.retrieveByIdOrGuid(request);

        assertTrue(result.isPresent());
        assertEquals(shipmentDetailsEntity, result.get());
    }

    @Test
    void retrieveByIdOrGuidTest4() {
        ShipmentV3Request request = new ShipmentV3Request();
        UUID guid = UUID.randomUUID();
        request.setGuid(guid);

        when(shipmentDao.findByGuid(guid)).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, () -> shipmentServiceImplV3.retrieveByIdOrGuid(request));
    }

    @Test
    void retrieveByIdOrGuidTest5() {
        ShipmentV3Request request = new ShipmentV3Request(); // both null

        assertThrows(RunnerException.class, () -> shipmentServiceImplV3.retrieveByIdOrGuid(request));
    }

    @Test
    void retrieveByIdOrGuidTest6() {
        assertThrows(NullPointerException.class, () -> shipmentServiceImplV3.retrieveByIdOrGuid(null));
    }

    @Test
    void isNotAllowedToViewShipmentTest() {
        consolidationDetailsEntity.setReceivingBranch(5L);
        shipmentDetailsEntity.setReceivingBranch(999L);

        boolean result = shipmentServiceImplV3.isNotAllowedToViewShipment(null, shipmentDetailsEntity, 5L, consolidationDetailsEntity);

        assertFalse(result);
    }

    @Test
    void isNotAllowedToViewShipmentTest2() {
        TriangulationPartner partner = new TriangulationPartner();
        partner.setTriangulationPartner(5L);
        consolidationDetailsEntity.setTriangulationPartnerList(List.of(partner));
        shipmentDetailsEntity.setReceivingBranch(999L);

        boolean result = shipmentServiceImplV3.isNotAllowedToViewShipment(null, shipmentDetailsEntity, 5L, consolidationDetailsEntity);

        assertFalse(result);
    }

    @Test
    void isNotAllowedToViewShipmentTest3() {
        consolidationDetailsEntity.setTriangulationPartner(5L);
        shipmentDetailsEntity.setReceivingBranch(999L);

        boolean result = shipmentServiceImplV3.isNotAllowedToViewShipment(null, shipmentDetailsEntity, 5L, consolidationDetailsEntity);

        assertFalse(result);
    }

    @Test
    void isNotAllowedToViewShipmentTest4() {
        shipmentDetailsEntity.setReceivingBranch(5L);

        boolean result = shipmentServiceImplV3.isNotAllowedToViewShipment(null, shipmentDetailsEntity, 5L, null);

        assertFalse(result);
    }

    @Test
    void isNotAllowedToViewShipmentTest5() {
        TriangulationPartner partner = new TriangulationPartner();
        partner.setTriangulationPartner(5L);
        shipmentDetailsEntity.setReceivingBranch(999L);

        boolean result = shipmentServiceImplV3.isNotAllowedToViewShipment(List.of(partner), shipmentDetailsEntity, 5L, null);

        assertFalse(result);
    }

    @Test
    void isNotAllowedToViewShipmentTest6() {
        shipmentDetailsEntity.setTriangulationPartner(5L);

        boolean result = shipmentServiceImplV3.isNotAllowedToViewShipment(null, shipmentDetailsEntity, 5L, null);

        assertFalse(result);
    }

    @Test
    void isNotAllowedToViewShipmentTest7() {
        shipmentDetailsEntity.setReceivingBranch(999L);
        shipmentDetailsEntity.setTriangulationPartner(888L);
        consolidationDetailsEntity.setReceivingBranch(777L);
        consolidationDetailsEntity.setTriangulationPartner(666L);
        consolidationDetailsEntity.setTriangulationPartnerList(List.of());

        boolean result = shipmentServiceImplV3.isNotAllowedToViewShipment(List.of(), shipmentDetailsEntity, 5L, consolidationDetailsEntity);

        assertTrue(result);
    }

    @Test
    void testRetrieveShipmentData_byId_success() throws Exception {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(123L).build();
        CommonRequestModel requestModel = CommonRequestModel.builder().data(getRequest).build();
        populateShipmentDetails();
        ShipmentRetrieveLiteResponse shipmentRetrieveLiteResponse = new ShipmentRetrieveLiteResponse();
        shipmentRetrieveLiteResponse.setStatus(0);
        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(123L);
        consoleShipmentMapping.setConsolidationId(12354L);

        when(shipmentDao.findById(123L)).thenReturn(Optional.of(shipmentDetailsEntity));
        when(consoleShipmentMappingDao.findByShipmentId(any())).thenReturn(Collections.singletonList(consoleShipmentMapping));
        when(modelMapper.map(any(), eq(ShipmentRetrieveLiteResponse.class)))
                .thenReturn(shipmentRetrieveLiteResponse);
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);

        ShipmentRetrieveLiteResponse response = shipmentServiceImplV3.retireveShipmentData(requestModel, "SOME_SOURCE");

        assertNotNull(response);
        verify(shipmentDao).findById(123L);
    }

    @Test
    void testRetrieveShipmentData_byGuid_success() throws Exception {
        UUID guid = UUID.randomUUID();
        CommonGetRequest getRequest = CommonGetRequest.builder().guid(guid.toString()).build();
        CommonRequestModel requestModel = CommonRequestModel.builder().data(getRequest).build();
        populateShipmentDetails();

        when(shipmentDao.findByGuid(guid)).thenReturn(Optional.of(shipmentDetailsEntity));
        when(modelMapper.map(any(), eq(ShipmentRetrieveLiteResponse.class)))
                .thenReturn(new ShipmentRetrieveLiteResponse());
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);

        ShipmentRetrieveLiteResponse response = shipmentServiceImplV3.retireveShipmentData(requestModel, "SOME_SOURCE");

        assertNotNull(response);
        verify(shipmentDao).findByGuid(guid);
    }

    @Test
    void testRetrieveShipmentData_nteSource1() {
        populateShipmentDetails();
        CommonGetRequest getRequest = CommonGetRequest.builder().id(999L).build();
        CommonRequestModel requestModel = CommonRequestModel.builder().data(getRequest).build();

        when(shipmentDao.findShipmentByIdWithQuery(999L)).thenReturn(Optional.of(shipmentDetailsEntity));

        assertThrows(AuthenticationException.class, () -> shipmentServiceImplV3.retireveShipmentData(requestModel, "network_transfer"));
    }

    @Test
    void testRetrieveShipmentData_nteSource2() {
        populateShipmentDetails();
        UUID guid = UUID.randomUUID();
        CommonGetRequest getRequest = CommonGetRequest.builder().guid(guid.toString()).build();
        CommonRequestModel requestModel = CommonRequestModel.builder().data(getRequest).build();

        when(shipmentDao.findShipmentByGuidWithQuery(guid)).thenReturn(Optional.of(shipmentDetailsEntity));

        assertThrows(AuthenticationException.class, () -> shipmentServiceImplV3.retireveShipmentData(requestModel, "network_transfer"));
    }

    @Test
    void testRetrieveShipmentData_nteSource3() {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(999L).build();
        CommonRequestModel requestModel = CommonRequestModel.builder().data(getRequest).build();

        when(shipmentDao.findShipmentByIdWithQuery(999L)).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, () -> shipmentServiceImplV3.retireveShipmentData(requestModel, "network_transfer"));
    }

    @Test
    void testRetrieveShipmentData_nteSource4() throws AuthenticationException, RunnerException {
        populateShipmentDetails();
        shipmentDetailsEntity.setPackingList(null);
        shipmentDetailsEntity.setReceivingBranch(5L);
        consolidationDetailsEntity.setReceivingBranch(777L);
        shipmentDetailsEntity.setConsolidationList(Set.of(consolidationDetailsEntity));

        CommonGetRequest getRequest = CommonGetRequest.builder().id(999L).build();
        CommonRequestModel requestModel = CommonRequestModel.builder().data(getRequest).build();

        when(shipmentDao.findShipmentByIdWithQuery(999L)).thenReturn(Optional.of(shipmentDetailsEntity));
        when(modelMapper.map(any(), eq(ShipmentRetrieveLiteResponse.class)))
                .thenReturn(new ShipmentRetrieveLiteResponse());
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);

        var response = shipmentServiceImplV3.retireveShipmentData(requestModel, "network_transfer");

        assertNotNull(response);
    }

    @Test
    void testRetrieveShipmentData_idAndGuidNull() {
        CommonGetRequest getRequest = CommonGetRequest.builder().build();
        CommonRequestModel requestModel = CommonRequestModel.builder().data(getRequest).build();

        RunnerException ex = assertThrows(RunnerException.class,
                () -> shipmentServiceImplV3.retireveShipmentData(requestModel, "SOME_SOURCE"));

        assertEquals(ShipmentConstants.ID_GUID_NULL_ERROR, ex.getMessage());
    }

    @Test
    void testRetrieveShipmentData_shipmentNotFound() {
        UUID guid = UUID.randomUUID();
        CommonGetRequest getRequest = CommonGetRequest.builder().guid(guid.toString()).build();
        CommonRequestModel requestModel = CommonRequestModel.builder().data(getRequest).build();

        when(shipmentDao.findByGuid(guid)).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class,
                () -> shipmentServiceImplV3.retireveShipmentData(requestModel, "SOME_SOURCE"));
    }

    @Test
    void testRetrieveById_shipmentNotFound() {
        UUID guid = UUID.randomUUID();
        CommonGetRequest getRequest = CommonGetRequest.builder().guid(guid.toString()).build();
        CommonRequestModel requestModel = CommonRequestModel.builder().data(getRequest).build();

        when(shipmentDao.findByGuid(guid)).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class,
                () -> shipmentServiceImplV3.retrieveById(requestModel, false, "SOME_SOURCE"));
    }

    @Test
    void retrieveByIdTest() throws RunnerException, AuthenticationException {
        Routings routings = new Routings();
        routings.setId(1L);
        routings.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routings.setVesselName("vessel");
        routings.setVoyage("0123");
        routings.setMode("SEA");
        routings.setIsSelectedForDocument(true);
        shipmentDetailsEntity.setRoutingsList(List.of(routings));
        shipmentDetailsEntity.setId(1l);
        shipmentDetailsEntity.setGuid(UUID.randomUUID());

        Routings routings1 = new Routings();
        routings1.setId(3L);
        routings1.setMode(TRANSPORT_MODE_SEA);
        routings1.setConsolidationId(1L);
        routings1.setVoyage("0123");
        routings1.setVesselName("vessel");
        routings1.setPol("pol");
        routings1.setFlightNumber("0123");
        routings1.setPod("pod");
        routings1.setCarriage(RoutingCarriage.MAIN_CARRIAGE);

        Routings routings2 = new Routings();
        routings2.setId(3l);
        routings2.setMode(TRANSPORT_MODE_SEA);
        routings2.setConsolidationId(1L);
        routings2.setVoyage("0123");
        routings2.setVesselName("vessel");
        routings2.setPol("pol");
        routings2.setPod("pod");
        routings1.setFlightNumber("0123");
        routings2.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOriginPort("origin");
        carrierDetails.setDestinationPort("dest");
        shipmentDetailsEntity.setCarrierDetails(carrierDetails);
        shipmentDetailsEntity.setTransportInfoStatus(TransportInfoStatus.IH);
        shipmentDetailsEntity.setRoutingsList(List.of(routings1,routings2));

        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        var shipId = 1L;
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(shipId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetailsEntity));
        when(notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(anyList(), anyString())).thenReturn(Map.of(1L, 5));

        when(modelMapper.map(any(), any())).thenReturn(ShipmentRetrieveLiteResponse.builder().build());

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetailsEntity));

        ShipmentRetrieveLiteResponse result = shipmentServiceImplV3.retrieveById(commonRequestModel, false, null);

        assertNotNull(result);
    }

    @Test
    void test_changeConsolidationDGValues_makeConsoleDG_true() {
        Long consolidationId = 1L;
        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetailsEntity));
        when(consolidationDetailsDao.updateV3(any(), anyBoolean())).thenReturn(consolidationDetailsEntity);

        ConsolidationDetails result = shipmentServiceImplV3.changeConsolidationDGValues(
                true, new AtomicBoolean(false), consolidationId, shipmentDetailsEntity, null);

        assertNotNull(result);
        verify(consolidationDetailsDao).updateV3(any(), anyBoolean());
    }

    @Test
    void test_changeConsolidationDGValues_makeConsoleNonDG_true_allNonDG() {
        Long consolidationId = 1L;
        shipmentDetailsEntity.setId(10L);
        consolidationDetailsEntity.setHazardous(true);

        ConsoleShipmentMapping consoleShipmentMapping1 = new ConsoleShipmentMapping();
        consoleShipmentMapping1.setShipmentId(5L);
        consoleShipmentMapping1.setConsolidationId(consolidationId);
        ConsoleShipmentMapping consoleShipmentMapping2 = new ConsoleShipmentMapping();
        consoleShipmentMapping2.setShipmentId(7L);
        consoleShipmentMapping2.setConsolidationId(consolidationId);

        when(shipmentDao.findByShipmentIdInAndContainsHazardous(anyList(), eq(true)))
                .thenReturn(Collections.emptyList());
        when(consoleShipmentMappingDao.findByConsolidationId(consolidationId)).thenReturn(List.of(consoleShipmentMapping1, consoleShipmentMapping2));
        when(consolidationDetailsDao.updateV3(any(), anyBoolean())).thenReturn(consolidationDetailsEntity);

        AtomicBoolean makeConsoleNonDG = new AtomicBoolean(true);
        ConsolidationDetails result = shipmentServiceImplV3.changeConsolidationDGValues(
                false, makeConsoleNonDG, consolidationId, shipmentDetailsEntity, consolidationDetailsEntity);

        assertNotNull(result);
        verify(consolidationDetailsDao).updateV3(any(), anyBoolean());
    }

    @Test
    void test_changeConsolidationDGValues_makeConsoleNonDG_true_notAllNonDG() {
        Long consolidationId = 1L;
        shipmentDetailsEntity.setId(10L);

        ConsoleShipmentMapping consoleShipmentMapping1 = new ConsoleShipmentMapping();
        consoleShipmentMapping1.setShipmentId(5L);
        consoleShipmentMapping1.setConsolidationId(consolidationId);
        ConsoleShipmentMapping consoleShipmentMapping2 = new ConsoleShipmentMapping();
        consoleShipmentMapping2.setShipmentId(7L);
        consoleShipmentMapping2.setConsolidationId(consolidationId);

        when(shipmentDao.findByShipmentIdInAndContainsHazardous(anyList(), eq(true)))
                .thenReturn(List.of(shipmentDetailsEntity)); // at least one DG shipment
        when(consoleShipmentMappingDao.findByConsolidationId(consolidationId)).thenReturn(List.of(consoleShipmentMapping1, consoleShipmentMapping2));

        AtomicBoolean makeConsoleNonDG = new AtomicBoolean(true);
        ConsolidationDetails result = shipmentServiceImplV3.changeConsolidationDGValues(
                false, makeConsoleNonDG, consolidationId, shipmentDetailsEntity, consolidationDetailsEntity);

        assertNull(result);
        verify(consolidationDetailsDao, never()).save(any(), anyBoolean(), anyBoolean());
    }

    @Test
    void test_changeConsolidationDGValues_neither_condition_true() {
        shipmentDetailsEntity.setId(10L);

        ConsolidationDetails result = shipmentServiceImplV3.changeConsolidationDGValues(
                false, new AtomicBoolean(false), 1L, shipmentDetailsEntity, consolidationDetailsEntity);

        assertNull(result);
    }

    @Test
    void test_getConsolidationDetails_whenPresent() {
        Long id = 1L;
        when(consolidationDetailsDao.findById(id)).thenReturn(Optional.of(consolidationDetailsEntity));

        ConsolidationDetails result = shipmentServiceImplV3.getConsolidationDetails(id, null);
        assertEquals(consolidationDetailsEntity, result);
    }

    @Test
    void test_getConsolidationDetails_whenPassed() {
        ConsolidationDetails result = shipmentServiceImplV3.getConsolidationDetails(1L, consolidationDetailsEntity);
        assertEquals(consolidationDetailsEntity, result);
    }

    @Test
    void test_getConsolidationDetails_whenMissing() {
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class,
                () -> shipmentServiceImplV3.getConsolidationDetails(1L, null));
    }

    @Test
    void test_checkIfAllShipmentsAreNonDG_true() {
        List<Long> ids = List.of(1L, 2L);
        when(shipmentDao.findByShipmentIdInAndContainsHazardous(ids, true))
                .thenReturn(Collections.emptyList());

        boolean result = shipmentServiceImplV3.checkIfAllShipmentsAreNonDG(ids);
        assertTrue(result);
    }

    @Test
    void test_checkIfAllShipmentsAreNonDG_false() {
        List<Long> ids = List.of(1L, 2L);
        when(shipmentDao.findByShipmentIdInAndContainsHazardous(ids, true))
                .thenReturn(List.of(shipmentDetailsEntity));

        boolean result = shipmentServiceImplV3.checkIfAllShipmentsAreNonDG(ids);
        assertFalse(result);
    }

    @Test
    void test_checkIfAllShipmentsAreNonDG_emptyList() {
        boolean result = shipmentServiceImplV3.checkIfAllShipmentsAreNonDG(Collections.emptyList());
        assertTrue(result);
    }

    @Test
    void test_saveConsolidationDGValue_dgFlag_true_changeRequired() {
        consolidationDetailsEntity.setHazardous(false);
        when(consolidationDetailsDao.updateV3(any(), anyBoolean())).thenReturn(consolidationDetailsEntity);

        ConsolidationDetails result = shipmentServiceImplV3.saveConsolidationDGValue(true, consolidationDetailsEntity);
        assertTrue(result.getHazardous());
    }

    @Test
    void test_saveConsolidationDGValue_dgFlag_false_changeRequired() {
        consolidationDetailsEntity.setHazardous(true);
        when(consolidationDetailsDao.updateV3(any(), anyBoolean())).thenReturn(consolidationDetailsEntity);

        ConsolidationDetails result = shipmentServiceImplV3.saveConsolidationDGValue(false, consolidationDetailsEntity);
        assertFalse(result.getHazardous());
    }

    @Test
    void test_saveConsolidationDGValue_noChangeNeeded() {
        consolidationDetailsEntity.setHazardous(true);

        ConsolidationDetails result = shipmentServiceImplV3.saveConsolidationDGValue(true, consolidationDetailsEntity);
        assertNull(result);
    }

    private void populateShipmentDetails() {
        shipmentDetailsEntity.setId(123L);
        shipmentDetailsEntity.setGuid(UUID.randomUUID());
        shipmentDetailsEntity.setStatus(1);
        shipmentDetailsEntity.setPackingList(List.of(new Packing()));
    }

    @Test
    void testFindById_whenShipmentExists() {
        Long id = 1L;
        ShipmentDetails shipment = new ShipmentDetails();
        when(shipmentDao.findById(id)).thenReturn(Optional.of(shipment));

        Optional<ShipmentDetails> result = shipmentServiceImplV3.findById(id);

        assertTrue(result.isPresent());
    }

    @Test
    void testUpdateCargoDetailsInShipment_shouldCallDao() {
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        Long shipmentId = 1L;
        shipmentDetails1.setId(shipmentId);
        CargoDetailsResponse response = new CargoDetailsResponse();
        response.setNoOfPacks(10);
        response.setPacksUnit("PKG");
        response.setVolume(BigDecimal.valueOf(2.5));
        response.setVolumeUnit("CBM");
        response.setWeight(BigDecimal.valueOf(100.0));
        response.setWeightUnit("KG");
        response.setVolumetricWeight(BigDecimal.valueOf(110.0));
        response.setVolumetricWeightUnit("KG");
        response.setChargable(BigDecimal.valueOf(105.0));
        response.setChargeableUnit("KG");

        shipmentServiceImplV3.updateCargoDetailsInShipment(shipmentDetails1, response);

        verify(shipmentDao).updateCargoDetailsInShipment(
                eq(shipmentId),
                eq(10), eq("PKG"),
                eq(BigDecimal.valueOf(2.5)), eq("CBM"),
                eq(BigDecimal.valueOf(100.0)), eq("KG"),
                eq(BigDecimal.valueOf(110.0)), eq("KG"),
                eq(BigDecimal.valueOf(105.0)), eq("KG"));
    }

    @Test
    void testUpdateShipmentDetailsFromPacks_shouldCallDao() {
        shipmentServiceImplV3.updateShipmentDetailsFromPacks(1L, DateBehaviorType.ACTUAL, LocalDateTime.now(), ShipmentPackStatus.BOOKED);
        verify(shipmentDao).updateShipmentDetailsFromPacks(any(), any(), any(), any());
    }

    @Test
    void testGetAllMasterData_whenShipmentNotFound_shouldThrow() {
        when(shipmentDao.findById(1L)).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentServiceImplV3.getAllMasterData(1L, "source");
        });
    }

    @Test
    void testUpdateShipmentFieldsAfterDetach_shouldNullifyFields() {
        ShipmentDetails shipment = new ShipmentDetails();
        CarrierDetails carrier = new CarrierDetails();
        shipment.setCarrierDetails(carrier);
        shipment.setMasterBill("MBL123");
        shipment.setBookingNumber("BK456");

        shipmentServiceImplV3.updateShipmentFieldsAfterDetach(List.of(shipment));

        assertNull(carrier.getEta());
        assertNull(carrier.getEtd());
        assertNull(carrier.getAta());
        assertNull(carrier.getAtd());
        assertNull(carrier.getShippingLine());
        assertNull(shipment.getMasterBill());
        assertNull(shipment.getBookingNumber());
    }

    @Test
    void testUpdateSailingScheduleDataToShipment_forSeaMode_shouldCallSeaUpdate() throws RunnerException {
        ShipmentSailingScheduleRequest request = new ShipmentSailingScheduleRequest();
        RoutingsRequest routing = new RoutingsRequest();
        routing.setShipmentId(1L);
        request.setRoutings(List.of(routing));

        Routings routing1 = new Routings();
        routing1.setShipmentId(1L);
        routing1.setCarriage(RoutingCarriage.MAIN_CARRIAGE);

        List<Routings> routingsList = new ArrayList<>();
        routingsList.add(routing1);
        shipmentDetailsEntity.setTransportMode("SEA");
        shipmentDetailsEntity.setId(1L);

        when(routingsV3Service.getRoutingsByShipmentId(anyLong())).thenReturn(routingsList);
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetailsEntity));
        when(routingsV3Service.updateBulk(any(), eq(SHIPMENT))).thenReturn(new BulkRoutingResponse());
        doNothing().when(shipmentDao).updateSailingScheduleRelatedInfo(any(), anyLong());

        shipmentServiceImplV3.updateSailingScheduleDataToShipment(request);

        verify(shipmentDao).updateSailingScheduleRelatedInfo(request, 1L);
    }

    @Test
    void testUpdateSailingScheduleDataToShipment_empty() throws RunnerException {
        ShipmentSailingScheduleRequest request = new ShipmentSailingScheduleRequest();
        ShipmentSailingScheduleResponse response = shipmentServiceImplV3.updateSailingScheduleDataToShipment(request);
        assertNotNull(response);
    }

    @Test
    void testUpdateSailingScheduleDataToShipment_Exception() {
        ShipmentSailingScheduleRequest request = new ShipmentSailingScheduleRequest();
        RoutingsRequest routing = new RoutingsRequest();
        routing.setShipmentId(1L);
        request.setRoutings(List.of(routing));

        Routings routing1 = new Routings();
        routing1.setShipmentId(1L);
        routing1.setCarriage(RoutingCarriage.MAIN_CARRIAGE);

        shipmentDetailsEntity.setTransportMode("SEA");
        shipmentDetailsEntity.setId(1L);
        List<ConsoleShipmentMapping> consoleShipmentMappings = new ArrayList<>();
        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        consoleShipmentMappings.add(consoleShipmentMapping);

        when(consoleShipmentMappingDao.findByShipmentId(anyLong())).thenReturn(consoleShipmentMappings);
        assertThrows(ValidationException.class, () -> shipmentServiceImplV3.updateSailingScheduleDataToShipment(request));
    }

    @Test
    void testUpdateSailingScheduleDataToShipment_forAirMode_shouldCallAirUpdate() throws RunnerException {
        ShipmentSailingScheduleRequest request = new ShipmentSailingScheduleRequest();
        RoutingsRequest routing = new RoutingsRequest();
        routing.setShipmentId(1L);
        request.setRoutings(List.of(routing));

        ShipmentDetails details = new ShipmentDetails();
        details.setTransportMode("AIR");
        details.setId(1L);
        details.setCarrierDetails(new CarrierDetails());

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(details));
        when(routingsV3Service.updateBulk(any(), eq(SHIPMENT))).thenReturn(new BulkRoutingResponse());
        doNothing().when(shipmentDao).updateSailingScheduleRelatedInfoForAir(any(), anyLong());

        shipmentServiceImplV3.updateSailingScheduleDataToShipment(request);

        verify(shipmentDao).updateSailingScheduleRelatedInfoForAir(request, 1L);
    }

    @Test
    void testGetAllMasterData_whenShipmentExists_shouldReturnMasterDataMap() {
        // Given
        Long shipmentId = 123L;
        String xSource = "testSource";

        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        Map<String, Object> dummyMasterData = Map.of("key1", "value1");

        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetailsEntity));

        when(commonUtils.setIncludedFieldsToResponse(eq(shipmentDetailsEntity), anySet(), any(ShipmentDetailsResponse.class)))
                .thenReturn(shipmentDetailsResponse);

        ShipmentServiceImplV3 spyService = Mockito.spy(shipmentServiceImplV3);
        doReturn(dummyMasterData).when(spyService).fetchAllMasterDataByKey(eq(shipmentDetailsEntity), eq(shipmentDetailsResponse));

        Map<String, Object> result = spyService.getAllMasterData(shipmentId, xSource);

        assertNotNull(result);
        assertEquals("value1", result.get("key1"));

        verify(shipmentDao).findById(shipmentId);
        verify(commonUtils).setIncludedFieldsToResponse(eq(shipmentDetailsEntity), anySet(), any(ShipmentDetailsResponse.class));
        verify(spyService).fetchAllMasterDataByKey(shipmentDetailsEntity, shipmentDetailsResponse);
    }

    @Test
    void testGetAllMasterData_whenShipmentExists_shouldReturnMasterDataMap_ForNte() {
        // Given
        Long shipmentId = 123L;
        String xSource = "network_transfer";

        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        Map<String, Object> dummyMasterData = Map.of("key1", "value1");

        when(shipmentDao.findShipmentByIdWithQuery(shipmentId)).thenReturn(Optional.of(shipmentDetailsEntity));

        when(commonUtils.setIncludedFieldsToResponse(eq(shipmentDetailsEntity), anySet(), any(ShipmentDetailsResponse.class)))
                .thenReturn(shipmentDetailsResponse);

        ShipmentServiceImplV3 spyService = Mockito.spy(shipmentServiceImplV3);
        doReturn(dummyMasterData).when(spyService).fetchAllMasterDataByKey(eq(shipmentDetailsEntity), eq(shipmentDetailsResponse));

        Map<String, Object> result = spyService.getAllMasterData(shipmentId, xSource);

        assertNotNull(result);
        assertEquals("value1", result.get("key1"));

        verify(shipmentDao).findShipmentByIdWithQuery(shipmentId);
        verify(commonUtils).setIncludedFieldsToResponse(eq(shipmentDetailsEntity), anySet(), any(ShipmentDetailsResponse.class));
        verify(spyService).fetchAllMasterDataByKey(shipmentDetailsEntity, shipmentDetailsResponse);
    }

    @Test
    void testGetShipmentAndPacks_whenNoMappings_thenReturnsEmptyResponse() {
        // Given
        Long containerId = 1L;
        when(shipmentsContainersMappingDao.findByContainerId(containerId)).thenReturn(Collections.emptyList());

        // When
        ShipmentPacksUnAssignContainerTrayDto result = shipmentServiceImplV3.getShipmentAndPacksForConsolidationUnAssignContainerTray(containerId);

        // Then
        assertNotNull(result);
        verify(shipmentsContainersMappingDao).findByContainerId(containerId);
        verifyNoMoreInteractions(shipmentDao, jsonHelper);
    }

    @Test
    void testGetShipmentAndPacks_whenValidMappings_thenReturnsMappedShipmentsWithFilteredPacks() {
        // Given
        Long containerId = 1L;

        // Mock mappings
        ShipmentsContainersMapping mapping = new ShipmentsContainersMapping();
        mapping.setShipmentId(100L);
        List<ShipmentsContainersMapping> mappings = List.of(mapping);

        // Mock shipment details with matching container ID pack
        Packing pack1 = new Packing();
        pack1.setContainerId(containerId);
        pack1.setId(1L);

        Packing pack2 = new Packing(); // another pack for control
        pack2.setContainerId(2L); // won't match
        pack2.setId(2L);

        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setId(100L);
        shipment.setPackingList(List.of(pack1, pack2));

        when(shipmentsContainersMappingDao.findByContainerId(containerId)).thenReturn(mappings);
        when(shipmentDao.findShipmentsByIds(Set.of(100L))).thenReturn(List.of(shipment));

        ShipmentPacksUnAssignContainerTrayDto.Shipments shipmentsDto = new ShipmentPacksUnAssignContainerTrayDto.Shipments();
        shipmentsDto.setId(100L);

        ShipmentPacksUnAssignContainerTrayDto.Shipments.Packages packing = new ShipmentPacksUnAssignContainerTrayDto.Shipments.Packages();
        packing.setContainerId(1L);

        // Mock JSON helper
        when(jsonHelper.convertValueToList(anyList(), eq(ShipmentPacksUnAssignContainerTrayDto.Shipments.class)))
                .thenReturn(List.of(shipmentsDto));
        when(jsonHelper.convertValueToList(eq(List.of(pack1)), eq(ShipmentPacksUnAssignContainerTrayDto.Shipments.Packages.class)))
                .thenReturn(List.of(packing));

        // When
        ShipmentPacksUnAssignContainerTrayDto result = shipmentServiceImplV3.getShipmentAndPacksForConsolidationUnAssignContainerTray(containerId);

        // Then
        assertNotNull(result);
        assertEquals(1, result.getShipmentsList().size());
        assertEquals(1, result.getShipmentsList().get(0).getPacksList().size());
        assertEquals(1L, result.getShipmentsList().get(0).getPacksList().get(0).getContainerId());

        verify(shipmentsContainersMappingDao).findByContainerId(containerId);
        verify(shipmentDao).findShipmentsByIds(Set.of(100L));
        verify(jsonHelper).convertValueToList(anyList(), eq(ShipmentPacksUnAssignContainerTrayDto.Shipments.class));
        verify(jsonHelper).convertValueToList(eq(List.of(pack1)), eq(ShipmentPacksUnAssignContainerTrayDto.Shipments.Packages.class));
    }

    @Test
    void testCreate_success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(true);
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setMasterBill(null);
        AdditionalDetails additionalDetails = getMockAdditionalDetails(LocalDateTime.now(), true, true, true);
        CarrierDetails mockCarrierDetails = mockShipment.getCarrierDetails();
        mockCarrierDetails.setEta(LocalDateTime.now());
        mockCarrierDetails.setEtd(LocalDateTime.now());
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ShipmentV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentV3Request.class);
        List<Events> eventsList = List.of(Events.builder()
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .eventCode("eventType").build());

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsV3Response mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsV3Response.class);
        mockShipmentRequest.setIsChargableEditable(true);
        commonRequestModel.setData(mockShipmentRequest);
        mockShipment.setId(2L);

        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipmentSettings();
        doNothing().when(shipmentValidationV3Util).validateStaleShipmentUpdateError(any(), anyBoolean());
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        doNothing().when(shipmentValidationV3Util).processDGValidations(any(), any(), any());
        mockTenantSettings();
        when(shipmentsV3Util.generateShipmentId(any())).thenReturn("ShipmentId");
        when(shipmentDao.save(any(), anyBoolean())).thenReturn(mockShipment);
        doNothing().when(dateTimeChangeLogService).createEntryFromShipment(any(), any());
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        when(eventsV3Util.createOrUpdateEvents(any(), any(), anyList(), anyBoolean())).thenReturn(eventsList);
        doNothing().when(commonUtils).updateEventWithMasterData(anyList());
        when(eventDao.updateEntityFromOtherEntity(anyList(), any(), anyString())).thenReturn(eventsList);
        doNothing().when(eventsV3Service).updateAtaAtdInShipment(anyList(), any(), any());
        when(commonUtils.convertToEntityList(anyList(), any(), any())).thenReturn(List.of(new Parties()));
        when(partiesDao.updateEntityFromOtherEntity(anyList(), any(), anyString())).thenReturn(List.of(new Parties()));
        doNothing().when(auditLogService).addAuditLog(any());

        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsV3Response.class))).thenReturn(mockShipmentResponse);

        ShipmentDetailsV3Response actualResponse = shipmentServiceImplV3.create(commonRequestModel);

        assertNotNull(actualResponse);
        assertEquals(mockShipmentResponse, actualResponse);
    }

    @Test
    void testCreate_successWithDestinationQuoteValidationException() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(true);
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setMasterBill(null);
        AdditionalDetails additionalDetails = getMockAdditionalDetails(LocalDateTime.now(), true, true, true);
        CarrierDetails mockCarrierDetails = mockShipment.getCarrierDetails();
        mockCarrierDetails.setEta(LocalDateTime.now());
        mockCarrierDetails.setEtd(LocalDateTime.now());
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setDestinationContractId("DPWQ-124");
        mockShipment.setDestinationCurrentPartyForQuote("CLIENT");
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ListContractResponse listContractResponse = getMockListContractResponse();
        ListContractResponse.FilterParams filterParams = listContractResponse.getContracts().get(0).getContract_usage().get(0).getFilter_params();
        filterParams.setCargo_type(List.of("20GP"));
        listContractResponse.getContracts().get(0).getContract_usage().get(0).setFilter_params(filterParams);

        ShipmentV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentV3Request.class);
        List<Events> eventsList = List.of(Events.builder()
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .eventCode("eventType").build());

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        mockShipmentRequest.setIsChargableEditable(true);
        mockShipmentRequest.getClient().setOrgCode("FRC1234");
        commonRequestModel.setData(mockShipmentRequest);
        mockShipment.setId(2L);

        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        when(jsonHelper.convertValue(any(), eq(ListContractResponse.class))).thenReturn(listContractResponse);
        DependentServiceResponse mockResponse = new DependentServiceResponse();
        mockResponse.setData(getMockListContractResponse());
        when(npmServiceAdapter.fetchContract(any())).thenReturn(ResponseEntity.ok(mockResponse));
        mockShipmentSettings();
        doNothing().when(shipmentValidationV3Util).validateStaleShipmentUpdateError(any(), anyBoolean());
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        doNothing().when(shipmentValidationV3Util).processDGValidations(any(), any(), any());
        mockTenantSettings();
        when(shipmentsV3Util.generateShipmentId(any())).thenReturn("ShipmentId");
        when(shipmentDao.save(any(), anyBoolean())).thenReturn(mockShipment);
        doNothing().when(dateTimeChangeLogService).createEntryFromShipment(any(), any());
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        when(eventsV3Util.createOrUpdateEvents(any(), any(), anyList(), anyBoolean())).thenReturn(eventsList);
        doNothing().when(commonUtils).updateEventWithMasterData(anyList());
        when(eventDao.updateEntityFromOtherEntity(anyList(), any(), anyString())).thenReturn(eventsList);
        doNothing().when(eventsV3Service).updateAtaAtdInShipment(anyList(), any(), any());
        when(commonUtils.convertToEntityList(anyList(), any(), any())).thenReturn(List.of(new Parties()));
        when(partiesDao.updateEntityFromOtherEntity(anyList(), any(), anyString())).thenReturn(List.of(new Parties()));
        assertThrows(ValidationException.class, () -> shipmentServiceImplV3.create(commonRequestModel));
    }

    @Test
    void testCreate_successWithDestinationQuote() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(true);
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setMasterBill(null);
        AdditionalDetails additionalDetails = getMockAdditionalDetails(LocalDateTime.now(), true, true, true);
        CarrierDetails mockCarrierDetails = mockShipment.getCarrierDetails();
        mockCarrierDetails.setEta(LocalDateTime.now());
        mockCarrierDetails.setEtd(LocalDateTime.now());
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setDestinationContractId("DPWQ-124");
        mockShipment.setDestinationCurrentPartyForQuote("CLIENT");
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ListContractResponse listContractResponse = getMockListContractResponse();

        ShipmentV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentV3Request.class);
        List<Events> eventsList = List.of(Events.builder()
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .eventCode("eventType").build());

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsV3Response mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsV3Response.class);
        mockShipmentRequest.setIsChargableEditable(true);
        mockShipmentRequest.getClient().setOrgCode("FRC1234");
        commonRequestModel.setData(mockShipmentRequest);
        mockShipment.setId(2L);

        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        when(jsonHelper.convertValue(any(), eq(ListContractResponse.class))).thenReturn(listContractResponse);
        DependentServiceResponse mockResponse = new DependentServiceResponse();
        mockResponse.setData(getMockListContractResponse());
        when(npmServiceAdapter.fetchContract(any())).thenReturn(ResponseEntity.ok(mockResponse));
        mockShipmentSettings();
        doNothing().when(shipmentValidationV3Util).validateStaleShipmentUpdateError(any(), anyBoolean());
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        doNothing().when(shipmentValidationV3Util).processDGValidations(any(), any(), any());
        mockTenantSettings();
        when(shipmentsV3Util.generateShipmentId(any())).thenReturn("ShipmentId");
        when(shipmentDao.save(any(), anyBoolean())).thenReturn(mockShipment);
        doNothing().when(dateTimeChangeLogService).createEntryFromShipment(any(), any());
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        when(eventsV3Util.createOrUpdateEvents(any(), any(), anyList(), anyBoolean())).thenReturn(eventsList);
        doNothing().when(commonUtils).updateEventWithMasterData(anyList());
        when(eventDao.updateEntityFromOtherEntity(anyList(), any(), anyString())).thenReturn(eventsList);
        doNothing().when(eventsV3Service).updateAtaAtdInShipment(anyList(), any(), any());
        when(commonUtils.convertToEntityList(anyList(), any(), any())).thenReturn(List.of(new Parties()));
        when(partiesDao.updateEntityFromOtherEntity(anyList(), any(), anyString())).thenReturn(List.of(new Parties()));
        when(containerV3Service.createBulk(any(), any())).thenReturn(new BulkContainerResponse());
        doNothing().when(auditLogService).addAuditLog(any());

        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsV3Response.class))).thenReturn(mockShipmentResponse);

        ShipmentDetailsV3Response actualResponse = shipmentServiceImplV3.create(commonRequestModel);

        assertNotNull(actualResponse);
        assertEquals(mockShipmentResponse, actualResponse);
    }

    @Test
    void testCreate_successWithOriginQuote() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(true);
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setMasterBill(null);
        AdditionalDetails additionalDetails = getMockAdditionalDetails(LocalDateTime.now(), true, true, true);
        CarrierDetails mockCarrierDetails = mockShipment.getCarrierDetails();
        mockCarrierDetails.setEta(LocalDateTime.now());
        mockCarrierDetails.setEtd(LocalDateTime.now());
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setContractId("DPWQ-124");
        mockShipment.setCurrentPartyForQuote("CLIENT");
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ListContractResponse listContractResponse = getMockListContractResponse();

        ShipmentV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentV3Request.class);
        List<Events> eventsList = List.of(Events.builder()
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .eventCode("eventType").build());

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsV3Response mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsV3Response.class);
        mockShipmentRequest.setIsChargableEditable(true);
        mockShipmentRequest.getClient().setOrgCode("FRC1234");
        commonRequestModel.setData(mockShipmentRequest);
        mockShipment.setId(2L);

        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        when(jsonHelper.convertValue(any(), eq(ListContractResponse.class))).thenReturn(listContractResponse);
        DependentServiceResponse mockResponse = new DependentServiceResponse();
        mockResponse.setData(getMockListContractResponse());
        when(npmServiceAdapter.fetchContract(any())).thenReturn(ResponseEntity.ok(mockResponse));
        mockShipmentSettings();
        doNothing().when(shipmentValidationV3Util).validateStaleShipmentUpdateError(any(), anyBoolean());
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        doNothing().when(shipmentValidationV3Util).processDGValidations(any(), any(), any());
        mockTenantSettings();
        when(shipmentsV3Util.generateShipmentId(any())).thenReturn("ShipmentId");
        when(shipmentDao.save(any(), anyBoolean())).thenReturn(mockShipment);
        doNothing().when(dateTimeChangeLogService).createEntryFromShipment(any(), any());
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        when(eventsV3Util.createOrUpdateEvents(any(), any(), anyList(), anyBoolean())).thenReturn(eventsList);
        doNothing().when(commonUtils).updateEventWithMasterData(anyList());
        when(eventDao.updateEntityFromOtherEntity(anyList(), any(), anyString())).thenReturn(eventsList);
        doNothing().when(eventsV3Service).updateAtaAtdInShipment(anyList(), any(), any());
        when(commonUtils.convertToEntityList(anyList(), any(), any())).thenReturn(List.of(new Parties()));
        when(partiesDao.updateEntityFromOtherEntity(anyList(), any(), anyString())).thenReturn(List.of(new Parties()));
        when(containerV3Service.createBulk(any(), any())).thenReturn(new BulkContainerResponse());
        doNothing().when(auditLogService).addAuditLog(any());

        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsV3Response.class))).thenReturn(mockShipmentResponse);

        ShipmentDetailsV3Response actualResponse = shipmentServiceImplV3.create(commonRequestModel);

        assertNotNull(actualResponse);
        assertEquals(mockShipmentResponse, actualResponse);
    }

    @Test
    void testCreate_successWithOriginQuoteAndSeaTransport() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(true);
        ShipmentDetails mockShipment = testShipment;
        testShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        testShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        mockShipment.setMasterBill(null);
        AdditionalDetails additionalDetails = getMockAdditionalDetails(LocalDateTime.now(), true, true, true);
        CarrierDetails mockCarrierDetails = mockShipment.getCarrierDetails();
        mockCarrierDetails.setEta(LocalDateTime.now());
        mockCarrierDetails.setEtd(LocalDateTime.now());
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setContractId("DPWQ-124");
        mockShipment.setCurrentPartyForQuote("CLIENT");
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ListContractResponse listContractResponse = getMockListContractResponse();
        ListContractResponse.ContractResponse contractResponse = listContractResponse.getContracts().get(0);
        contractResponse.setLoad_types(List.of("LCL"));

        ShipmentV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentV3Request.class);
        List<Events> eventsList = List.of(Events.builder()
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .eventCode("eventType").build());

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsV3Response mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsV3Response.class);
        mockShipmentRequest.setIsChargableEditable(true);
        mockShipmentRequest.getClient().setOrgCode("FRC1234");
        commonRequestModel.setData(mockShipmentRequest);
        mockShipment.setId(2L);

        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        when(jsonHelper.convertValue(any(), eq(ListContractResponse.class))).thenReturn(listContractResponse);
        DependentServiceResponse mockResponse = new DependentServiceResponse();
        mockResponse.setData(getMockListContractResponse());
        when(npmServiceAdapter.fetchContract(any())).thenReturn(ResponseEntity.ok(mockResponse));
        mockShipmentSettings();
        doNothing().when(shipmentValidationV3Util).validateStaleShipmentUpdateError(any(), anyBoolean());
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        doNothing().when(shipmentValidationV3Util).processDGValidations(any(), any(), any());
        mockTenantSettings();
        when(shipmentsV3Util.generateShipmentId(any())).thenReturn("ShipmentId");
        when(shipmentDao.save(any(), anyBoolean())).thenReturn(mockShipment);
        doNothing().when(dateTimeChangeLogService).createEntryFromShipment(any(), any());
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        when(eventsV3Util.createOrUpdateEvents(any(), any(), anyList(), anyBoolean())).thenReturn(eventsList);
        doNothing().when(commonUtils).updateEventWithMasterData(anyList());
        when(eventDao.updateEntityFromOtherEntity(anyList(), any(), anyString())).thenReturn(eventsList);
        doNothing().when(eventsV3Service).updateAtaAtdInShipment(anyList(), any(), any());
        when(commonUtils.convertToEntityList(anyList(), any(), any())).thenReturn(List.of(new Parties()));
        when(partiesDao.updateEntityFromOtherEntity(anyList(), any(), anyString())).thenReturn(List.of(new Parties()));
        BulkPackingResponse bulkPackingResponse = new BulkPackingResponse();
        bulkPackingResponse.setPackingResponseList(List.of(new PackingResponse()));
        when(packingV3Service.updateBulk(any(), any())).thenReturn(bulkPackingResponse);
        doNothing().when(auditLogService).addAuditLog(any());

        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsV3Response.class))).thenReturn(mockShipmentResponse);

        ShipmentDetailsV3Response actualResponse = shipmentServiceImplV3.create(commonRequestModel);

        assertNotNull(actualResponse);
        assertEquals(mockShipmentResponse, actualResponse);
    }

    private ListContractResponse getMockListContractResponse() {
        EntityTransferCarrier evergreenCarrier = EntityTransferCarrier.builder()
                .ItemValue("EVERGREEN LINE")
                .ItemDescription("EVERGREEN LINE")
                .ValuenDesc("EVERGREEN LINE (EVERGREEN LINE)")
                .Identifier1("EGLV")
                .IATACode("22")
                .AirlineCode("111")
                .HasSeaPort(true)
                .HasAirPort(true)
                .ValuenDescAir("EVERGREEN LINE (22)")
                .build();

        UnlocationsResponse originUnloc = UnlocationsResponse.builder()
                .id(138013)
                .country("EG")
                .name("Damietta")
                .portName("Damietta")
                .locCode("EGDAM")
                .nameWoDiacritics("DAMIETTA")
                .locationsReferenceGUID("EGDAM_POR")
                .lookupDescAir("DAMIETTA")
                .lookupDescSea("DAMIETTA - EGDAM, Damietta")
                .countryName("Egypt")
                .hasSeaPort(true)
                .hasAirport(false)
                .build();

        UnlocationsResponse destinationUnloc = UnlocationsResponse.builder()
                .id(138078)
                .country("GB")
                .name("Felixstowe Port")
                .portName("Felixstowe Port")
                .locCode("GBFXT")
                .nameWoDiacritics("Felixstowe Port")
                .locationsReferenceGUID("GBFXT_POR")
                .lookupDescAir("Felixstowe Port")
                .lookupDescSea("Felixstowe Port - GBFXT, Felixstowe Port")
                .countryName("United Kingdom")
                .hasSeaPort(true)
                .hasAirport(false)
                .build();

        Map<String, UnlocationsResponse> unlocMasterData = new HashMap<>();
        unlocMasterData.put("EGDAM_POR", originUnloc);
        unlocMasterData.put("GBFXT_POR", destinationUnloc);

        ListContractResponse.FilterParams filterParams = ListContractResponse.FilterParams.builder()
                .load_type(List.of("FCL"))
                .cargo_type(List.of("20GP", "BAG"))
                .commodity(List.of("FAK"))
                .build();

        ListContractResponse.ContractUsage contractUsage = ListContractResponse.ContractUsage.builder()
                .usage_id("0e4d51fb-b526-435a-9a4f-a92d63453fb6")
                .filter_params(filterParams)
                .usage(1L)
                .usage_uom("unit")
                .meta(ListContractResponse.ContractUsageMeta.builder()
                        .original_usage(1L)
                        .original_usage_uom("unit")
                        .load_attributes(ListContractResponse.LoadAttributes.builder()
                                .weight(BigDecimal.ONE)
                                .quantity(1L)
                                .weight_uom("KG")
                                .quantity_uom("unit")
                                .dimensions(ListContractResponse.Dimensions.builder().build())
                                .build())
                        .build())
                .build();

        ListContractResponse.Route route1 = ListContractResponse.Route.builder()
                .type("NODE")
                .node(ListContractResponse.RouteInfo.builder().code("EGDAM_POR").build())
                .build();

        ListContractResponse.Route route2 = ListContractResponse.Route.builder()
                .type("LEG")
                .origin(ListContractResponse.RouteInfo.builder().code("EGDAM_POR").build())
                .destination(ListContractResponse.RouteInfo.builder().code("GBFXT_POR").build())
                .build();

        ListContractResponse.Route route3 = ListContractResponse.Route.builder()
                .type("NODE")
                .node(ListContractResponse.RouteInfo.builder().code("GBFXT_POR").build())
                .build();

        ListContractResponse.Meta meta = ListContractResponse.Meta.builder()
                .pol("EGDAM_POR")
                .pod("GBFXT_POR")
                .route(List.of(route1, route2, route3))
                .incoterm("EXW")
                .service_mode("P2P")
                .payment_terms("PPD")
                .additional_info("NA")
                .mode_of_transport("SEA")
                .shipment_movement("EXP")
                .branch_info(ListContractResponse.BranchInfo.builder()
                        .id("GBLHR")
                        .country("United Kingdom")
                        .build())
                .minTransitHours("0")
                .maxTransitHours("360")
                .build();

        ListContractResponse.ContractResponse contract = ListContractResponse.ContractResponse.builder()
                ._id("ea0bfbae-57a2-4eb4-a0f9-7c6b1e2392a1")
                .contract_id("DPWQ-399521-110033")
                .parent_contract_id("DPWQ-966695-109218")
                .contract_type("MULTI_USAGE")
                .source("QUOTE_MODULE")
                .source_type("QUOTE_MODULE")
                .customer_org_id("FRC00003242")
                .origin("EGDAM_POR")
                .destination("GBFXT_POR")
                .valid_from(LocalDateTime.of(2025, 6, 10, 0, 0))
                .valid_till(LocalDateTime.of(2025, 6, 24, 0, 0))
                .product_type("EXIM")
                .state("ENABLED")
                .tenant_id("0cc13a27-6a08-4c0c-b45a-596c5af46df2")
                .offer_type("ONLY_CONTRACT_OFFERS")
                .via_nodes(Collections.singletonList(null))
                .load_types(List.of("FCL"))
                .cargo_types(List.of("20GP"))
                .commodities(List.of("FAK"))
                .carrier_codes(List.of("EVERGREEN LINE"))
                .cha("ANY")
                .forwarder("ANY")
                .bco("ANY")
                .cycle("ANY")
                .meta(meta)
                .createdAt(LocalDateTime.of(2025, 6, 10, 5, 55, 19, 927000000))
                .updatedAt(LocalDateTime.of(2025, 6, 10, 5, 55, 19, 927000000))
                .contract_usage(List.of(contractUsage))
                .origin_name("Damietta")
                .destination_name("Felixstowe Port")
                .build();

        return ListContractResponse.builder()
                .contracts(List.of(contract))
                .count(1L)
                .unlocMasterData(unlocMasterData)
                .carrierMasterData(Map.of("EVERGREEN LINE", evergreenCarrier))
                .build();
    }

    @Test
    void testCreate_success2() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(false).setIsNetworkTransferEntityEnabled(true).setIsAutomaticTransferEnabled(true);
        TenantSettingsDetailsContext.getCurrentTenantSettings().setP100Branch(true);
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setMasterBill(null);
        AdditionalDetails additionalDetails = getMockAdditionalDetails(LocalDateTime.now(), true, true, true);
        CarrierDetails mockCarrierDetails = mockShipment.getCarrierDetails();
        mockCarrierDetails.setEta(LocalDateTime.now());
        mockCarrierDetails.setEtd(LocalDateTime.now());
        mockShipment.setId(1L);
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        mockShipment.setShipmentAddresses(null);
        mockShipment.setTruckDriverDetails(new ArrayList<>());
        mockShipment.setReferenceNumbersList(new ArrayList<>());
        mockShipment.setConsolidationList(Set.of(testConsol));
        mockShipment.setSourceTenantId(null);

        ShipmentV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentV3Request.class);
        List<Events> eventsList = List.of(Events.builder()
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .eventCode("eventType").build());

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsV3Response mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsV3Response.class);
        commonRequestModel.setData(mockShipmentRequest);

        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipmentSettings();
        doNothing().when(shipmentValidationV3Util).validateStaleShipmentUpdateError(any(), anyBoolean());
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        doNothing().when(shipmentValidationV3Util).processDGValidations(any(), any(), any());
        mockTenantSettings();
        when(shipmentsV3Util.generateShipmentId(any())).thenReturn("ShipmentId");
        when(shipmentDao.save(any(), anyBoolean())).thenReturn(mockShipment);
        doNothing().when(dateTimeChangeLogService).createEntryFromShipment(any(), any());
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        when(eventsV3Util.createOrUpdateEvents(any(), any(), anyList(), anyBoolean())).thenReturn(eventsList);
        doNothing().when(commonUtils).updateEventWithMasterData(anyList());
        when(eventDao.updateEntityFromOtherEntity(anyList(), any(), anyString())).thenReturn(eventsList);
        doNothing().when(eventsV3Service).updateAtaAtdInShipment(anyList(), any(), any());
        when(commonUtils.convertToEntityList(anyList(), any(), any())).thenReturn(List.of(new Parties()));
        when(truckDriverDetailsDao.updateEntityFromShipment(anyList(), any())).thenReturn(List.of(new TruckDriverDetails()));
        when(referenceNumbersDao.updateEntityFromShipment(anyList(), any())).thenReturn(List.of(new ReferenceNumbers()));
        doNothing().when(auditLogService).addAuditLog(any());

        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsV3Response.class))).thenReturn(mockShipmentResponse);

        ShipmentDetailsV3Response actualResponse = shipmentServiceImplV3.create(commonRequestModel);

        assertNotNull(actualResponse);
        assertEquals(mockShipmentResponse, actualResponse);
    }

    @Test
    void testCreate_exception() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(false).setIsNetworkTransferEntityEnabled(true).setIsAutomaticTransferEnabled(true);
        TenantSettingsDetailsContext.getCurrentTenantSettings().setP100Branch(true);
        ShipmentDetails mockShipment = new ShipmentDetails();
        mockShipment.setGuid(UUID.randomUUID());
        mockShipment.setMasterBill(null);
        mockShipment.setShipmentId("SHIP001");
        mockShipment.setJobType("DRT");

        ShipmentV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentV3Request.class);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        commonRequestModel.setData(mockShipmentRequest);

        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipmentSettings();
        doThrow(ValidationException.class).when(shipmentValidationV3Util).validateStaleShipmentUpdateError(any(), anyBoolean());

        assertThrows(ValidationException.class, () -> shipmentServiceImplV3.create(commonRequestModel));
    }

    @Test
    void testUpdate_success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(true);
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setMasterBill(null);
        mockShipment.setShipmentId("SHIP001");
        AdditionalDetails additionalDetails = getMockAdditionalDetails(LocalDateTime.now(), true, true, true);
        CarrierDetails mockCarrierDetails = mockShipment.getCarrierDetails();
        mockCarrierDetails.setEta(LocalDateTime.now());
        mockCarrierDetails.setEtd(LocalDateTime.now());
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ShipmentV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentV3Request.class);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsV3Response mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsV3Response.class);
        mockShipmentRequest.setIsChargableEditable(true);
        commonRequestModel.setData(mockShipmentRequest);
        mockShipment.setId(2L);

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(mockShipment));
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipmentSettings();
        doNothing().when(shipmentValidationV3Util).validateStaleShipmentUpdateError(any(), anyBoolean());
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        doNothing().when(shipmentValidationV3Util).processDGValidations(any(), any(), any());
        mockTenantSettings();
        when(shipmentDao.update(any(), anyBoolean())).thenReturn(mockShipment);
        doNothing().when(dateTimeChangeLogService).createEntryFromShipment(any(), any());
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        when(commonUtils.convertToEntityList(anyList(), any(), any())).thenReturn(List.of(new Parties()));
        when(partiesDao.updateEntityFromOtherEntity(anyList(), any(), anyString())).thenReturn(List.of(new Parties()));
        doNothing().when(auditLogService).addAuditLog(any());
        when(jsonHelper.convertToJson(any())).thenReturn("Shipment");

        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsV3Response.class))).thenReturn(mockShipmentResponse);

        ShipmentDetailsV3Response actualResponse = shipmentServiceImplV3.completeUpdate(commonRequestModel);

        assertNotNull(actualResponse);
        assertEquals(mockShipmentResponse, actualResponse);
    }

    @Test
    void testUpdate_success_WithContractChanged() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(true);
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setMasterBill(null);
        mockShipment.setShipmentId("SHIP001");
        AdditionalDetails additionalDetails = getMockAdditionalDetails(LocalDateTime.now(), true, true, true);
        CarrierDetails mockCarrierDetails = mockShipment.getCarrierDetails();
        mockCarrierDetails.setEta(LocalDateTime.now());
        mockCarrierDetails.setEtd(LocalDateTime.now());
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setContractId("DPWQ-124");
        mockShipment.setCurrentPartyForQuote("CLIENT");
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        mockShipment.setContractId("DPWQ-6754");
        mockShipment.setCurrentPartyForQuote("CLIENT");

        ShipmentV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentV3Request.class);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsV3Response mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsV3Response.class);
        mockShipmentRequest.setIsChargableEditable(true);
        mockShipmentRequest.setContractId("DPWQ-6754");
        mockShipmentRequest.setCurrentPartyForQuote("CLIENT");
        commonRequestModel.setData(mockShipmentRequest);
        mockShipment.setId(2L);

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(mockShipment));
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipmentSettings();
        doNothing().when(shipmentValidationV3Util).validateStaleShipmentUpdateError(any(), anyBoolean());
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        doNothing().when(shipmentValidationV3Util).processDGValidations(any(), any(), any());
        mockTenantSettings();
        when(shipmentDao.update(any(), anyBoolean())).thenReturn(mockShipment);
        doNothing().when(dateTimeChangeLogService).createEntryFromShipment(any(), any());
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        when(commonUtils.convertToEntityList(anyList(), any(), any())).thenReturn(List.of(new Parties()));
        when(partiesDao.updateEntityFromOtherEntity(anyList(), any(), anyString())).thenReturn(List.of(new Parties()));
        doNothing().when(auditLogService).addAuditLog(any());
        when(jsonHelper.convertToJson(any())).thenReturn("Shipment");

        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsV3Response.class))).thenReturn(mockShipmentResponse);

        ShipmentDetailsV3Response actualResponse = shipmentServiceImplV3.completeUpdate(commonRequestModel);

        assertNotNull(actualResponse);
        assertEquals(mockShipmentResponse, actualResponse);
    }

    @Test
    void testUpdate_success2() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(false).setIsNetworkTransferEntityEnabled(true).setIsAutomaticTransferEnabled(true);
        TenantSettingsDetailsContext.getCurrentTenantSettings().setP100Branch(true);
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setMasterBill(null);
        mockShipment.setShipmentId("SHIP001");
        AdditionalDetails additionalDetails = getMockAdditionalDetails(LocalDateTime.now(), true, true, true);
        CarrierDetails mockCarrierDetails = mockShipment.getCarrierDetails();
        mockCarrierDetails.setEta(LocalDateTime.now());
        mockCarrierDetails.setEtd(LocalDateTime.now());
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        mockShipment.setShipmentAddresses(null);
        mockShipment.setTruckDriverDetails(new ArrayList<>());
        mockShipment.setReferenceNumbersList(new ArrayList<>());
        testConsol.setShipmentsList(new HashSet<>());
        mockShipment.setConsolidationList(Set.of(testConsol));
        mockShipment.setSourceTenantId(null);

        ShipmentV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentV3Request.class);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsV3Response mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsV3Response.class);
        commonRequestModel.setData(mockShipmentRequest);
        mockShipment.setId(2L);

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(mockShipment));
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipmentSettings();
        doNothing().when(shipmentValidationV3Util).validateStaleShipmentUpdateError(any(), anyBoolean());
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        doNothing().when(shipmentValidationV3Util).processDGValidations(any(), any(), any());
        mockTenantSettings();
        when(shipmentDao.update(any(), anyBoolean())).thenReturn(mockShipment);
        doNothing().when(dateTimeChangeLogService).createEntryFromShipment(any(), any());
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        when(commonUtils.convertToEntityList(anyList(), any(), any())).thenReturn(List.of(new Parties()));
        when(truckDriverDetailsDao.updateEntityFromShipment(anyList(), any())).thenReturn(List.of(new TruckDriverDetails()));
        when(referenceNumbersDao.updateEntityFromShipment(anyList(), any())).thenReturn(List.of(new ReferenceNumbers()));
        doNothing().when(auditLogService).addAuditLog(any());
        when(jsonHelper.convertToJson(any())).thenReturn("Shipment");
        when(jsonHelper.convertValue(any(), eq(ShipmentWtVolResponse.class))).thenReturn(ShipmentWtVolResponse.builder().build());
        doNothing().when(consolidationV3Service).updateConsolidationCargoSummary(any(), any());

        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsV3Response.class))).thenReturn(mockShipmentResponse);

        ShipmentDetailsV3Response actualResponse = shipmentServiceImplV3.completeUpdate(commonRequestModel);

        assertNotNull(actualResponse);
        assertEquals(mockShipmentResponse, actualResponse);
    }

    @Test
    void testUpdate_exception() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(false).setIsNetworkTransferEntityEnabled(true).setIsAutomaticTransferEnabled(true);
        TenantSettingsDetailsContext.getCurrentTenantSettings().setP100Branch(true);
        ShipmentDetails mockShipment = new ShipmentDetails();
        mockShipment.setGuid(UUID.randomUUID());
        mockShipment.setMasterBill(null);
        mockShipment.setShipmentId("SHIP001");
        mockShipment.setJobType("DRT");
        testShipment.setJobType("STD");

        ShipmentV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentV3Request.class);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        commonRequestModel.setData(mockShipmentRequest);

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(testShipment));
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipmentSettings();
        when(consoleShipmentMappingDao.countAllStateMappings(any())).thenReturn(2);

        assertThrows(ValidationException.class, () -> shipmentServiceImplV3.completeUpdate(commonRequestModel));
    }

    @Test
    void testAfterSave_whenCreate_shouldProcessAllStepsWithoutAwbUpdate() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(1L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setMasterBill("MBL001");
        newShipment.setAdditionalDetails(new AdditionalDetails());
        ShipmentV3Request shipmentRequest = new ShipmentV3Request();
        shipmentRequest.setShipmentAddresses(List.of(new PartiesRequest()));
        shipmentRequest.setTruckDriverDetails(List.of(new TruckDriverDetailsRequest()));
        shipmentRequest.setReferenceNumbersList(List.of(new ReferenceNumbersRequest()));
        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(true);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        when(commonUtils.convertToEntityList(any(), eq(Parties.class), anyBoolean()))
                .thenReturn(List.of(new Parties()));
        when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), any()))
                .thenReturn(List.of(new Parties()));
        when(commonUtils.convertToEntityList(any(), eq(TruckDriverDetails.class), anyBoolean()))
                .thenReturn(List.of(new TruckDriverDetails()));
        when(truckDriverDetailsDao.updateEntityFromShipment(any(), anyLong()))
                .thenReturn(List.of(new TruckDriverDetails()));
        when(commonUtils.convertToEntityList(any(), eq(ReferenceNumbers.class), anyBoolean()))
                .thenReturn(List.of(new ReferenceNumbers()));
        when(referenceNumbersDao.updateEntityFromShipment(any(), anyLong()))
                .thenReturn(List.of(new ReferenceNumbers()));
        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, null, shipmentRequest, ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData);

        verify(partiesDao).updateEntityFromOtherEntity(any(), anyLong(), any());
        verify(truckDriverDetailsDao).updateEntityFromShipment(any(), anyLong());
        verify(referenceNumbersDao).updateEntityFromShipment(any(), anyLong());
        verify(jsonHelper).convertValue(any(), eq(ShipmentRequest.class));
        verify(dateTimeChangeLogService).createEntryFromShipment(any(), isNull());
    }

    @Test
    void testAfterSave_whenNotCreate_shouldCallAwbUpdate() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setStatus(1);
        newShipment.setId(2L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setTransportMode("AIR");
        newShipment.setMasterBill("NEWMBL");
        oldShipment.setMasterBill("OLDMBL");
        newShipment.setAdditionalDetails(new AdditionalDetails());
        oldShipment.setAdditionalDetails(new AdditionalDetails());
        newShipment.getAdditionalDetails().setEfreightStatus(Constants.EAW);
        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        mockTenantSettings();
        mockShipmentSettings();
        doNothing().when(awbDao).updatedAwbInformationEvent(any(), any());

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, new ShipmentV3Request(), ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData);

        verify(awbDao).updatedAwbInformationEvent(newShipment, oldShipment);
    }

    @Test
    void testAfterSave_shouldSyncConsoleIfConsolidationExists() throws RunnerException {
        TenantSettingsDetailsContext.getCurrentTenantSettings().setEnableAirMessaging(true);
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(5L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setTransportMode("AIR");
        newShipment.setDirection("EXP");
        newShipment.setCarrierDetails(new CarrierDetails());
        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setDirection("IMP");
        oldShipment.setConsolidationList(Set.of(new ConsolidationDetails()));
        oldShipment.setAdditionalDetails(new AdditionalDetails());
        newShipment.setAdditionalDetails(new AdditionalDetails());
        newShipment.getAdditionalDetails().setEfreightStatus(Constants.NON);
        testConsol.setEfreightStatus(null);
        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        ShipmentV3Request shipmentRequest = new ShipmentV3Request();

        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        mockTenantSettings();
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(testConsol));

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, shipmentRequest, ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData);

        assertNull(newShipment.getConsolidationList());
    }

    @Test
    void testAfterSave_withNullLists_shouldSkipOptionalSections() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(3L);
        newShipment.setAdditionalDetails(new AdditionalDetails());
        newShipment.setGuid(UUID.randomUUID());
        ShipmentV3Request request = new ShipmentV3Request();
        request.setShipmentAddresses(null);
        request.setTruckDriverDetails(null);
        request.setReferenceNumbersList(null);
        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(true);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, null, request, ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData);

        verify(partiesDao, never()).updateEntityFromOtherEntity(any(), anyLong(), any());
        verify(truckDriverDetailsDao, never()).updateEntityFromShipment(any(), anyLong());
        verify(referenceNumbersDao, never()).updateEntityFromShipment(any(), anyLong());
    }

    @Test
    void testStoreMblAudit_shouldTriggerAuditWhenMasterBillChanged() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(10L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setMasterBill("NEWMBL");
        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setMasterBill("OLDMBL");
        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        ConsolidationDetailsProjection projection = mock(ConsolidationDetailsProjection.class);
        when(projection.getConsolidationNumber()).thenReturn("C123");
        when(projection.getTenantId()).thenReturn(5);

        when(consolidationDetailsDao.findMblNumberInDifferentTenant(any())).thenReturn(List.of(projection));
        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, new ShipmentV3Request(), ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData);

        verify(auditLogService).addAuditLog(any());
    }

    @Test
    void testUpdateLinkedShipmentData_shouldThrowRunnerException() {
        TenantSettingsDetailsContext.getCurrentTenantSettings().setEnableAirMessaging(true);
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipment.setAdditionalDetails(new AdditionalDetails());
        shipment.getAdditionalDetails().setEfreightStatus(Constants.NON);

        ConsolidationDetails details = new ConsolidationDetails();
        details.setEfreightStatus(Constants.EAW);

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setAdditionalDetails(new AdditionalDetails());
        oldShipment.setConsolidationList(Set.of(details));

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(details));

        assertThrows(RunnerException.class, () -> {
            shipmentServiceImplV3.afterSave(shipment, oldShipment, new ShipmentV3Request(), ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData);
        });
    }

    @Test
    void testAfterSave_whenShipmentCancelled_shouldDeletePendingState() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(4L);
        newShipment.setStatus(ShipmentStatus.Cancelled.getValue());
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setAdditionalDetails(new AdditionalDetails());

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setStatus(ShipmentStatus.Created.getValue());
        oldShipment.setAdditionalDetails(new AdditionalDetails());

        ShipmentV3Request shipmentRequest = new ShipmentV3Request();

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setIsMAWBColoadingEnabled(true);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        mockShipmentSettings();

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, shipmentRequest,
                ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData);

        verify(consoleShipmentMappingDao).deletePendingStateByShipmentId(newShipment.getId());
    }

    @Test
    void testAfterSave_withEvents_shouldProcessEventsCorrectly() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(6L);
        newShipment.setShipmentId("SHIP-001");
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setAdditionalDetails(new AdditionalDetails());

        ShipmentDetails oldShipment = new ShipmentDetails();
        List<Events> eventsList = new ArrayList<>();
        Events event = new Events();
        eventsList.add(event);
        oldShipment.setEventsList(eventsList);
        oldShipment.setAdditionalDetails(new AdditionalDetails());

        ShipmentV3Request shipmentRequest = new ShipmentV3Request();
        ShipmentSettingsDetails shipmentSettings = new ShipmentSettingsDetails();
        shipmentSettings.setAutoEventCreate(true);
        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        when(eventsV3Util.createOrUpdateEvents(any(), any(), any(), anyBoolean()))
                .thenReturn(eventsList);
        when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString()))
                .thenReturn(eventsList);
        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, shipmentRequest,
                shipmentSettings, consoleShipmentData);

        verify(eventsV3Util).createOrUpdateEvents(eq(newShipment), eq(oldShipment), any(), eq(false));
        verify(commonUtils).updateEventWithMasterData(any());
        verify(eventDao).updateEntityFromOtherEntity(any(), eq(newShipment.getId()), eq(Constants.SHIPMENT));
        verify(eventsV3Service).updateAtaAtdInShipment(any(), eq(newShipment), eq(shipmentSettings));
    }

    @Test
    void testAfterSave_withNewShipmentAndAutoEvent_shouldCreateEvent() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(7L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setAdditionalDetails(new AdditionalDetails());

        ShipmentSettingsDetails shipmentSettings = new ShipmentSettingsDetails();
        shipmentSettings.setAutoEventCreate(true);

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(true);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, null, new ShipmentV3Request(),
                shipmentSettings, consoleShipmentData);

        verify(eventsV3Util).autoGenerateCreateEvent(newShipment);
    }

    @Test
    void testAfterSave_withNetworkTransferEnabled_shouldTriggerNetworkTransfer() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(8L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setAdditionalDetails(new AdditionalDetails());

        ShipmentSettingsDetails shipmentSettings = new ShipmentSettingsDetails();
        shipmentSettings.setIsNetworkTransferEntityEnabled(true);
        shipmentSettings.setIsAutomaticTransferEnabled(true);

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(true);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        when(masterDataUtils.withMdc(any())).thenAnswer(invocation -> invocation.getArgument(0));
        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, null, new ShipmentV3Request(),
                shipmentSettings, consoleShipmentData);

        verify(masterDataUtils, times(2)).withMdc(any());
    }

    @Test
    void testAfterSave_withP100Branch_shouldUpdateBookingInPlatform() {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(9L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setAdditionalDetails(new AdditionalDetails());

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(true);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();

        assertDoesNotThrow(() -> shipmentServiceImplV3.afterSave(newShipment, null, new ShipmentV3Request(),
                new ShipmentSettingsDetails(), consoleShipmentData));
    }

    @Test
    void testAfterSave_withDangerousGoods_shouldUpdateConsolidation() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(11L);
        newShipment.setContainsHazardous(true);
        newShipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setAdditionalDetails(new AdditionalDetails());
        newShipment.setDirection("EXP");
        newShipment.setCarrierDetails(new CarrierDetails());

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setContainsHazardous(false);
        oldShipment.setDirection("EXP");
        oldShipment.setAdditionalDetails(new AdditionalDetails());

        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setId(100L);
        consolidation.setHazardous(false);
        Set<ConsolidationDetails> consolidationSet = new HashSet<>();
        consolidationSet.add(consolidation);
        oldShipment.setConsolidationList(consolidationSet);

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidation));
        when(consolidationDetailsDao.updateV3(any(), anyBoolean())).thenReturn(consolidation);

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, new ShipmentV3Request(),
                new ShipmentSettingsDetails(), consoleShipmentData);

        verify(consolidationDetailsDao).updateV3(any(ConsolidationDetails.class), anyBoolean());
        assertTrue(consolidation.getHazardous());
    }

    @Test
    void testAfterSave_withNonDGShipment_shouldUpdateConsolidationToNonDG() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(12L);
        newShipment.setContainsHazardous(false);
        newShipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setAdditionalDetails(new AdditionalDetails());
        newShipment.setDirection("EXP");
        newShipment.setCarrierDetails(new CarrierDetails());

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setContainsHazardous(true);
        oldShipment.setDirection("EXP");
        oldShipment.setAdditionalDetails(new AdditionalDetails());

        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setId(101L);
        consolidation.setHazardous(true);
        Set<ConsolidationDetails> consolidationSet = new HashSet<>();
        consolidationSet.add(consolidation);
        oldShipment.setConsolidationList(consolidationSet);

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(new ArrayList<>());

        mockTenantSettings();
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidation));

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, new ShipmentV3Request(),
                new ShipmentSettingsDetails(), consoleShipmentData);

        assertFalse(consolidation.getHazardous());
    }

    @Test
    void testAfterSave_withSCIT1_shouldUpdateConsolidation() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(13L);
        newShipment.setGuid(UUID.randomUUID());
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setSci(AwbConstants.T1);
        newShipment.setAdditionalDetails(additionalDetails);
        newShipment.setDirection("EXP");
        newShipment.setCarrierDetails(new CarrierDetails());

        ShipmentDetails oldShipment = new ShipmentDetails();
        additionalDetails = new AdditionalDetails();
        additionalDetails.setSci("OTHER");
        oldShipment.setAdditionalDetails(additionalDetails);
        oldShipment.setDirection("EXP");

        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setId(102L);
        consolidation.setSci(null);
        Set<ConsolidationDetails> consolidationSet = new HashSet<>();
        consolidationSet.add(consolidation);
        oldShipment.setConsolidationList(consolidationSet);

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidation));
        when(consolidationDetailsDao.updateV3(any(), anyBoolean())).thenReturn(consolidation);
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(new ArrayList<>());

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, new ShipmentV3Request(),
                new ShipmentSettingsDetails(), consoleShipmentData);

        verify(consolidationDetailsDao).updateV3(any(ConsolidationDetails.class), anyBoolean());
        assertEquals(AwbConstants.T1, consolidation.getSci());
    }

    @Test
    void testAfterSave_withConsolidationAndInterBranchConsoleTrue_shouldNotUpdateAgents() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(14L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setMasterBill("NEW-MBL");

        Parties exportBroker = new Parties();
        exportBroker.setOrgId("1");
        Parties importBroker = new Parties();
        importBroker.setOrgId("2");

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setExportBroker(exportBroker);
        additionalDetails.setImportBroker(importBroker);
        newShipment.setAdditionalDetails(additionalDetails);

        newShipment.setDirection("EXP");
        newShipment.setCarrierDetails(new CarrierDetails());

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setMasterBill("OLD-MBL");
        oldShipment.setAdditionalDetails(new AdditionalDetails());
        oldShipment.setDirection("IMP");

        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setId(103L);
        consolidation.setInterBranchConsole(true);
        consolidation.setSendingAgent(new Parties());
        consolidation.setReceivingAgent(new Parties());

        Set<ConsolidationDetails> consolidationSet = new HashSet<>();
        consolidationSet.add(consolidation);
        oldShipment.setConsolidationList(consolidationSet);

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidation));
        when(consolidationDetailsDao.updateV3(any(), anyBoolean())).thenReturn(consolidation);

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, new ShipmentV3Request(),
                new ShipmentSettingsDetails(), consoleShipmentData);

        verify(consolidationDetailsDao).updateV3(any(ConsolidationDetails.class), any(Boolean.class));

        ArgumentCaptor<ConsolidationDetails> consolidationCaptor = ArgumentCaptor.forClass(ConsolidationDetails.class);
        verify(consolidationDetailsDao).updateV3(consolidationCaptor.capture(), anyBoolean());
        ConsolidationDetails savedConsolidation = consolidationCaptor.getValue();

        assertNotNull(savedConsolidation.getSendingAgent());
        assertNotNull(savedConsolidation.getReceivingAgent());
    }

    @Test
    void testAfterSave_withConsolidationAndShipmentsUpdateAllShipments() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(15L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setMasterBill("UPDATED-MBL");
        newShipment.setDirection("EXP");

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setVoyage("NEW-VOY");
        carrierDetails.setVessel("NEW-VSL");
        carrierDetails.setShippingLine("NEW-LINE");
        newShipment.setCarrierDetails(carrierDetails);

        AdditionalDetails additionalDetails = new AdditionalDetails();
        Parties importBroker = new Parties();
        importBroker.setOrgId("2");
        Parties exportBroker = new Parties();
        exportBroker.setOrgId("1");
        additionalDetails.setImportBroker(importBroker);
        additionalDetails.setExportBroker(exportBroker);
        newShipment.setAdditionalDetails(additionalDetails);

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setMasterBill("OLD-MBL");
        oldShipment.setDirection("IMP");
        oldShipment.setAdditionalDetails(new AdditionalDetails());
        oldShipment.setCarrierDetails(new CarrierDetails());

        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setId(104L);
        consolidation.setInterBranchConsole(false);

        Set<ConsolidationDetails> consolidationSet = new HashSet<>();
        consolidationSet.add(consolidation);
        oldShipment.setConsolidationList(consolidationSet);

        ConsoleShipmentMapping mapping1 = new ConsoleShipmentMapping();
        mapping1.setShipmentId(20L);
        ConsoleShipmentMapping mapping2 = new ConsoleShipmentMapping();
        mapping2.setShipmentId(21L);
        List<ConsoleShipmentMapping> mappings = Arrays.asList(mapping1, mapping2);

        ShipmentDetails shipment1 = new ShipmentDetails();
        shipment1.setId(20L);
        shipment1.setMasterBill("OLD-MBL");
        shipment1.setAdditionalDetails(new AdditionalDetails());
        shipment1.setCarrierDetails(new CarrierDetails());

        ShipmentDetails shipment2 = new ShipmentDetails();
        shipment2.setId(21L);
        shipment2.setMasterBill("OLD-MBL");
        shipment2.setAdditionalDetails(new AdditionalDetails());
        shipment2.setCarrierDetails(new CarrierDetails());

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidation));
        when(consolidationDetailsDao.updateV3(any(), anyBoolean())).thenReturn(consolidation);
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(mappings);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(Arrays.asList(shipment1, shipment2));

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, new ShipmentV3Request(),
                new ShipmentSettingsDetails(), consoleShipmentData);

        verify(shipmentDao).saveAll(any());

        ArgumentCaptor<List<ShipmentDetails>> shipmentListCaptor = ArgumentCaptor.forClass(List.class);
        verify(shipmentDao).saveAll(shipmentListCaptor.capture());
        List<ShipmentDetails> updatedShipments = shipmentListCaptor.getValue();

        assertEquals(2, updatedShipments.size());
        for (ShipmentDetails updated : updatedShipments) {
            assertEquals("UPDATED-MBL", updated.getMasterBill());
            assertEquals("EXP", updated.getDirection());
            assertEquals("NEW-VOY", updated.getCarrierDetails().getVoyage());
            assertEquals("NEW-VSL", updated.getCarrierDetails().getVessel());
            assertEquals("NEW-LINE", updated.getCarrierDetails().getShippingLine());
        }
    }

    @Test
    void testAfterSave_whenNotFromET_shouldPushShipmentDataToDependentService() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(16L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setAdditionalDetails(new AdditionalDetails());

        ShipmentV3Request shipmentRequest = new ShipmentV3Request();
        shipmentRequest.setIsAutoSellRequired(true);

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(true);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, null, shipmentRequest,
                new ShipmentSettingsDetails(), consoleShipmentData);

        verify(dateTimeChangeLogService).createEntryFromShipment(any(), isNull());
    }

    @Test
    void testAfterSave_withAutoEventCreateTrue_shouldGenerateCreateEvent() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(18L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setAdditionalDetails(new AdditionalDetails());

        ShipmentSettingsDetails shipmentSettings = new ShipmentSettingsDetails();
        shipmentSettings.setAutoEventCreate(true);

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(true);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, null, new ShipmentV3Request(),
                shipmentSettings, consoleShipmentData);

        verify(eventsV3Util).autoGenerateCreateEvent(newShipment);
    }

    @Test
    void testUpdateAwb_withNonAirTransportMode_shouldNotCallAwbUpdate() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        ShipmentDetails oldShipment = new ShipmentDetails();
        newShipment.setId(20L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        newShipment.setAdditionalDetails(new AdditionalDetails());
        oldShipment.setAdditionalDetails(new AdditionalDetails());

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, new ShipmentV3Request(),
                ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData);

        verify(awbDao, never()).updatedAwbInformationEvent(any(), any());
    }

    @Test
    void testUpdateAwb_withChangedSecurityStatus_shouldCallAwbUpdate() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        ShipmentDetails oldShipment = new ShipmentDetails();
        newShipment.setId(21L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        newShipment.setSecurityStatus("SECURE");
        oldShipment.setSecurityStatus("NOT_SECURE");
        newShipment.setAdditionalDetails(new AdditionalDetails());
        oldShipment.setAdditionalDetails(new AdditionalDetails());

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();
        mockShipmentSettings();

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, new ShipmentV3Request(),
                ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData);

        verify(awbDao).updatedAwbInformationEvent(newShipment, oldShipment);
    }

    @Test
    void testUpdateAwb_withChangedSci_shouldCallAwbUpdate() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        ShipmentDetails oldShipment = new ShipmentDetails();
        newShipment.setId(22L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        AdditionalDetails newDetails = new AdditionalDetails();
        newDetails.setSci("NEW_SCI");
        newShipment.setAdditionalDetails(newDetails);

        AdditionalDetails oldDetails = new AdditionalDetails();
        oldDetails.setSci("OLD_SCI");
        oldShipment.setAdditionalDetails(oldDetails);

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();
        mockShipmentSettings();

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, new ShipmentV3Request(),
                ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData);

        verify(awbDao).updatedAwbInformationEvent(newShipment, oldShipment);
    }

    @Test
    void testUpdateAwb_withChangedEfreightStatus_shouldCallAwbUpdate() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        ShipmentDetails oldShipment = new ShipmentDetails();
        newShipment.setId(23L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        AdditionalDetails newDetails = new AdditionalDetails();
        newDetails.setEfreightStatus(Constants.EAW);
        newShipment.setAdditionalDetails(newDetails);

        AdditionalDetails oldDetails = new AdditionalDetails();
        oldDetails.setEfreightStatus(Constants.NON);
        oldShipment.setAdditionalDetails(oldDetails);

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();
        mockShipmentSettings();

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, new ShipmentV3Request(),
                ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData);

        verify(awbDao).updatedAwbInformationEvent(newShipment, oldShipment);
    }

    @Test
    void testStoreMblAudit_withEmptyMasterBill_shouldNotQueryDatabase() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(24L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setMasterBill("");
        newShipment.setAdditionalDetails(new AdditionalDetails());

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(true);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, null, new ShipmentV3Request(),
                ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData);

        verify(consolidationDetailsDao, never()).findMblNumberInDifferentTenant(anyString());
    }

    @Test
    void testStoreMblAudit_withExceptionDuringAudit_shouldContinueProcessing() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(25L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setMasterBill("ERROR-MBL");
        newShipment.setAdditionalDetails(new AdditionalDetails());

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(true);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        ConsolidationDetailsProjection projection = mock(ConsolidationDetailsProjection.class);
        when(projection.getConsolidationNumber()).thenReturn("C123");
        when(projection.getTenantId()).thenReturn(5);

        when(consolidationDetailsDao.findMblNumberInDifferentTenant(any())).thenReturn(List.of(projection));
        doThrow(new RuntimeException("Test Exception")).when(auditLogService).addAuditLog(any());
        mockTenantSettings();

        // Should not throw exception
        assertDoesNotThrow(() -> shipmentServiceImplV3.afterSave(newShipment, null, new ShipmentV3Request(),
                ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData));
    }

    @Test
    void testUpdateLinkedShipmentData_withNoConsolidationList_shouldReturnNull() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(26L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setAdditionalDetails(new AdditionalDetails());

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(true);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, null, new ShipmentV3Request(),
                ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData);

        assertNull(newShipment.getConsolidationList());
    }

    @Test
    void testUpdateLinkedShipmentData_withOceanDGShipment_shouldSetHazardousTrue() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(27L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        newShipment.setContainsHazardous(true);
        newShipment.setAdditionalDetails(new AdditionalDetails());

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setContainsHazardous(false);
        oldShipment.setAdditionalDetails(new AdditionalDetails());

        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setId(105L);
        consolidation.setHazardous(false);
        Set<ConsolidationDetails> consolidationSet = new HashSet<>();
        consolidationSet.add(consolidation);
        oldShipment.setConsolidationList(consolidationSet);

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidation));
        when(consolidationDetailsDao.updateV3(any(), anyBoolean())).thenReturn(consolidation);

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, new ShipmentV3Request(),
                ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData);

        assertTrue(consolidation.getHazardous());
    }

    @Test
    void testChangeConsolidationDGValues_whenNullConsolidationList_shouldReturnNull() {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(28L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setAdditionalDetails(new AdditionalDetails());

        AtomicBoolean makeConsoleNonDG = new AtomicBoolean(false);
        AtomicBoolean makeConsoleDG = new AtomicBoolean(false);

        ConsolidationDetails result = shipmentServiceImplV3.changeConsolidationDGValues(makeConsoleDG, makeConsoleNonDG, null, newShipment);

        assertNull(result);
    }

    @Test
    void testCheckIfAllShipmentsAreNonDG_withEmptyList_shouldReturnTrue() {
        List<Long> emptyList = new ArrayList<>();

        boolean result = shipmentServiceImplV3.checkIfAllShipmentsAreNonDG(emptyList);

        assertTrue(result);
    }

    @Test
    void testCheckIfAllShipmentsAreNonDG_withHazardousShipments_shouldReturnFalse() {
        List<Long> shipmentIds = List.of(100L, 101L);
        ShipmentDetails hazardousShipment = new ShipmentDetails();
        hazardousShipment.setContainsHazardous(true);

        when(shipmentDao.findByShipmentIdInAndContainsHazardous(eq(shipmentIds), eq(true)))
                .thenReturn(List.of(hazardousShipment));

        boolean result = shipmentServiceImplV3.checkIfAllShipmentsAreNonDG(shipmentIds);

        assertFalse(result);
    }

    @Test
    void testSaveConsolidationDGValue_withUnchangedValue_shouldReturnNull() {
        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setHazardous(true);

        ConsolidationDetails result = shipmentServiceImplV3.saveConsolidationDGValue(true, consolidation);

        assertNull(result);
    }

    @Test
    void testSaveConsolidationDGValue_withChangedValue_shouldSaveAndReturn() {
        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setHazardous(false);

        when(consolidationDetailsDao.updateV3(any(), anyBoolean())).thenReturn(consolidation);

        ConsolidationDetails result = shipmentServiceImplV3.saveConsolidationDGValue(true, consolidation);

        assertNotNull(result);
        assertTrue(consolidation.getHazardous());
    }

    @Test
    void testGetConsolidationDetails_withProvidedDetails_shouldReturnSame() {
        ConsolidationDetails providedDetails = new ConsolidationDetails();
        providedDetails.setId(106L);

        ConsolidationDetails result = shipmentServiceImplV3.getConsolidationDetails(106L, providedDetails);

        assertSame(providedDetails, result);
        verify(consolidationDetailsDao, never()).findById(anyLong());
    }

    @Test
    void testGetConsolidationDetails_withNullDetails_shouldFindById() {
        ConsolidationDetails expectedDetails = new ConsolidationDetails();
        expectedDetails.setId(107L);

        when(consolidationDetailsDao.findById(107L)).thenReturn(Optional.of(expectedDetails));

        ConsolidationDetails result = shipmentServiceImplV3.getConsolidationDetails(107L, null);

        assertSame(expectedDetails, result);
        verify(consolidationDetailsDao).findById(107L);
    }

    @Test
    void testGetConsolidationDetails_withMissingDetails_shouldThrowException() {
        when(consolidationDetailsDao.findById(108L)).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentServiceImplV3.getConsolidationDetails(108L, null);
        });
    }

    @Test
    void testSetEventDetails_withEmptyList_shouldReturnSameList() {
        List<Events> emptyList = new ArrayList<>();
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setShipmentId("SHIP-001");

        List<Events> result = shipmentServiceImplV3.setEventDetails(emptyList, shipment);

        assertSame(emptyList, result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testSetEventDetails_withEvents_shouldSetShipmentNumber() {
        Events event1 = new Events();
        Events event2 = new Events();
        List<Events> eventsList = Arrays.asList(event1, event2);

        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setShipmentId("SHIP-002");

        List<Events> result = shipmentServiceImplV3.setEventDetails(eventsList, shipment);

        for (Events event : result) {
            assertEquals("SHIP-002", event.getShipmentNumber());
        }
    }

    @Test
    void testProcessLinkedConsolidationDetails_withCarrierDetailsNull_shouldCreateNew() throws RunnerException {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setId(30L);
        shipment.setMasterBill("TEST-MBL");
        shipment.setDirection("EXP");
        shipment.setCarrierDetails(new CarrierDetails());
        shipment.setAdditionalDetails(new AdditionalDetails());

        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setId(109L);
        consolidation.setCarrierDetails(null);

        Set<ConsolidationDetails> consolidationSet = new HashSet<>();
        consolidationSet.add(consolidation);

        when(consolidationDetailsDao.findById(109L)).thenReturn(Optional.of(consolidation));

        AtomicBoolean makeConsoleNonDG = new AtomicBoolean(false);
        AtomicBoolean makeConsoleSciT1 = new AtomicBoolean(false);
        AtomicBoolean makeConsoleDG = new AtomicBoolean(false);

        ConsolidationDetails result = shipmentServiceImplV3.processLinkedConsolidationDetails(
                shipment, null, consolidationSet, makeConsoleDG, makeConsoleNonDG, makeConsoleSciT1);

        assertNotNull(result.getCarrierDetails());
        assertEquals(shipment.getMasterBill(), result.getBol());
        assertEquals(shipment.getDirection(), result.getShipmentType());
    }

    @Test
    void testCheckConsoleSciUpdateT1_withNullAdditionalDetails_shouldReturnFalse() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setAdditionalDetails(null);

        boolean result = shipmentServiceImplV3.checkConsoleSciUpdateT1(shipment, null);

        assertFalse(result);
    }

    @Test
    void testCheckConsoleSciUpdateT1_withNullSci_shouldReturnFalse() {
        ShipmentDetails shipment = new ShipmentDetails();
        AdditionalDetails details = new AdditionalDetails();
        details.setSci(null);
        shipment.setAdditionalDetails(details);

        boolean result = shipmentServiceImplV3.checkConsoleSciUpdateT1(shipment, null);

        assertFalse(result);
    }

    @Test
    void testCheckConsoleSciUpdateT1_withNonT1Sci_shouldReturnFalse() {
        ShipmentDetails shipment = new ShipmentDetails();
        AdditionalDetails details = new AdditionalDetails();
        details.setSci("NOT-T1");
        shipment.setAdditionalDetails(details);

        boolean result = shipmentServiceImplV3.checkConsoleSciUpdateT1(shipment, null);

        assertFalse(result);
    }

    @Test
    void testCheckConsoleSciUpdateT1_withT1SciAndNoOldEntity_shouldReturnTrue() {
        ShipmentDetails shipment = new ShipmentDetails();
        AdditionalDetails details = new AdditionalDetails();
        details.setSci(AwbConstants.T1);
        shipment.setAdditionalDetails(details);

        boolean result = shipmentServiceImplV3.checkConsoleSciUpdateT1(shipment, null);

        assertTrue(result);
    }

    @Test
    void testCheckConsoleSciUpdateT1_withT1SciAndSameOldSci_shouldReturnFalse() {
        ShipmentDetails shipment = new ShipmentDetails();
        AdditionalDetails newDetails = new AdditionalDetails();
        newDetails.setSci(AwbConstants.T1);
        shipment.setAdditionalDetails(newDetails);

        ShipmentDetails oldShipment = new ShipmentDetails();
        AdditionalDetails oldDetails = new AdditionalDetails();
        oldDetails.setSci(AwbConstants.T1);
        oldShipment.setAdditionalDetails(oldDetails);

        boolean result = shipmentServiceImplV3.checkConsoleSciUpdateT1(shipment, oldShipment);

        assertFalse(result);
    }

    @Test
    void testCheckConsoleSciUpdateT1_withT1SciAndDifferentOldSci_shouldReturnTrue() {
        ShipmentDetails shipment = new ShipmentDetails();
        AdditionalDetails newDetails = new AdditionalDetails();
        newDetails.setSci(AwbConstants.T1);
        shipment.setAdditionalDetails(newDetails);

        ShipmentDetails oldShipment = new ShipmentDetails();
        AdditionalDetails oldDetails = new AdditionalDetails();
        oldDetails.setSci("NOT-T1");
        oldShipment.setAdditionalDetails(oldDetails);

        boolean result = shipmentServiceImplV3.checkConsoleSciUpdateT1(shipment, oldShipment);

        assertTrue(result);
    }

    @Test
    void testIsExportOrImportBrokerPresent_withNullBrokers_shouldReturnFalse() {
        ShipmentDetails shipment = new ShipmentDetails();
        AdditionalDetails details = new AdditionalDetails();
        details.setExportBroker(null);
        details.setImportBroker(null);
        shipment.setAdditionalDetails(details);

        boolean result = shipmentServiceImplV3.isExportOrImportBrokerPresent(shipment);

        assertFalse(result);
    }

    @Test
    void testAddAdditionalDetailsForShipment_withNullAdditionalDetails_shouldCreateNew() {
        ShipmentDetails source = new ShipmentDetails();
        AdditionalDetails sourceDetails = new AdditionalDetails();
        Parties exportBroker = new Parties();
        Parties importBroker = new Parties();
        sourceDetails.setExportBroker(exportBroker);
        sourceDetails.setImportBroker(importBroker);
        source.setAdditionalDetails(sourceDetails);

        ShipmentDetails target = new ShipmentDetails();
        target.setAdditionalDetails(null);

        when(commonUtils.removeIdFromParty(any())).thenReturn(new Parties());

        shipmentServiceImplV3.addAdditionalDetailsForShipment(source, target);

        assertNotNull(target.getAdditionalDetails());
        assertNotNull(target.getAdditionalDetails().getExportBroker());
        assertNotNull(target.getAdditionalDetails().getImportBroker());
    }

    @Test
    void testAddAdditionalDetailsForShipment_withSameExportAndImportBroker_shouldNotUpdate() {
        Parties broker = new Parties();
        broker.setOrgId("1");
        ShipmentDetails source = new ShipmentDetails();
        AdditionalDetails sourceDetails = new AdditionalDetails();
        sourceDetails.setExportBroker(broker);
        sourceDetails.setImportBroker(broker);
        source.setAdditionalDetails(sourceDetails);

        ShipmentDetails target = new ShipmentDetails();
        AdditionalDetails targetDetails = new AdditionalDetails();
        targetDetails.setExportBroker(broker);
        targetDetails.setImportBroker(broker);
        target.setAdditionalDetails(targetDetails);

        shipmentServiceImplV3.addAdditionalDetailsForShipment(source, target);

        // Since brokers are the same, they should not be updated
        assertSame(broker, target.getAdditionalDetails().getExportBroker());
        assertSame(broker, target.getAdditionalDetails().getImportBroker());
    }

    @Test
    void testAddAdditionalDetailsForShipment_withDifferentExportBroker_shouldUpdateOnlyExport() {
        Parties exportSource = new Parties();
        exportSource.setOrgId("1");
        Parties exportTarget = new Parties(); // Different from source
        exportTarget.setOrgId("2");
        Parties importBroker = new Parties();
        importBroker.setOrgId("3");

        ShipmentDetails source = new ShipmentDetails();
        AdditionalDetails sourceDetails = new AdditionalDetails();
        sourceDetails.setExportBroker(exportSource);
        sourceDetails.setImportBroker(importBroker);
        source.setAdditionalDetails(sourceDetails);

        ShipmentDetails target = new ShipmentDetails();
        AdditionalDetails targetDetails = new AdditionalDetails();
        targetDetails.setExportBroker(exportTarget); // Different than source
        targetDetails.setImportBroker(importBroker); // Same as source
        target.setAdditionalDetails(targetDetails);

        when(commonUtils.removeIdFromParty(exportSource)).thenReturn(new Parties());

        shipmentServiceImplV3.addAdditionalDetailsForShipment(source, target);

        assertNotNull(target.getAdditionalDetails().getExportBroker());
        assertSame(importBroker, target.getAdditionalDetails().getImportBroker()); // Should remain unchanged
    }

    @Test
    void testAddAdditionalDetailsForShipment_withDifferentImportBroker_shouldUpdateOnlyImport() {
        Parties importSource = new Parties();
        importSource.setOrgId("1");
        Parties importTarget = new Parties(); // Different from source
        importTarget.setOrgId("2");
        Parties exportBroker = new Parties();
        exportBroker.setOrgId("3");

        ShipmentDetails source = new ShipmentDetails();
        AdditionalDetails sourceDetails = new AdditionalDetails();
        sourceDetails.setExportBroker(exportBroker);
        sourceDetails.setImportBroker(importSource);
        source.setAdditionalDetails(sourceDetails);

        ShipmentDetails target = new ShipmentDetails();
        AdditionalDetails targetDetails = new AdditionalDetails();
        targetDetails.setExportBroker(exportBroker); // Same as source
        targetDetails.setImportBroker(importTarget); // Different than source
        target.setAdditionalDetails(targetDetails);

        when(commonUtils.removeIdFromParty(importSource)).thenReturn(new Parties());

        shipmentServiceImplV3.addAdditionalDetailsForShipment(source, target);

        assertSame(exportBroker, target.getAdditionalDetails().getExportBroker());
        assertNotNull(target.getAdditionalDetails().getImportBroker());
    }

    @Test
    void testCheckOriginalPrintedForJobTypeChange_withNullOldEntity_shouldReturnFalse() {
        ShipmentDetails shipment = new ShipmentDetails();

        boolean result = shipmentServiceImplV3.checkOriginalPrintedForJobTypeChange(shipment, null);

        assertFalse(result);
    }

    @Test
    void testCheckOriginalPrintedForJobTypeChange_withNonSeaTransport_shouldReturnFalse() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        ShipmentDetails oldShipment = new ShipmentDetails();

        boolean result = shipmentServiceImplV3.checkOriginalPrintedForJobTypeChange(shipment, oldShipment);

        assertFalse(result);
    }

    @Test
    void testCheckOriginalPrintedForJobTypeChange_withNonExportDirection_shouldReturnFalse() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setDirection(Constants.DIRECTION_IMP);
        ShipmentDetails oldShipment = new ShipmentDetails();

        boolean result = shipmentServiceImplV3.checkOriginalPrintedForJobTypeChange(shipment, oldShipment);

        assertFalse(result);
    }

    @Test
    void testCheckOriginalPrintedForJobTypeChange_withFalsePrintedOriginal_shouldReturnFalse() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setDirection(Constants.DIRECTION_EXP);
        AdditionalDetails details = new AdditionalDetails();
        details.setPrintedOriginal(false);
        shipment.setAdditionalDetails(details);
        ShipmentDetails oldShipment = new ShipmentDetails();

        boolean result = shipmentServiceImplV3.checkOriginalPrintedForJobTypeChange(shipment, oldShipment);

        assertFalse(result);
    }

    @Test
    void testCheckOriginalPrintedForJobTypeChange_withSameJobType_shouldReturnFalse() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setDirection(Constants.DIRECTION_EXP);
        shipment.setJobType("JOB-TYPE");
        AdditionalDetails details = new AdditionalDetails();
        details.setPrintedOriginal(true);
        shipment.setAdditionalDetails(details);

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setJobType("JOB-TYPE");

        boolean result = shipmentServiceImplV3.checkOriginalPrintedForJobTypeChange(shipment, oldShipment);

        assertFalse(result);
    }

    @Test
    void testCheckOriginalPrintedForJobTypeChange_withAllConditionsMet_shouldReturnTrue() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setDirection(Constants.DIRECTION_EXP);
        shipment.setJobType("NEW-JOB-TYPE");
        AdditionalDetails details = new AdditionalDetails();
        details.setPrintedOriginal(true);
        shipment.setAdditionalDetails(details);

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setJobType("OLD-JOB-TYPE");

        boolean result = shipmentServiceImplV3.checkOriginalPrintedForJobTypeChange(shipment, oldShipment);

        assertTrue(result);
    }

    @Test
    void testCheckDisableFetchConditionForAwb_withNullOldEntity_shouldReturnFalse() {
        ShipmentDetails shipment = new ShipmentDetails();
        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();

        boolean result = shipmentServiceImplV3.checkDisableFetchConditionForAwb(shipment, null, settings);

        assertFalse(result);
    }

    @Test
    void testCheckDisableFetchConditionForAwb_withFalseIataTactFlag_shouldReturnFalse() {
        ShipmentDetails shipment = new ShipmentDetails();
        ShipmentDetails oldShipment = new ShipmentDetails();
        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setIataTactFlag(false);

        boolean result = shipmentServiceImplV3.checkDisableFetchConditionForAwb(shipment, oldShipment, settings);

        assertFalse(result);
    }

    @Test
    void testCheckDisableFetchConditionForAwb_withNonAirTransport_shouldReturnFalse() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        ShipmentDetails oldShipment = new ShipmentDetails();
        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setIataTactFlag(true);

        boolean result = shipmentServiceImplV3.checkDisableFetchConditionForAwb(shipment, oldShipment, settings);

        assertFalse(result);
    }

    @Test
    void testCheckDisableFetchConditionForAwb_withNonDRTJobType_shouldReturnFalse() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipment.setJobType("NOT-DRT");
        ShipmentDetails oldShipment = new ShipmentDetails();
        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setIataTactFlag(true);

        boolean result = shipmentServiceImplV3.checkDisableFetchConditionForAwb(shipment, oldShipment, settings);

        assertFalse(result);
    }

    @Test
    void testCheckDisableFetchConditionForAwb_withSamePortsAndShippingLine_shouldReturnFalse() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipment.setJobType(Constants.SHIPMENT_TYPE_DRT);
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOriginPort("PORT-A");
        carrierDetails.setDestinationPort("PORT-B");
        carrierDetails.setShippingLine("LINE-C");
        shipment.setCarrierDetails(carrierDetails);

        ShipmentDetails oldShipment = new ShipmentDetails();
        CarrierDetails oldCarrierDetails = new CarrierDetails();
        oldCarrierDetails.setOriginPort("PORT-A");
        oldCarrierDetails.setDestinationPort("PORT-B");
        oldCarrierDetails.setShippingLine("LINE-C");
        oldShipment.setCarrierDetails(oldCarrierDetails);

        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setIataTactFlag(true);

        boolean result = shipmentServiceImplV3.checkDisableFetchConditionForAwb(shipment, oldShipment, settings);

        assertFalse(result);
    }

    @Test
    void testCheckDisableFetchConditionForAwb_withChangedPort_shouldReturnTrue() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipment.setJobType(Constants.SHIPMENT_TYPE_DRT);
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOriginPort("NEW-PORT-A");
        carrierDetails.setDestinationPort("PORT-B");
        carrierDetails.setShippingLine("LINE-C");
        shipment.setCarrierDetails(carrierDetails);

        ShipmentDetails oldShipment = new ShipmentDetails();
        CarrierDetails oldCarrierDetails = new CarrierDetails();
        oldCarrierDetails.setOriginPort("OLD-PORT-A");
        oldCarrierDetails.setDestinationPort("PORT-B");
        oldCarrierDetails.setShippingLine("LINE-C");
        oldShipment.setCarrierDetails(oldCarrierDetails);

        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setIataTactFlag(true);

        boolean result = shipmentServiceImplV3.checkDisableFetchConditionForAwb(shipment, oldShipment, settings);

        assertTrue(result);
    }

    @Test
    void testUpdateAwbForDisableFetchConditionForAwb_whenConditionMet_shouldUpdateAwb() throws RunnerException {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setId(31L);
        shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipment.setJobType(Constants.SHIPMENT_TYPE_DRT);
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOriginPort("NEW-PORT");
        shipment.setCarrierDetails(carrierDetails);

        ShipmentDetails oldShipment = new ShipmentDetails();
        CarrierDetails oldCarrierDetails = new CarrierDetails();
        oldCarrierDetails.setOriginPort("OLD-PORT");
        oldShipment.setCarrierDetails(oldCarrierDetails);

        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setIataTactFlag(true);

        // Mock Awb setup
        Awb awb = new Awb();
        List<AwbGoodsDescriptionInfo> goodsInfo = new ArrayList<>();
        AwbGoodsDescriptionInfo info1 = new AwbGoodsDescriptionInfo();
        goodsInfo.add(info1);
        awb.setAwbGoodsDescriptionInfo(goodsInfo);

        when(awbDao.findByShipmentId(31L)).thenReturn(List.of(awb));

        shipmentServiceImplV3.updateAwbForDisableFetchConditionForAwb(shipment, oldShipment, settings);

        verify(awbDao).save(awb);
    }

    @Test
    void testProcessBranchesAndPartner_shouldSetNullForZeroValues() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setReceivingBranch(0L);
        shipment.setDocumentationPartner(0L);

        // Setup triangulation partner
        TriangulationPartner triangulationPartner = new TriangulationPartner();
        triangulationPartner.setTriangulationPartner(0L);
        List<TriangulationPartner> partners = new ArrayList<>();
        partners.add(triangulationPartner);
        shipment.setTriangulationPartnerList(partners);

        shipmentServiceImplV3.processBranchesAndPartner(shipment);

        assertNull(shipment.getReceivingBranch());
        assertNull(shipment.getDocumentationPartner());
        assertNull(shipment.getTriangulationPartnerList());
    }

    @Test
    void testProcessBranchesAndPartner_withZeroTriangulationPartner_shouldSetNull() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTriangulationPartner(0L);
        shipment.setTriangulationPartnerList(null);

        shipmentServiceImplV3.processBranchesAndPartner(shipment);

        assertNull(shipment.getTriangulationPartner());
    }

    @Test
    void testValidateBeforeSave_withSameConsignerConsignee_shouldThrowException() {
        ShipmentDetails shipment = new ShipmentDetails();

        Parties consigner = new Parties();
        consigner.setOrgCode("SAME-CODE");
        shipment.setConsigner(consigner);

        Parties consignee = new Parties();
        consignee.setOrgCode("SAME-CODE");
        shipment.setConsignee(consignee);

        assertThrows(ValidationException.class, () -> {
            shipmentServiceImplV3.validateBeforeSave(shipment, null);
        });
    }

    @Test
    void testValidateBeforeSave_withDRTAndNonAirSeaTransport_shouldSetHouseBill() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipment.setTransportMode("ROAD");
        shipment.setMasterBill("MASTER-BILL");

        shipmentServiceImplV3.validateBeforeSave(shipment, null);

        assertEquals("MASTER-BILL", shipment.getHouseBill());
    }

    @Test
    void testValidateBeforeSave_withNonDRTJobType_shouldNotChangeHouseBill() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setJobType("OTHER_TYPE");
        shipment.setTransportMode("ROAD");
        shipment.setMasterBill("MASTER");
        shipment.setHouseBill("ORIGINAL");

        shipmentServiceImplV3.validateBeforeSave(shipment, null);

        assertEquals("ORIGINAL", shipment.getHouseBill()); // unchanged
    }

    @Test
    void testValidateBeforeSave_withDRTAndNullTransportMode_shouldSkipHouseBillUpdate() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipment.setTransportMode(null);
        shipment.setMasterBill("MASTER-BILL");
        shipment.setHouseBill("HOUSE-BILL");

        shipmentServiceImplV3.validateBeforeSave(shipment, null);

        assertEquals("HOUSE-BILL", shipment.getHouseBill()); // unchanged
    }

    @Test
    void testValidateBeforeSave_withDRTAndAirTransport_shouldSetNullHouseBill() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipment.setMasterBill("MASTER-BILL");
        shipment.setHouseBill("HOUSE-BILL");

        shipmentServiceImplV3.validateBeforeSave(shipment, null);

        assertNull(shipment.getHouseBill());
    }

    @Test
    void testValidateBeforeSave_withNullConsignee_shouldNotThrow() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipment.setJobType(Constants.SHIPMENT_TYPE_STD);
        shipment.setConsigner(new Parties());
        shipment.setConsignee(null); // Consignee is null

        assertDoesNotThrow(() -> shipmentServiceImplV3.validateBeforeSave(shipment, null));
    }

    @Test
    void testValidateBeforeSave_withNullJobType_shouldNotChangeHouseBill() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setJobType(null); // JobType is null
        shipment.setTransportMode("ROAD");
        shipment.setMasterBill("MASTER");
        shipment.setHouseBill("EXISTING");

        shipmentServiceImplV3.validateBeforeSave(shipment, null);

        assertEquals("EXISTING", shipment.getHouseBill());
    }

    @Test
    void testValidateBeforeSave_withEmptyTransportMode_shouldNotChangeHouseBill() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipment.setTransportMode(""); // Empty transport mode
        shipment.setMasterBill("MASTER");
        shipment.setHouseBill("EXISTING");

        shipmentServiceImplV3.validateBeforeSave(shipment, null);

        assertEquals("EXISTING", shipment.getHouseBill());
    }

    @Test
    void testValidateBeforeSave_withNullTransportMode_shouldNotChangeHouseBill() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipment.setTransportMode(null); // Null transport mode
        shipment.setMasterBill("MASTER");
        shipment.setHouseBill("EXISTING");

        shipmentServiceImplV3.validateBeforeSave(shipment, null);

        assertEquals("EXISTING", shipment.getHouseBill());
    }

    @Test
    void testValidateBeforeSave_withDRTAndUnknownTransportMode_shouldSetHouseBill() {
        ShipmentDetails details = new ShipmentDetails();
        details.setJobType(Constants.SHIPMENT_TYPE_DRT);
        details.setTransportMode("AIRSEA"); // not AIR, not SEA, not null
        details.setMasterBill("MBL123");

        shipmentServiceImplV3.validateBeforeSave(details, null);

        assertEquals("MBL123", details.getHouseBill());
    }

    @Test
    void testIsDiffPresentInOldNewShipment_withDifferentMasterBill_shouldReturnTrue() {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setMasterBill("NEW-MBL");

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setMasterBill("OLD-MBL");

        boolean result = shipmentServiceImplV3.isDiffPresentInOldNewShipment(newShipment, oldShipment);

        assertTrue(result);
    }

    @Test
    void testIsDiffPresentInOldNewShipment_withDifferentSci_shouldReturnTrue() {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setMasterBill("SAME-MBL");
        newShipment.setDirection("SAME-DIR");
        AdditionalDetails newDetails = new AdditionalDetails();
        newDetails.setSci("NEW-SCI");
        newShipment.setAdditionalDetails(newDetails);

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setMasterBill("SAME-MBL");
        oldShipment.setDirection("SAME-DIR");
        AdditionalDetails oldDetails = new AdditionalDetails();
        oldDetails.setSci("OLD-SCI");
        oldShipment.setAdditionalDetails(oldDetails);

        boolean result = shipmentServiceImplV3.isDiffPresentInOldNewShipment(newShipment, oldShipment);

        assertTrue(result);
    }

    @Test
    void testIsDiffPresentInOldNewShipment_withDifferentBrokers_shouldReturnTrue() {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setMasterBill("SAME-MBL");
        newShipment.setDirection("SAME-DIR");
        AdditionalDetails newDetails = new AdditionalDetails();
        newDetails.setExportBroker(new Parties());
        newShipment.setAdditionalDetails(newDetails);

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setMasterBill("SAME-MBL");
        oldShipment.setDirection("SAME-DIR");
        AdditionalDetails oldDetails = new AdditionalDetails();
        oldDetails.setExportBroker(null);
        oldShipment.setAdditionalDetails(oldDetails);

        boolean result = shipmentServiceImplV3.isDiffPresentInOldNewShipment(newShipment, oldShipment);

        assertTrue(result);
    }

    @Test
    void testDeletePendingStateAfterCancellation_withNoStatusChange_shouldNotDeleteState() {
        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setIsMAWBColoadingEnabled(true);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);

        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setStatus(ShipmentStatus.Created.getValue());

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setStatus(ShipmentStatus.Created.getValue());

        shipmentServiceImplV3.deletePendingStateAfterCancellation(newShipment, oldShipment);

        verify(consoleShipmentMappingDao, never()).deletePendingStateByShipmentId(any());
    }

    @Test
    void testAfterSave_withMasterBillChange_shouldStoreAudit() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(10L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setMasterBill("NEWMBL");
        newShipment.setAdditionalDetails(new AdditionalDetails());

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setMasterBill("OLDMBL");
        oldShipment.setAdditionalDetails(new AdditionalDetails());

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        ConsolidationDetailsProjection projection = mock(ConsolidationDetailsProjection.class);
        when(projection.getConsolidationNumber()).thenReturn("C123");
        when(projection.getTenantId()).thenReturn(5);

        when(consolidationDetailsDao.findMblNumberInDifferentTenant(any())).thenReturn(List.of(projection));
        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, new ShipmentV3Request(),
                ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData);

        verify(auditLogService).addAuditLog(any());

        ArgumentCaptor<AuditLogMetaData> auditCaptor = ArgumentCaptor.forClass(AuditLogMetaData.class);
        verify(auditLogService).addAuditLog(auditCaptor.capture());

        AuditLogMetaData auditData = auditCaptor.getValue();
        assertEquals(ShipmentDetails.class.getSimpleName(), auditData.getParent());
        assertEquals(newShipment.getId(), auditData.getParentId());
        assertEquals(MblDuplicatedLog.class.getSimpleName(), auditData.getEntityType());
        assertEquals(DBOperationType.LOG.name(), auditData.getOperation());
    }

    @Test
    void testAfterSave_withAirMessagingAndEfreightMismatch_shouldThrowException() {
        TenantSettingsDetailsContext.getCurrentTenantSettings().setEnableAirMessaging(true);

        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        AdditionalDetails newDetails = new AdditionalDetails();
        newDetails.setEfreightStatus(Constants.NON);
        newShipment.setAdditionalDetails(newDetails);

        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setEfreightStatus(Constants.EAW);

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setAdditionalDetails(new AdditionalDetails());
        oldShipment.setConsolidationList(Set.of(consolidation));

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setCreate(false);
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        mockTenantSettings();
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidation));

        RunnerException exception = assertThrows(RunnerException.class, () -> {
            shipmentServiceImplV3.afterSave(newShipment, oldShipment, new ShipmentV3Request(),
                    ShipmentSettingsDetailsContext.getCurrentTenantSettings(), consoleShipmentData);
        });

        assertEquals("EFreight status can only be EAW as Consolidation EFrieght Status is EAW", exception.getMessage());
    }

    @Test
    void testIsDiffPresentInOldNewShipment_withOldShipmentNull_shouldReturnTrue() {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setMasterBill("MBL");

        boolean result = shipmentServiceImplV3.isDiffPresentInOldNewShipment(newShipment, null);
        assertTrue(result);
    }

    @Test
    void testIsDiffPresentInOldNewShipment_withDifferentDirection_shouldReturnTrue() {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setMasterBill("SAME");
        newShipment.setDirection("IMPORT");

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setMasterBill("SAME");
        oldShipment.setDirection("EXPORT");

        boolean result = shipmentServiceImplV3.isDiffPresentInOldNewShipment(newShipment, oldShipment);
        assertTrue(result);
    }

    @Test
    void testIsDiffPresentInOldNewShipment_withDifferentImportBroker_shouldReturnTrue() {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setMasterBill("SAME");
        newShipment.setDirection("SAME");
        AdditionalDetails newDetails = new AdditionalDetails();
        newDetails.setImportBroker(new Parties());
        newShipment.setAdditionalDetails(newDetails);

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setMasterBill("SAME");
        oldShipment.setDirection("SAME");
        AdditionalDetails oldDetails = new AdditionalDetails();
        oldDetails.setImportBroker(null);
        oldShipment.setAdditionalDetails(oldDetails);

        boolean result = shipmentServiceImplV3.isDiffPresentInOldNewShipment(newShipment, oldShipment);
        assertTrue(result);
    }

    @Test
    void testIsDiffPresentInOldNewShipment_withDifferentVoyage_shouldReturnTrue() {
        ShipmentDetails newShipment = new ShipmentDetails();
        CarrierDetails newCarrier = new CarrierDetails();
        newCarrier.setVoyage("VOYAGE1");
        newShipment.setCarrierDetails(newCarrier);

        ShipmentDetails oldShipment = new ShipmentDetails();
        CarrierDetails oldCarrier = new CarrierDetails();
        oldCarrier.setVoyage("VOYAGE2");
        oldShipment.setCarrierDetails(oldCarrier);

        boolean result = shipmentServiceImplV3.isDiffPresentInOldNewShipment(newShipment, oldShipment);
        assertTrue(result);
    }

    @Test
    void testIsDiffPresentInOldNewShipment_withDifferentVessel_shouldReturnTrue() {
        ShipmentDetails newShipment = new ShipmentDetails();
        CarrierDetails newCarrier = new CarrierDetails();
        newCarrier.setVessel("VESSEL1");
        newShipment.setCarrierDetails(newCarrier);

        ShipmentDetails oldShipment = new ShipmentDetails();
        CarrierDetails oldCarrier = new CarrierDetails();
        oldCarrier.setVessel("VESSEL2");
        oldShipment.setCarrierDetails(oldCarrier);

        boolean result = shipmentServiceImplV3.isDiffPresentInOldNewShipment(newShipment, oldShipment);
        assertTrue(result);
    }

    @Test
    void testIsDiffPresentInOldNewShipment_withDifferentShippingLine_shouldReturnTrue() {
        ShipmentDetails newShipment = new ShipmentDetails();
        CarrierDetails newCarrier = new CarrierDetails();
        newCarrier.setShippingLine("LINE-A");
        newShipment.setCarrierDetails(newCarrier);

        ShipmentDetails oldShipment = new ShipmentDetails();
        CarrierDetails oldCarrier = new CarrierDetails();
        oldCarrier.setShippingLine("LINE-B");
        oldShipment.setCarrierDetails(oldCarrier);

        boolean result = shipmentServiceImplV3.isDiffPresentInOldNewShipment(newShipment, oldShipment);
        assertTrue(result);
    }

    @Test
    void testIsDiffPresentInOldNewShipment_withDifferentAircraftType_shouldReturnTrue() {
        ShipmentDetails newShipment = new ShipmentDetails();
        CarrierDetails newCarrier = new CarrierDetails();
        newCarrier.setAircraftType("TYPE1");
        newShipment.setCarrierDetails(newCarrier);

        ShipmentDetails oldShipment = new ShipmentDetails();
        CarrierDetails oldCarrier = new CarrierDetails();
        oldCarrier.setAircraftType("TYPE2");
        oldShipment.setCarrierDetails(oldCarrier);

        boolean result = shipmentServiceImplV3.isDiffPresentInOldNewShipment(newShipment, oldShipment);
        assertTrue(result);
    }

    @Test
    void testIsDiffPresentInOldNewShipment_whenAllFieldsMatch_shouldReturnFalse() {
        Parties broker = new Parties();

        AdditionalDetails newDetails = new AdditionalDetails();
        newDetails.setSci("SCI");
        newDetails.setExportBroker(broker);
        newDetails.setImportBroker(broker);

        CarrierDetails newCarrier = new CarrierDetails();
        newCarrier.setVoyage("V1");
        newCarrier.setVessel("VESSEL");
        newCarrier.setShippingLine("LINE");
        newCarrier.setAircraftType("AIR");

        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setMasterBill("MBL");
        newShipment.setDirection("DIR");
        newShipment.setAdditionalDetails(newDetails);
        newShipment.setCarrierDetails(newCarrier);

        AdditionalDetails oldDetails = new AdditionalDetails();
        oldDetails.setSci("SCI");
        oldDetails.setExportBroker(broker);
        oldDetails.setImportBroker(broker);

        CarrierDetails oldCarrier = new CarrierDetails();
        oldCarrier.setVoyage("V1");
        oldCarrier.setVessel("VESSEL");
        oldCarrier.setShippingLine("LINE");
        oldCarrier.setAircraftType("AIR");

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setMasterBill("MBL");
        oldShipment.setDirection("DIR");
        oldShipment.setAdditionalDetails(oldDetails);
        oldShipment.setCarrierDetails(oldCarrier);

        boolean result = shipmentServiceImplV3.isDiffPresentInOldNewShipment(newShipment, oldShipment);
        assertFalse(result);
    }

    @Test
    void testProcessBranchesAndPartner_withNonZeroReceivingBranch_shouldRemainUnchanged() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setReceivingBranch(100L); // Non-zero

        shipmentServiceImplV3.processBranchesAndPartner(shipment);

        assertEquals(100L, shipment.getReceivingBranch());
    }

    @Test
    void testProcessBranchesAndPartner_withTriangulationListSizeGreaterThanOne_shouldNotSetNull() {
        ShipmentDetails shipment = new ShipmentDetails();
        List<TriangulationPartner> list = new ArrayList<>();
        TriangulationPartner tp1 = new TriangulationPartner();
        tp1.setTriangulationPartner(0L);
        TriangulationPartner tp2 = new TriangulationPartner();
        tp2.setTriangulationPartner(1L);
        list.add(tp1);
        list.add(tp2);
        shipment.setTriangulationPartnerList(list);

        shipmentServiceImplV3.processBranchesAndPartner(shipment);

        assertNotNull(shipment.getTriangulationPartnerList());
        assertEquals(2, shipment.getTriangulationPartnerList().size());
    }

    @Test
    void testProcessBranchesAndPartner_withTriangulationListSizeOneButPartnerNotZero_shouldRemainUnchanged() {
        ShipmentDetails shipment = new ShipmentDetails();
        List<TriangulationPartner> list = new ArrayList<>();
        TriangulationPartner tp = new TriangulationPartner();
        tp.setTriangulationPartner(10L); // Non-zero
        list.add(tp);
        shipment.setTriangulationPartnerList(list);

        shipmentServiceImplV3.processBranchesAndPartner(shipment);

        assertNotNull(shipment.getTriangulationPartnerList());
        assertEquals(1, shipment.getTriangulationPartnerList().size());
        assertEquals(10L, shipment.getTriangulationPartnerList().get(0).getTriangulationPartner());
    }

    @Test
    void testProcessBranchesAndPartner_withTriangulationListEmpty_shouldSkipAllBranches() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTriangulationPartnerList(new ArrayList<>()); // Empty list

        shipmentServiceImplV3.processBranchesAndPartner(shipment);

        assertNotNull(shipment.getTriangulationPartnerList());
        assertTrue(shipment.getTriangulationPartnerList().isEmpty());
    }

    @Test
    void testProcessBranchesAndPartner_withNonZeroDocumentationPartner_shouldRemainUnchanged() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setDocumentationPartner(99L); // Non-zero

        shipmentServiceImplV3.processBranchesAndPartner(shipment);

        assertEquals(99L, shipment.getDocumentationPartner());
    }

    @Test
    void testEmptyConsolidationList_shouldPopulateImportExportBroker() {
        shipmentDetailsEntity.setConsolidationList(null);
        shipmentDetailsEntity.setDirection(Constants.DIRECTION_EXP);
        shipmentDetailsEntity.setTransportMode(TRANSPORT_MODE_AIR);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put("TenantId", 1);
        Parties exportParty = new Parties();
        exportParty.setOrgId("1");
        exportParty.setTenantId(1);
        exportParty.setOrgData(orgData);
        when(v1ServiceUtil.getDefaultAgentOrgParty(null)).thenReturn(exportParty);

        shipmentServiceImplV3.populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetailsEntity);

        assertNotNull(shipmentDetailsEntity.getAdditionalDetails().getExportBroker());
        assertEquals(1L, shipmentDetailsEntity.getOriginBranch());
    }

    @Test
    void testSendingAgentPresent_shouldSetExportBrokerAndOriginBranch() {
        Parties sendingAgent = new Parties();
        sendingAgent.setOrgId("1");
        sendingAgent.setTenantId(2);

        consolidationDetailsEntity.setSendingAgent(sendingAgent);
        shipmentDetailsEntity.setConsolidationList(Set.of(consolidationDetailsEntity));
        when(commonUtils.removeIdFromParty(sendingAgent)).thenReturn(sendingAgent);

        shipmentServiceImplV3.populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetailsEntity);

        assertEquals(sendingAgent, shipmentDetailsEntity.getAdditionalDetails().getExportBroker());
    }

    @Test
    void testSendingAgentPresent_withNullAdditionalDetails() {
        Parties sendingAgent = new Parties();
        sendingAgent.setOrgId("1");
        sendingAgent.setTenantId(1);

        shipmentDetailsEntity.setAdditionalDetails(null);
        consolidationDetailsEntity.setSendingAgent(sendingAgent);
        shipmentDetailsEntity.setConsolidationList(Set.of(consolidationDetailsEntity));
        when(commonUtils.removeIdFromParty(sendingAgent)).thenReturn(sendingAgent);

        shipmentServiceImplV3.populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetailsEntity);

        assertNotNull(shipmentDetailsEntity.getAdditionalDetails().getExportBroker());
    }

    @Test
    void testReceivingAgentPresent_shouldSetImportBroker() {
        Parties receivingAgent = new Parties();
        receivingAgent.setOrgId("1");
        consolidationDetailsEntity.setReceivingAgent(receivingAgent);
        shipmentDetailsEntity.setConsolidationList(Set.of(consolidationDetailsEntity));

        when(commonUtils.removeIdFromParty(receivingAgent)).thenReturn(receivingAgent);

        shipmentServiceImplV3.populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetailsEntity);

        assertEquals(receivingAgent, shipmentDetailsEntity.getAdditionalDetails().getImportBroker());
    }

    @Test
    void testReceivingAgentPresent_withNullAdditionalDetails_IMP() {
        Parties receivingAgent = new Parties();
        receivingAgent.setOrgId("1");
        shipmentDetailsEntity.setAdditionalDetails(null);
        consolidationDetailsEntity.setReceivingAgent(receivingAgent);
        shipmentDetailsEntity.setConsolidationList(Set.of(consolidationDetailsEntity));
        shipmentDetailsEntity.setDirection("IMP");

        when(commonUtils.removeIdFromParty(receivingAgent)).thenReturn(receivingAgent);

        shipmentServiceImplV3.populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetailsEntity);

        assertNotNull(shipmentDetailsEntity.getAdditionalDetails().getImportBroker());
    }

    @Test
    void testDefaultExportBroker_whenDirectionIsEXP() {
        shipmentDetailsEntity.setDirection(Constants.DIRECTION_EXP);
        shipmentDetailsEntity.setAdditionalDetails(null);

        Parties defaultParty = new Parties();
        defaultParty.setOrgId("1");
        defaultParty.setTenantId(1);
        when(v1ServiceUtil.getDefaultAgentOrgParty(null)).thenReturn(defaultParty);

        shipmentDetailsEntity.setConsolidationList(null);

        shipmentServiceImplV3.populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetailsEntity);

        assertEquals(defaultParty, shipmentDetailsEntity.getAdditionalDetails().getExportBroker());
    }

    @Test
    void testDefaultImportBroker_whenDirectionIsIMP() {
        shipmentDetailsEntity.setDirection(Constants.DIRECTION_IMP);
        shipmentDetailsEntity.setAdditionalDetails(null);

        Parties defaultParty = new Parties();
        defaultParty.setOrgId("default");
        when(v1ServiceUtil.getDefaultAgentOrgParty(null)).thenReturn(defaultParty);

        shipmentDetailsEntity.setConsolidationList(null);

        shipmentServiceImplV3.populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetailsEntity);

        assertEquals(defaultParty, shipmentDetailsEntity.getAdditionalDetails().getImportBroker());
    }

    @Test
    void testShouldSaveConsolidationDetails_whenUpdated() {
        Parties agent = new Parties();
        agent.setOrgId("1");
        agent.setTenantId(1);
        shipmentDetailsEntity.getAdditionalDetails().setExportBroker(agent);
        consolidationDetailsEntity.setSendingAgent(null);
        consolidationDetailsEntity.setReceivingAgent(null);

        Parties receiving = new Parties();
        receiving.setOrgId("2");
        receiving.setTenantId(2);
        shipmentDetailsEntity.getAdditionalDetails().setImportBroker(receiving);

        shipmentDetailsEntity.setConsolidationList(Set.of(consolidationDetailsEntity));

        assertDoesNotThrow(() -> shipmentServiceImplV3.populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetailsEntity));
    }

    @Test
    void testConsoleCreationNeeded_SEA_FCL() {
        var req = new CustomerBookingV3Request();
        req.setTransportType(Constants.TRANSPORT_MODE_SEA);
        req.setCargoType(Constants.CARGO_TYPE_FCL);
        assertTrue(shipmentServiceImplV3.isConsoleCreationNeededV3(req));
    }

    @Test
    void testConsoleCreationNeeded_ROA_FTL() {
        var req = new CustomerBookingV3Request();
        req.setTransportType(Constants.TRANSPORT_MODE_ROA);
        req.setCargoType(Constants.CARGO_TYPE_FTL);
        assertTrue(shipmentServiceImplV3.isConsoleCreationNeededV3(req));
    }

    @Test
    void testConsoleCreationNeeded_ROA_FCL() {
        var req = new CustomerBookingV3Request();
        req.setTransportType(Constants.TRANSPORT_MODE_ROA);
        req.setCargoType(Constants.CARGO_TYPE_FCL);
        assertTrue(shipmentServiceImplV3.isConsoleCreationNeededV3(req));
    }

    @Test
    void testConsoleCreationNeeded_RAI_FCL() {
        var req = new CustomerBookingV3Request();
        req.setTransportType(Constants.TRANSPORT_MODE_RAI);
        req.setCargoType(Constants.CARGO_TYPE_FCL);
        assertTrue(shipmentServiceImplV3.isConsoleCreationNeededV3(req));
    }

    @Test
    void testConsoleCreationNotNeeded_AIR_FCL() {
        var req = new CustomerBookingV3Request();
        req.setTransportType(Constants.TRANSPORT_MODE_AIR);
        req.setCargoType(Constants.CARGO_TYPE_FCL);
        assertFalse(shipmentServiceImplV3.isConsoleCreationNeededV3(req));
    }

    @Test
    void testConsoleCreationNotNeeded_ROA_LTL() {
        var req = new CustomerBookingV3Request();
        req.setTransportType(Constants.TRANSPORT_MODE_ROA);
        req.setCargoType("LTL");
        assertFalse(shipmentServiceImplV3.isConsoleCreationNeededV3(req));
    }

    @Test
    void testGetRoutingList_whenCarrierDetailNull_shouldReturnEmptyList() {
        List<RoutingsRequest> result = shipmentServiceImplV3.getCustomerBookingRequestRoutingList(null, Constants.TRANSPORT_MODE_SEA);
        assertTrue(result.isEmpty());
    }

    @Test
    void testGetRoutingList_whenRouteMasterDisabled_shouldReturnEmptyList() {
        CarrierDetailRequest carrierDetail = new CarrierDetailRequest();
        mockShipmentSettings();
        List<RoutingsRequest> result = shipmentServiceImplV3.getCustomerBookingRequestRoutingList(carrierDetail, Constants.TRANSPORT_MODE_SEA);
        assertTrue(result.isEmpty());
    }

    @Test
    void testGetRoutingList_whenRouteMasterEnabled_shouldReturnConvertedList() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setEnableRouteMaster(true);
        CarrierDetailRequest carrierDetail = new CarrierDetailRequest();
        CarrierDetails carrierDetails = new CarrierDetails();
        List<Routings> routings = List.of(new Routings());
        List<RoutingsRequest> expectedList = List.of(new RoutingsRequest());

        mockShipmentSettings();
        when(jsonHelper.convertValue(carrierDetail, CarrierDetails.class)).thenReturn(carrierDetails);
        when(routingsDao.generateDefaultRouting(carrierDetails, Constants.TRANSPORT_MODE_SEA)).thenReturn(routings);
        when(commonUtils.convertToList(routings, RoutingsRequest.class)).thenReturn(expectedList);

        List<RoutingsRequest> result = shipmentServiceImplV3.getCustomerBookingRequestRoutingList(carrierDetail, Constants.TRANSPORT_MODE_SEA);

        assertEquals(expectedList, result);
    }

    @Test
    void testAddFilesFromBookingToShipment_successfulResponse() {
        DocumentManagerResponse<T> expectedResponse = new DocumentManagerResponse<>();
        when(documentManagerService.updateFileEntities(any())).thenReturn(expectedResponse);

        var result = shipmentServiceImplV3.addFilesFromBookingToShipment("shipment-guid-123", "booking-guid-456");

        assertNotNull(result);
        assertEquals(expectedResponse, result);

        ArgumentCaptor<DocumentManagerUpdateFileEntitiesRequest> captor =
                ArgumentCaptor.forClass(DocumentManagerUpdateFileEntitiesRequest.class);
        verify(documentManagerService).updateFileEntities(captor.capture());

        var request = captor.getValue();
        assertNotNull(request);
        assertEquals(1, request.getFilesToUpdate().size());

        var updateFileRequest = request.getFilesToUpdate().get(0);
        assertEquals(Constants.BOOKINGS_WITH_SQ_BRACKETS, updateFileRequest.getSource().getEntityType());
        assertEquals(1, updateFileRequest.getEntitiesToAttach().size());
        assertEquals(Constants.SHIPMENTS_WITH_SQ_BRACKETS, updateFileRequest.getEntitiesToAttach().get(0).getEntityType());
    }

    @Test
    void testAddFilesFromBookingToShipment_whenExceptionThrown_shouldReturnNull() {
        when(documentManagerService.updateFileEntities(any()))
                .thenThrow(new RuntimeException("Update failed"));

        var result = shipmentServiceImplV3.addFilesFromBookingToShipment("shipment-guid-123", "booking-guid-456");

        assertNull(result);
    }

    @Test
    void testCreateLogHistoryForShipment_withPayload_success() {
        String payload = "{\"some\":\"json\"}";
        assertDoesNotThrow(() ->
                shipmentServiceImplV3.createLogHistoryForShipment(payload, 1L, UUID.randomUUID())
        );

        verify(logsHistoryService, times(1)).createLogHistory(any());
    }

    @Test
    void testCreateLogHistoryForShipment_withPayload_exception() {
        String payload = "{\"some\":\"json\"}";
        doThrow(new RuntimeException("DB error")).when(logsHistoryService)
                .createLogHistory(any(LogHistoryRequest.class));

        assertDoesNotThrow(() -> shipmentServiceImplV3.createLogHistoryForShipment(payload, 1L, UUID.randomUUID()));
    }

    @Test
    void testCreateLogHistoryForShipment_withShipmentDetails_success() {
        String payload = "{\"some\":\"json\"}";
        ShipmentDetails mockShipmentDetails = new ShipmentDetails();
        mockShipmentDetails.setId(1L);
        mockShipmentDetails.setGuid(UUID.randomUUID());
        mockShipmentDetails.setTenantId(1);

        when(jsonHelper.convertToJson(mockShipmentDetails)).thenReturn(payload);

        shipmentServiceImplV3.createLogHistoryForShipment(mockShipmentDetails);

        verify(logsHistoryService).createLogHistory(any());
    }

    @Test
    void testCreateLogHistoryForShipment_withShipmentDetails_exception() {
        ShipmentDetails mockShipmentDetails = new ShipmentDetails();
        mockShipmentDetails.setId(1L);
        mockShipmentDetails.setGuid(UUID.randomUUID());
        mockShipmentDetails.setTenantId(1);

        when(jsonHelper.convertToJson(mockShipmentDetails)).thenThrow(new RuntimeException("JSON error"));

        shipmentServiceImplV3.createLogHistoryForShipment(mockShipmentDetails);

        assertDoesNotThrow(() -> shipmentServiceImplV3.createLogHistoryForShipment(mockShipmentDetails));
    }

    @Test
    void testFetchAllMasterDataByKey_shouldRunAllAsyncCallsAndReturnMap() {
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();

        // Mock withMdc to return the same Runnable
        when(masterDataUtils.withMdc(any())).thenAnswer(invocation -> invocation.getArgument(0));

        // Mock all master data helper methods to just modify the map for visibility
        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("master", "ok");
            return null;
        }).when(masterDataHelper).addAllMasterDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("unlocation", "ok");
            return null;
        }).when(masterDataHelper).addAllUnlocationDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("carrier", "ok");
            return null;
        }).when(masterDataHelper).addAllCarrierDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("currency", "ok");
            return null;
        }).when(masterDataHelper).addAllCurrencyDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("tenant", "ok");
            return null;
        }).when(masterDataHelper).addAllTenantDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("warehouse", "ok");
            return null;
        }).when(masterDataHelper).addAllWarehouseDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("activity", "ok");
            return null;
        }).when(masterDataHelper).addAllActivityDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("salesAgent", "ok");
            return null;
        }).when(masterDataHelper).addAllSalesAgentInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("vessel", "ok");
            return null;
        }).when(masterDataHelper).addAllVesselDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("organization", "ok");
            return null;
        }).when(masterDataHelper).addAllOrganizationDataInSingleCall(any(), any());

        // Call method under test
        Map<String, Object> responseMap = shipmentServiceImplV3.fetchAllMasterDataByKey(shipmentDetailsEntity, shipmentDetailsResponse);

        // Validate map contains all expected keys
        assertEquals(10, responseMap.size());
        assertEquals("ok", responseMap.get("master"));
        assertEquals("ok", responseMap.get("unlocation"));
        assertEquals("ok", responseMap.get("carrier"));
        assertEquals("ok", responseMap.get("currency"));
        assertEquals("ok", responseMap.get("tenant"));
        assertEquals("ok", responseMap.get("warehouse"));
        assertEquals("ok", responseMap.get("activity"));
        assertEquals("ok", responseMap.get("salesAgent"));
        assertEquals("ok", responseMap.get("vessel"));
        assertEquals("ok", responseMap.get("organization"));

        // Optional: verify method calls
        verify(masterDataHelper).addAllMasterDataInSingleCall(any(), any());
        verify(masterDataHelper).addAllUnlocationDataInSingleCall(any(), any());
        verify(masterDataHelper).addAllCarrierDataInSingleCall(any(), any());
        verify(masterDataHelper).addAllCurrencyDataInSingleCall(any(), any());
        verify(masterDataHelper).addAllTenantDataInSingleCall(any(), any());
        verify(masterDataHelper).addAllWarehouseDataInSingleCall(any(), any());
        verify(masterDataHelper).addAllActivityDataInSingleCall(any(), any());
        verify(masterDataHelper).addAllSalesAgentInSingleCall(any(), any());
        verify(masterDataHelper).addAllVesselDataInSingleCall(any(), any());
        verify(masterDataHelper).addAllOrganizationDataInSingleCall(any(), any());
    }

    @Test
    void testCalculateVW1() throws Exception {
        AutoUpdateWtVolRequest request = new AutoUpdateWtVolRequest();
        request.setTransportMode(null);
        AutoUpdateWtVolResponse response = new AutoUpdateWtVolResponse();

        AutoUpdateWtVolResponse result = shipmentServiceImplV3.calculateVW(request, response, false);

        assertSame(response, result);
    }

    @Test
    void testCalculateVW2() throws Exception {
        AutoUpdateWtVolRequest request = new AutoUpdateWtVolRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        AutoUpdateWtVolResponse response = new AutoUpdateWtVolResponse();
        response.setWeightUnit("KG");
        response.setVolumeUnit("M3");
        response.setWeight(BigDecimal.TEN);
        response.setVolume(BigDecimal.ONE);

        VolumeWeightChargeable vw = new VolumeWeightChargeable();
        vw.setChargeable(BigDecimal.valueOf(7.77));
        vw.setChargeableUnit("M3");
        vw.setVolumeWeight(BigDecimal.valueOf(7.77));
        vw.setVolumeWeightUnit("M3");

        Mockito.when(consolidationV3Service.calculateVolumeWeight(anyString(), anyString(), anyString(), any(), any()))
                .thenReturn(vw);

        AutoUpdateWtVolResponse result = shipmentServiceImplV3.calculateVW(request, response, false);

        assertEquals(BigDecimal.valueOf(8.0), result.getChargable());
        assertEquals("M3", result.getChargeableUnit());
    }

    @Test
    void testCalculateVW3() throws Exception {
        AutoUpdateWtVolRequest request = new AutoUpdateWtVolRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        AutoUpdateWtVolResponse response = new AutoUpdateWtVolResponse();
        response.setWeightUnit("LB");
        response.setVolumeUnit("M3");
        response.setWeight(BigDecimal.valueOf(220.46));
        response.setVolume(BigDecimal.valueOf(1));

        VolumeWeightChargeable vw = new VolumeWeightChargeable();
        vw.setChargeable(BigDecimal.TEN);
        vw.setChargeableUnit("KG");
        vw.setVolumeWeight(BigDecimal.valueOf(9.99));
        vw.setVolumeWeightUnit("KG");

        Mockito.when(consolidationV3Service.calculateVolumeWeight(anyString(), anyString(), anyString(), any(), any()))
                .thenReturn(vw);

        AutoUpdateWtVolResponse result = shipmentServiceImplV3.calculateVW(request, response, true);

        assertEquals(BigDecimal.valueOf(10), result.getChargable());
        assertEquals(Constants.WEIGHT_UNIT_KG, result.getChargeableUnit());
    }

    @Test
    void testCalculateVW4() throws Exception {
        AutoUpdateWtVolRequest request = new AutoUpdateWtVolRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        AutoUpdateWtVolResponse response = new AutoUpdateWtVolResponse();
        response.setWeightUnit(null);
        response.setVolumeUnit(null);

        AutoUpdateWtVolResponse result = shipmentServiceImplV3.calculateVW(request, response, false);

        assertSame(response, result);
    }

    @Test
    void testUpdateShipmentDetails_withValidContainers() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setWeightChargeableUnit(Constants.WEIGHT_UNIT_KG).setVolumeChargeableUnit(Constants.VOLUME_UNIT_M3);
        mockShipmentSettings();

        Containers c1 = new Containers();
        c1.setGrossWeight(BigDecimal.valueOf(100));
        c1.setGrossWeightUnit("LB");
        c1.setTareWeight(BigDecimal.valueOf(10));
        c1.setTareWeightUnit("LB");
        c1.setGrossVolume(BigDecimal.valueOf(2));
        c1.setGrossVolumeUnit("M3");
        c1.setPacks("5");
        c1.setContainerCount(1L);

        List<Containers> containers = List.of(c1);

        AutoUpdateWtVolResponse response = new AutoUpdateWtVolResponse();
        AutoUpdateWtVolResponse updated = shipmentServiceImplV3.updateShipmentDetails(response, containers);

        assertEquals(Constants.WEIGHT_UNIT_KG, updated.getWeightUnit());
        assertEquals(Constants.VOLUME_UNIT_M3, updated.getVolumeUnit());
        assertEquals("5", updated.getNoOfPacks());
        assertEquals(Constants.WEIGHT_UNIT_KG, updated.getNetWeightUnit());
    }

    @Test
    void testUpdateShipmentDetails_withNullContainerList() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setWeightChargeableUnit(Constants.WEIGHT_UNIT_KG).setVolumeChargeableUnit(Constants.VOLUME_UNIT_M3);
        mockShipmentSettings();

        AutoUpdateWtVolResponse response = new AutoUpdateWtVolResponse();
        AutoUpdateWtVolResponse updated = shipmentServiceImplV3.updateShipmentDetails(response, new ArrayList<>());

        assertEquals(Constants.WEIGHT_UNIT_KG, updated.getWeightUnit());
        assertEquals(Constants.VOLUME_UNIT_M3, updated.getVolumeUnit());
        assertNull(updated.getNoOfPacks());
        assertEquals("", updated.getPacksUnit());
        assertEquals(Constants.WEIGHT_UNIT_KG, updated.getNetWeightUnit());
    }

    @Test
    void testUpdateShipmentDetails_withEmptyContainerList() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setWeightChargeableUnit(Constants.WEIGHT_UNIT_KG).setVolumeChargeableUnit(Constants.VOLUME_UNIT_M3);
        mockShipmentSettings();

        List<Containers> containers = new ArrayList<>();

        AutoUpdateWtVolResponse response = new AutoUpdateWtVolResponse();
        AutoUpdateWtVolResponse updated = shipmentServiceImplV3.updateShipmentDetails(response, containers);

        assertEquals(Constants.WEIGHT_UNIT_KG, updated.getWeightUnit());
        assertEquals(Constants.VOLUME_UNIT_M3, updated.getVolumeUnit());
        assertNull(updated.getNoOfPacks());
        assertEquals("", updated.getPacksUnit());
        assertEquals(Constants.WEIGHT_UNIT_KG, updated.getNetWeightUnit());
    }

    @Test
    void testCalculateWeightAndVolumeUnit_withEmptyRequestUnits_andValidPackings() throws RunnerException {
        AutoUpdateWtVolRequest request = new AutoUpdateWtVolRequest(); // no units
        AutoUpdateWtVolResponse response = new AutoUpdateWtVolResponse();

        Packing p1 = new Packing();
        p1.setWeight(BigDecimal.valueOf(100));
        p1.setWeightUnit("LB");
        p1.setVolume(BigDecimal.valueOf(2));
        p1.setVolumeUnit("M3");

        List<Packing> packings = List.of(p1);

        AutoUpdateWtVolResponse result = shipmentServiceImplV3.calculateWeightAndVolumeUnit(request, packings, response);

        assertEquals(Constants.WEIGHT_UNIT_KG, result.getWeightUnit());
        assertEquals(Constants.VOLUME_UNIT_M3, result.getVolumeUnit());
    }

    @Test
    void testCalculateWeightAndVolumeUnit_withPresetUnits_andMultiplePackings() throws RunnerException {
        AutoUpdateWtVolRequest request = new AutoUpdateWtVolRequest();
        request.setWeightUnit("KG");
        request.setVolumeUnit("M3");

        AutoUpdateWtVolResponse response = new AutoUpdateWtVolResponse();
        response.setWeightUnit("KG");
        response.setVolumeUnit("M3");

        Packing p1 = new Packing();
        p1.setWeight(BigDecimal.valueOf(100));
        p1.setWeightUnit("LB");
        p1.setVolume(BigDecimal.valueOf(1));
        p1.setVolumeUnit("M3");

        Packing p2 = new Packing();
        p2.setWeight(BigDecimal.valueOf(200));
        p2.setWeightUnit("LB");
        p2.setVolume(BigDecimal.valueOf(3));
        p2.setVolumeUnit("M3");

        List<Packing> packings = List.of(p1, p2);

        AutoUpdateWtVolResponse result = shipmentServiceImplV3.calculateWeightAndVolumeUnit(request, packings, response);

        assertEquals("KG", result.getWeightUnit());
        assertEquals("M3", result.getVolumeUnit());
    }

    @Test
    void testCalculateWeightAndVolumeUnit_withNullOrEmptyPackings() throws RunnerException {
        AutoUpdateWtVolRequest request = new AutoUpdateWtVolRequest();
        AutoUpdateWtVolResponse response = new AutoUpdateWtVolResponse();

        AutoUpdateWtVolResponse result1 = shipmentServiceImplV3.calculateWeightAndVolumeUnit(request, null, response);
        assertEquals(Constants.WEIGHT_UNIT_KG, result1.getWeightUnit());
        assertEquals(Constants.VOLUME_UNIT_M3, result1.getVolumeUnit());

        AutoUpdateWtVolResponse result2 = shipmentServiceImplV3.calculateWeightAndVolumeUnit(request, Collections.emptyList(), response);
        assertEquals(Constants.WEIGHT_UNIT_KG, result2.getWeightUnit());
        assertEquals(Constants.VOLUME_UNIT_M3, result2.getVolumeUnit());
    }

    @Test
    void testBeforeSave_shouldDeleteHblAndUnsetDraftPrinted() throws Exception {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setId(100L);
        shipment.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setDirection(Constants.DIRECTION_EXP);

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setDraftPrinted(true);
        shipment.setAdditionalDetails(additionalDetails);

        ShipmentV3Request request = new ShipmentV3Request();
        request.setIsChargableEditable(false);

        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();

        Hbl hbl = new Hbl();
        List<Hbl> hbls = List.of(hbl);

        when(hblDao.findByShipmentId(100L)).thenReturn(hbls);

        doNothing().when(shipmentValidationV3Util).validateStaleShipmentUpdateError(any(), anyBoolean());
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        mockTenantSettings();

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        shipmentServiceImplV3.beforeSave(shipment, null, true, request, settings, false, consoleShipmentData);

        verify(hblDao).delete(hbl);
        assertFalse(shipment.getAdditionalDetails().getDraftPrinted());
    }

    @Test
    void testBeforeSave_shouldThrowValidationExceptionForJobTypeChange() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setId(101L);
        shipment.setDirection(Constants.DIRECTION_EXP);
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setJobType(Constants.SHIPMENT_TYPE_DRT);
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setPrintedOriginal(true);
        shipment.setAdditionalDetails(additionalDetails);

        ShipmentDetails oldEntity = new ShipmentDetails();
        oldEntity.setJobType("OTHER_TYPE");

        ShipmentV3Request request = new ShipmentV3Request();
        request.setIsChargableEditable(false);

        ConsoleShipmentData consoleShipmentData = new ConsoleShipmentData();
        consoleShipmentData.setFromET(false);
        consoleShipmentData.setSyncConsole(false);

        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();

        doNothing().when(shipmentValidationV3Util).validateStaleShipmentUpdateError(any(), anyBoolean());
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);

        ValidationException exception = assertThrows(
                ValidationException.class,
                () -> shipmentServiceImplV3.beforeSave(shipment, oldEntity, false, request, settings, false, consoleShipmentData)
        );

        assertEquals("Consolidation type cannot be changed as the original BL has been generated for this shipment.", exception.getMessage());
    }

    @Test
    void assignFirstBookingContainerToShipmentCargo_shouldAssignFirstAndResetRest() throws RunnerException {
        Containers firstContainer = mock(Containers.class);
        Containers secondContainer = mock(Containers.class);
        List<Containers> containersList = List.of(firstContainer, secondContainer);
        CustomerBookingV3Request bookingRequest = mock(CustomerBookingV3Request.class);

        when(firstContainer.getId()).thenReturn(100L);

        Long result = shipmentServiceImplV3.assignFirstBookingContainerToShipmentCargo(containersList, bookingRequest);

        assertEquals(100L, result);
        verify(containerV3Service).addShipmentCargoToContainerInCreateFromBooking(firstContainer, bookingRequest);
        verify(containerV3Util).setContainerNetWeight(firstContainer);
    }

    @Test
    void findByIdIn_shouldReturnShipmentDetails() {
        List<Long> shipmentIds = List.of(1L, 2L);
        ShipmentDetails shipmentDetail = mock(ShipmentDetails.class);
        List<ShipmentDetails> expectedShipments = List.of(shipmentDetail);

        when(shipmentDao.findByIdIn(shipmentIds)).thenReturn(expectedShipments);

        List<ShipmentDetails> result = shipmentServiceImplV3.findByIdIn(shipmentIds);

        assertEquals(expectedShipments, result);
        verify(shipmentDao).findByIdIn(shipmentIds);
    }

    @Test
    void saveAll_shouldReturnSavedShipments() throws RunnerException {
        ShipmentDetails shipment1 = mock(ShipmentDetails.class);
        ShipmentDetails shipment2 = mock(ShipmentDetails.class);
        List<ShipmentDetails> inputShipments = List.of(shipment1, shipment2);
        List<ShipmentDetails> savedShipments = List.of(shipment1, shipment2);

        when(shipmentDao.saveAll(inputShipments)).thenReturn(savedShipments);

        List<ShipmentDetails> result = shipmentServiceImplV3.saveAll(inputShipments);

        assertEquals(savedShipments, result);
        verify(shipmentDao).saveAll(inputShipments);
    }

    @Test
    void saveAll_emptyList_shouldReturnEmptyList() throws RunnerException {
        List<ShipmentDetails> inputShipments = List.of();

        when(shipmentDao.saveAll(inputShipments)).thenReturn(List.of());

        List<ShipmentDetails> result = shipmentServiceImplV3.saveAll(inputShipments);

        assertTrue(result.isEmpty());
        verify(shipmentDao).saveAll(inputShipments);
    }

    @Test
    void findShipmentDetailsByAttachedContainerIds_shouldReturnProjections() {
        List<Long> containerIds = List.of(10L, 20L);
        ShipmentDetailsProjection projection = mock(ShipmentDetailsProjection.class);
        List<ShipmentDetailsProjection> expected = List.of(projection);

        when(shipmentDao.findShipmentDetailsByAttachedContainerIds(containerIds)).thenReturn(expected);

        List<ShipmentDetailsProjection> result = shipmentServiceImplV3.findShipmentDetailsByAttachedContainerIds(containerIds);

        assertEquals(expected, result);
        verify(shipmentDao).findShipmentDetailsByAttachedContainerIds(containerIds);
    }

    @Test
    void findShipmentDetailsByAttachedContainerIds_emptyList_shouldReturnEmptyList() {
        List<Long> containerIds = List.of();

        when(shipmentDao.findShipmentDetailsByAttachedContainerIds(containerIds)).thenReturn(List.of());

        List<ShipmentDetailsProjection> result = shipmentServiceImplV3.findShipmentDetailsByAttachedContainerIds(containerIds);

        assertTrue(result.isEmpty());
        verify(shipmentDao).findShipmentDetailsByAttachedContainerIds(containerIds);
    }

    @Test
    void getShipmentsFromId_shouldReturnShipments() {
        List<Long> shipmentIds = List.of(101L, 102L);
        List<ShipmentDetails> expected = List.of(shipmentDetailsEntity);

        when(shipmentDao.getShipmentNumberFromId(shipmentIds)).thenReturn(expected);

        List<ShipmentDetails> result = shipmentServiceImplV3.getShipmentsFromId(shipmentIds);

        assertEquals(expected, result);
        verify(shipmentDao).getShipmentNumberFromId(shipmentIds);
    }

    @Test
    void getShipmentsFromId_emptyList_shouldReturnEmptyList() {
        List<Long> shipmentIds = List.of();

        when(shipmentDao.getShipmentNumberFromId(shipmentIds)).thenReturn(List.of());

        List<ShipmentDetails> result = shipmentServiceImplV3.getShipmentsFromId(shipmentIds);

        assertTrue(result.isEmpty());
        verify(shipmentDao).getShipmentNumberFromId(shipmentIds);
    }

    @Test
    void testSetContainerTeuCountResponse_withContainers() {
        ShipmentRetrieveLiteResponse response = new ShipmentRetrieveLiteResponse();
        Set<Containers> containers = Set.of(createContainer("20GP", 2L, 100L));
        StringBuilder stringBuilder = new StringBuilder("key");

        Cache mockCache = mock(Cache.class);
        Cache.ValueWrapper wrapper = mock(Cache.ValueWrapper.class);
        EntityTransferContainerType containerType = new EntityTransferContainerType();
        containerType.setTeu(1.5);
        when(wrapper.get()).thenReturn(containerType);
        when(mockCache.get(any())).thenReturn(wrapper);
        when(cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA)).thenReturn(mockCache);
        when(keyGenerator.customCacheKeyForMasterData(any(), any())).thenReturn(stringBuilder);

        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);

        shipmentServiceImplV3.setContainerTeuCountResponse(response, containers);

        assertEquals(2L, response.getContainerCount());
    }

    @Test
    void testSetContainerTeuCountResponse_withEmptyContainers() {
        ShipmentRetrieveLiteResponse response = new ShipmentRetrieveLiteResponse();
        shipmentServiceImplV3.setContainerTeuCountResponse(response, Collections.emptySet());

        assertNull(response.getTeuCount());
        assertNull(response.getContainerCount());
    }

    @Test
    void testGetEntityTransferObjectCache_fromEmptyCacheMap() {
        Containers container = createContainer("20GP", 1L, 101L);
        Map<String, Object> cacheMap = new HashMap<>();
        StringBuilder stringBuilder = new StringBuilder("key");

        Cache mockCache = mock(Cache.class);
        Cache.ValueWrapper wrapper = mock(Cache.ValueWrapper.class);
        when(wrapper.get()).thenReturn(new EntityTransferContainerType());
        when(mockCache.get(any())).thenReturn(wrapper);
        when(cacheManager.getCache(any())).thenReturn(mockCache);
        when(keyGenerator.customCacheKeyForMasterData(any(), any())).thenReturn(stringBuilder);

        Object result = shipmentServiceImplV3.getEntityTransferObjectCache(container, cacheMap);

        assertNotNull(result);
    }

    @Test
    void testGetEntityTransferObjectCache_fromPopulatedCacheMap() {
        Containers container = createContainer("40HC", 1L, 102L);
        EntityTransferContainerType expected = new EntityTransferContainerType();
        Map<String, Object> cacheMap = new HashMap<>();
        cacheMap.put("40HC", expected);

        Object result = shipmentServiceImplV3.getEntityTransferObjectCache(container, cacheMap);

        assertEquals(expected, result);
    }

    @Test
    void testProcessCacheAndContainerResponseList() {
        Containers container = createContainer("20GP", 1L, 201L);
        List<Containers> containerList = List.of(container);
        Set<String> containerTypes = new HashSet<>();
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Map<String, Object> cacheMap = new HashMap<>();

        ContainerResponse containerResponse = new ContainerResponse();
        containerResponse.setId(201L);

        when(jsonHelper.convertValueToList(any(), eq(ContainerResponse.class))).thenReturn(List.of(containerResponse));
        when(masterDataUtils.createInBulkContainerTypeRequest(any(), any(), any(), any(), any())).thenReturn(List.of("20GP"));
        when(masterDataUtils.fetchInBulkContainerTypes(Set.of("20GP"))).thenReturn(Map.of("20GP", new EntityTransferContainerType()));

        shipmentServiceImplV3.processCacheAndContainerResponseList(containerList, containerTypes, fieldNameKeyMap, cacheMap);

        assertTrue(containerTypes.contains("20GP"));
    }

    @Test
    void testTriggerPushToDownStream_withConsolidationAndMappings_notMatchingShipmentId() {
        ShipmentDetails shipment = shipmentDetailsEntity;
        shipment.setId(100L);
        shipment.setConsolidationList(Set.of(consolidationDetailsEntity));

        ConsoleShipmentMapping mapping1 = new ConsoleShipmentMapping();
        mapping1.setShipmentId(999L); // not equal to SHIPMENT_ID
        mapping1.setConsolidationId(200L);

        when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(List.of(mapping1));
        doNothing().when(dependentServiceHelper).pushToKafkaForDownStream(any(), any());

        assertDoesNotThrow(() -> shipmentServiceImplV3.triggerPushToDownStream(shipment, null, true));
    }

    @Test
    void testTriggerPushToDownStream_withEmptyConsolidationList() {
        ShipmentDetails shipment = shipmentDetailsEntity;
        shipment.setId(123L);
        shipment.setConsolidationList(Collections.emptySet());

        shipmentServiceImplV3.triggerPushToDownStream(shipment, null, false);

        ArgumentCaptor<PushToDownstreamEventDto> captor = ArgumentCaptor.forClass(PushToDownstreamEventDto.class);
        verify(dependentServiceHelper).pushToKafkaForDownStream(captor.capture(), eq("123"));

        PushToDownstreamEventDto dto = captor.getValue();
        assertEquals("SHIPMENT", dto.getParentEntityName());
        assertFalse(dto.getMeta().getIsCreate());
        assertNull(dto.getTriggers());
    }

    @Test
    void testTriggerPushToDownStream_withConsolidation_butNoMappings() {
        ShipmentDetails shipment = shipmentDetailsEntity;
        shipment.setId(100L);
        shipment.setConsolidationList(Set.of(consolidationDetailsEntity));
        when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(Collections.emptyList());

        shipmentServiceImplV3.triggerPushToDownStream(shipment, null, true);

        ArgumentCaptor<PushToDownstreamEventDto> captor = ArgumentCaptor.forClass(PushToDownstreamEventDto.class);
        verify(dependentServiceHelper).pushToKafkaForDownStream(captor.capture(), eq("100"));

        PushToDownstreamEventDto dto = captor.getValue();
        assertEquals(100L, dto.getParentEntityId());
        assertTrue(dto.getMeta().getIsCreate());
        assertNull(dto.getTriggers());
    }

    @Test
    void testTriggerPushToDownStream_withSameShipmentIdInMapping() {
        ShipmentDetails shipment = shipmentDetailsEntity;
        shipment.setId(100L);
        shipment.setConsolidationList(Set.of(consolidationDetailsEntity));

        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setShipmentId(100L); // same as current
        mapping.setConsolidationId(200L);

        when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(List.of(mapping));

        shipmentServiceImplV3.triggerPushToDownStream(shipment, null, true);

        ArgumentCaptor<PushToDownstreamEventDto> captor = ArgumentCaptor.forClass(PushToDownstreamEventDto.class);
        verify(dependentServiceHelper).pushToKafkaForDownStream(captor.capture(), eq("100"));

        PushToDownstreamEventDto dto = captor.getValue();
        List<PushToDownstreamEventDto.Triggers> triggers = dto.getTriggers();
        assertEquals(1, triggers.size());
        assertEquals(200L, triggers.get(0).getEntityId());
        assertEquals("CONSOLIDATION", triggers.get(0).getEntityName());
    }

    private Containers createContainer(String code, Long count, Long id) {
        Containers container = new Containers();
        container.setContainerCode(code);
        container.setContainerCount(count);
        container.setId(id);
        return container;
    }

    @Test
    void testSetContainerNumberAndMasterData() {
        ShipmentPacksAssignContainerTrayDto response = new ShipmentPacksAssignContainerTrayDto();
        response.setShipmentsList(List.of(ShipmentPacksAssignContainerTrayDto.Shipments.builder()
                .packsList(List.of(ShipmentPacksAssignContainerTrayDto.Shipments.Packages.builder()
                        .containerNumber("")
                        .containerId(2L)
                        .commodity("123")
                        .build()))
                .build()));
        Set<Long> containerIds = new HashSet<>();
        containerIds.add(1L);
        shipmentServiceImplV3.setContainerNumberAndMasterData(response, containerIds);
        assertNotNull(response);
    }

    @Test
    void testSetContainerNumberAndMasterData1() {
        ShipmentPacksAssignContainerTrayDto response = new ShipmentPacksAssignContainerTrayDto();
        response.setShipmentsList(List.of(ShipmentPacksAssignContainerTrayDto.Shipments.builder()
                .packsList(List.of(ShipmentPacksAssignContainerTrayDto.Shipments.Packages.builder()
                        .containerNumber("")
                        .containerId(2L)
                        .build()))
                .build()));
        Set<Long> containerIds = new HashSet<>();
        containerIds.add(1L);
        ContainerInfoProjection projection = mock(ContainerInfoProjection.class);
        when(packingV3Service.getContainerIdNumberMap(any())).thenReturn(Map.of(2L, projection));
        shipmentServiceImplV3.setContainerNumberAndMasterData(response, containerIds);
        assertNotNull(response);
    }

    @Test
    void testSetContainerNumberAndMasterData2() {
        ShipmentPacksAssignContainerTrayDto response = new ShipmentPacksAssignContainerTrayDto();
        response.setShipmentsList(List.of(ShipmentPacksAssignContainerTrayDto.Shipments.builder()
                .packsList(List.of(ShipmentPacksAssignContainerTrayDto.Shipments.Packages.builder()
                        .containerNumber("")
                        .build()))
                .build()));
        Set<Long> containerIds = new HashSet<>();
        containerIds.add(1L);
        shipmentServiceImplV3.setContainerNumberAndMasterData(response, containerIds);
        assertNotNull(response);
    }

    @Test
    void testSetContainerNumberAndMasterData3() {
        ShipmentPacksAssignContainerTrayDto response = new ShipmentPacksAssignContainerTrayDto();
        response.setShipmentsList(List.of(ShipmentPacksAssignContainerTrayDto.Shipments.builder()
                .packsList(List.of(ShipmentPacksAssignContainerTrayDto.Shipments.Packages.builder()
                        .containerNumber("")
                        .containerId(2L)
                        .commodity("123")
                        .build()))
                .build()));
        Set<Long> containerIds = new HashSet<>();
        containerIds.add(1L);
        when(masterDataUtils.fetchInBulkCommodityTypes(any())).thenReturn(Map.of("123", new EntityTransferCommodityType()));
        shipmentServiceImplV3.setContainerNumberAndMasterData(response, containerIds);
        assertNotNull(response);
    }

    @Test
    void testSetContainerNumberAndMasterData_UnAssign() {
        ShipmentPacksUnAssignContainerTrayDto response = new ShipmentPacksUnAssignContainerTrayDto();
        response.setShipmentsList(List.of(ShipmentPacksUnAssignContainerTrayDto.Shipments.builder()
                .packsList(List.of(ShipmentPacksUnAssignContainerTrayDto.Shipments.Packages.builder()
                        .containerNumber("")
                        .containerId(2L)
                        .commodity("123")
                        .build()))
                .build()));
        Set<Long> containerIds = new HashSet<>();
        containerIds.add(1L);
        shipmentServiceImplV3.setContainerNumberAndMasterData(response, containerIds);
        assertNotNull(response);
    }

    @Test
    void testSetContainerNumberAndMasterData1_UnAssign() {
        ShipmentPacksUnAssignContainerTrayDto response = new ShipmentPacksUnAssignContainerTrayDto();
        response.setShipmentsList(List.of(ShipmentPacksUnAssignContainerTrayDto.Shipments.builder()
                .packsList(List.of(ShipmentPacksUnAssignContainerTrayDto.Shipments.Packages.builder()
                        .containerNumber("")
                        .containerId(2L)
                        .build()))
                .build()));
        Set<Long> containerIds = new HashSet<>();
        containerIds.add(1L);
        ContainerInfoProjection projection = mock(ContainerInfoProjection.class);
        when(packingV3Service.getContainerIdNumberMap(any())).thenReturn(Map.of(2L, projection));
        shipmentServiceImplV3.setContainerNumberAndMasterData(response, containerIds);
        assertNotNull(response);
    }

    @Test
    void testSetContainerNumberAndMasterData2_UnAssign() {
        ShipmentPacksUnAssignContainerTrayDto response = new ShipmentPacksUnAssignContainerTrayDto();
        response.setShipmentsList(List.of(ShipmentPacksUnAssignContainerTrayDto.Shipments.builder()
                .packsList(List.of(ShipmentPacksUnAssignContainerTrayDto.Shipments.Packages.builder()
                        .containerNumber("")
                        .build()))
                .build()));
        Set<Long> containerIds = new HashSet<>();
        containerIds.add(1L);
        shipmentServiceImplV3.setContainerNumberAndMasterData(response, containerIds);
        assertNotNull(response);
    }

    @Test
    void testSetContainerNumberAndMasterData3_UnAssign() {
        ShipmentPacksUnAssignContainerTrayDto response = new ShipmentPacksUnAssignContainerTrayDto();
        response.setShipmentsList(List.of(ShipmentPacksUnAssignContainerTrayDto.Shipments.builder()
                .packsList(new ArrayList<>())
                .build()));
        Set<Long> containerIds = new HashSet<>();
        containerIds.add(1L);
        shipmentServiceImplV3.setContainerNumberAndMasterData(response, containerIds);
        assertNotNull(response);
    }

    @Test
    void testSetContainerNumberAndMasterData4_UnAssign() {
        ShipmentPacksUnAssignContainerTrayDto response = new ShipmentPacksUnAssignContainerTrayDto();
        response.setShipmentsList(List.of(ShipmentPacksUnAssignContainerTrayDto.Shipments.builder()
                .packsList(List.of(ShipmentPacksUnAssignContainerTrayDto.Shipments.Packages.builder()
                        .containerNumber("")
                        .containerId(2L)
                        .commodity("123")
                        .build()))
                .build()));
        Set<Long> containerIds = new HashSet<>();
        containerIds.add(1L);
        when(masterDataUtils.fetchInBulkCommodityTypes(any())).thenReturn(Map.of("123", new EntityTransferCommodityType()));
        shipmentServiceImplV3.setContainerNumberAndMasterData(response, containerIds);
        assertNotNull(response);
    }

    private AdditionalDetails getMockAdditionalDetails(LocalDateTime mockDateTime, Boolean isDocTurnedOverToCustomer,
                                                       Boolean isPickupByConsigneeCompleted,
                                                       Boolean isEmptyContainerReturned) {
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setShipmentId(1L);
        additionalDetails.setPickupDate(mockDateTime);
        additionalDetails.setCargoDeliveredDate(mockDateTime.plusDays(2));
        additionalDetails.setCustomReleaseDate(mockDateTime.plusDays(3));
        additionalDetails.setProofOfDeliveryDate(mockDateTime.plusDays(4));
        additionalDetails.setWarehouseCargoArrivalDate(mockDateTime.plusDays(5));
        additionalDetails.setDocTurnedOverToCustomer(isDocTurnedOverToCustomer);
        additionalDetails.setPickupByConsigneeCompleted(isPickupByConsigneeCompleted);
        additionalDetails.setEmptyContainerReturned(isEmptyContainerReturned);
        return additionalDetails;
    }

    @Test
    void testSetShipmentCargoFields_whenPacksExist_shouldCopyFieldsFromOldShipment() {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(1L);

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setNoOfPacks(10);
        oldShipment.setWeight(BigDecimal.valueOf(100.0));
        oldShipment.setWeightUnit("KG");
        oldShipment.setVolume(BigDecimal.valueOf(20.0));
        oldShipment.setVolumeUnit("CBM");
        oldShipment.setVolumetricWeight(BigDecimal.valueOf(120.0));
        oldShipment.setVolumetricWeightUnit("KG");

        when(packingDao.checkPackingExistsForShipment(1L)).thenReturn(true);

        shipmentServiceImplV3.setShipmentCargoFields(newShipment, oldShipment);

        assertEquals(10, newShipment.getNoOfPacks());
        assertEquals("KG", newShipment.getWeightUnit());
        assertEquals("CBM", newShipment.getVolumeUnit());
        assertEquals("KG", newShipment.getVolumetricWeightUnit());
    }

    @Test
    void testSetShipmentCargoFields_whenPacksNotExist_shouldNotModifyShipment() {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(2L);

        ShipmentDetails oldShipment = new ShipmentDetails();

        when(packingDao.checkPackingExistsForShipment(2L)).thenReturn(false);

        shipmentServiceImplV3.setShipmentCargoFields(newShipment, oldShipment);

        assertNull(newShipment.getNoOfPacks());
        assertNull(newShipment.getWeight());
        assertNull(newShipment.getWeightUnit());
        assertNull(newShipment.getVolume());
        assertNull(newShipment.getVolumeUnit());
        assertNull(newShipment.getVolumetricWeight());
        assertNull(newShipment.getVolumetricWeightUnit());
    }

    @Test
    void getIdFromGuid_success() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        when(shipmentDao.findByGuid(UUID.fromString("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06"))).thenReturn(Optional.of(shipmentDetailsEntity));
        ShipmentDetailsResponse mockShipmentResponse = ShipmentDetailsResponse.builder().id(
                shipmentDetailsEntity.getId()
        ).build();

        ResponseEntity<IRunnerResponse> httpResponse = shipmentServiceImplV3.getIdFromGuid(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void getIdFromGuidCatch() {
        assertEquals(HttpStatus.BAD_REQUEST, shipmentServiceImplV3.getIdFromGuid(null).getStatusCode());
    }

    @Test
    void testAibPushRequest_Success() throws RunnerException {
        ShipmentServiceImplV3 spyService = spy(shipmentServiceImplV3);
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of());
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetailsEntity));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetailsEntity));
        doNothing().when(spyService).sendEmailForPushRequested(any(), any(), any());
        var response = spyService.aibPushRequest(1L, 2L, "");
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testAibPushRequest_Success_Error() throws RunnerException {
        ShipmentServiceImplV3 spyService = spy(shipmentServiceImplV3);
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of(ConsoleShipmentMapping.builder()
                .shipmentId(1L).consolidationId(3L).isAttachmentDone(true).build()));
        var response = spyService.aibPushRequest(1L, 2L, "");
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testAibPushRequest_ExistingMapping() throws RunnerException {
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of(ConsoleShipmentMapping.builder()
                .shipmentId(1L).consolidationId(2L).build()));
        var response = shipmentServiceImplV3.aibPushRequest(1L, 2L, "");
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testAibPushRequest_InterBranchImportShipment() throws RunnerException {
        ShipmentServiceImplV3 spyService = spy(shipmentServiceImplV3);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .direction(Constants.DIRECTION_IMP)
                .build();
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(ConsolidationDetails.builder().build()));
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of());
        when(commonUtils.getEmailTemplates(anyString())).thenReturn(List.of());
        var response = spyService.aibPushRequest(1L, 2L, "");
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(spyService, never()).sendEmailForPushRequested(any(), any(), any());
    }

    @Test
    void testAibPushRequest_InterBranchImportShipment1() throws RunnerException {
        ShipmentServiceImplV3 spyService = spy(shipmentServiceImplV3);
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder()
                .direction(Constants.DIRECTION_IMP)
                .build();
        shipmentDetails1.setCreatedBy("abc");
        shipmentDetails1.setAssignedTo("def");
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails1));
        when(consolidationDetailsDao.findById(2L)).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of());
        when(commonUtils.getEmailTemplates(anyString())).thenReturn(List.of(new EmailTemplatesRequest()));
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        var response = spyService.aibPushRequest(1L, 2L, "");
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(spyService, never()).sendEmailForPushRequested(any(), any(), any());
    }

    @Test
    void testAibPushRequest_InterBranchImportShipment2() throws RunnerException {
        ShipmentServiceImplV3 spyService = spy(shipmentServiceImplV3);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .direction(Constants.DIRECTION_IMP)
                .build();
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of());
        when(commonUtils.getEmailTemplates(anyString())).thenReturn(null);
        var response = spyService.aibPushRequest(1L, 2L, "");
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(spyService, never()).sendEmailForPushRequested(any(), any(), any());
    }

    @Test
    void testAibPushRequest_InterBranchImportShipment3() throws RunnerException {
        ShipmentServiceImplV3 spyService = spy(shipmentServiceImplV3);
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder()
                .direction(Constants.DIRECTION_IMP)
                .build();
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails1));
        when(consolidationDetailsDao.findById(2L)).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of());
        when(commonUtils.getEmailTemplates(anyString())).thenReturn(List.of(new EmailTemplatesRequest()));
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        var response = spyService.aibPushRequest(1L, 2L, "");
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(spyService, never()).sendEmailForPushRequested(any(), any(), any());
    }

    @Test
    void testAibPushRequest_NonInterBranchShipment_EmailSent() throws RunnerException {
        ShipmentServiceImplV3 spyService = spy(shipmentServiceImplV3);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .direction("EXP")
                .build();
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(ConsolidationDetails.builder().build()));
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of());
        doNothing().when(spyService).sendEmailForPushRequested(any(), any(), any());
        var response = spyService.aibPushRequest(1L, 2L, "");
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(consoleShipmentMappingDao).save(argThat(entity ->
                entity.getIsAttachmentDone() != null && !entity.getIsAttachmentDone()
        ));
        verify(spyService).sendEmailForPushRequested(any(), any(), any());
    }

    @Test
    void testUpdateShipments_NonHubRequest_Approve() throws RunnerException {
        ShipmentServiceImplV3 spyService = spy(shipmentServiceImplV3);
        aibActionShipment.setShipmentRequestedType(ShipmentRequestedType.APPROVE);
        aibActionShipment.setConsoleIdsList(List.of(1L));
        aibActionShipment.setShipmentId(1L);
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>(List.of(ConsoleShipmentMapping.builder().build()))));
        doNothing().when(spyService).sendEmailsForPullRequestAccept(any(), any(), any(), any());

        ResponseEntity<IRunnerResponse> response = spyService.aibAction(aibActionShipment);

        verify(consoleShipmentMappingDao).deletePendingStateByShipmentId(1L);
        assertNotNull(response);
    }

    @Test
    void testUpdateShipments_NonHubRequest_Approve_With_Error() throws RunnerException {
        ShipmentServiceImplV3 spyService = spy(shipmentServiceImplV3);
        aibActionShipment.setShipmentRequestedType(ShipmentRequestedType.APPROVE);
        aibActionShipment.setConsoleIdsList(List.of(1L));
        aibActionShipment.setShipmentId(1L);
        when(consolidationV3Service.attachShipments(any())).thenThrow(new RunnerException("TEST"));

        var exception = assertThrows(ValidationException.class, () -> {
            spyService.aibAction(aibActionShipment);
        });
        assertNotNull(exception);
        assertEquals("TEST", exception.getMessage());
    }

    @Test
    void testUpdateShipments_NonHubRequest_Reject() throws RunnerException {
        ShipmentServiceImplV3 spyService = spy(shipmentServiceImplV3);
        aibActionShipment.setShipmentRequestedType(ShipmentRequestedType.REJECT);
        aibActionShipment.setConsoleIdsList(List.of(1L));
        aibActionShipment.setShipmentId(1L);
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(ConsoleShipmentMapping.builder().build())));
        doNothing().when(spyService).sendEmailForPullRequestReject(any(), any(), any(), any(), any());

        ResponseEntity<IRunnerResponse> response = spyService.aibAction(aibActionShipment);

        verify(consoleShipmentMappingDao).deletePendingStateByConsoleIdAndShipmentId(1L, 1L);
        assertNotNull(response);
    }

    @Test
    void testUpdateShipments_NonHubRequest_Withdraw() throws RunnerException {
        aibActionShipment.setShipmentRequestedType(ShipmentRequestedType.WITHDRAW);
        aibActionShipment.setConsoleIdsList(List.of(1L));
        aibActionShipment.setShipmentId(1L);
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(ConsoleShipmentMapping.builder().build())));
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetailsEntity));
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);

        ResponseEntity<IRunnerResponse> response = shipmentServiceImplV3.aibAction(aibActionShipment);

        verify(consoleShipmentMappingDao).deletePendingStateByConsoleIdAndShipmentId(1L, 1L);
        assertNotNull(response);
    }

    @Test
    void testUpdateShipments_NonHubRequest_InvalidDataException() {
        aibActionShipment.setConsoleIdsList(null); // Invalid data
        aibActionShipment.setShipmentRequestedType(ShipmentRequestedType.APPROVE);
        aibActionShipment.setShipmentId(1L);

        InvalidDataAccessApiUsageException exception = assertThrows(InvalidDataAccessApiUsageException.class, () -> {
            shipmentServiceImplV3.aibAction(aibActionShipment);
        });

        assertEquals("Console Ids list should not be empty!!!", exception.getMessage());
    }

    @Test
    void testUpdateShipments_NonHubRequest_InvalidDataException2() {
        aibActionShipment.setConsoleIdsList(List.of()); // Invalid data
        aibActionShipment.setShipmentRequestedType(ShipmentRequestedType.APPROVE);
        aibActionShipment.setShipmentId(1L);

        InvalidDataAccessApiUsageException exception = assertThrows(InvalidDataAccessApiUsageException.class, () -> {
            shipmentServiceImplV3.aibAction(aibActionShipment);
        });

        assertEquals("Console Ids list should not be empty!!!", exception.getMessage());
    }

    @Test
    void sendEmailsForPullRequestAccept() throws Exception {
        ShipmentServiceImplV3 spyService = spy(shipmentServiceImplV3);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetailsEntity));
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetailsEntity);
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consolidationDetailsEntity)));
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        spyService.sendEmailsForPullRequestAccept(1L, 2L, new HashSet<>(), new ArrayList<>());
        verify(commonUtils).sendEmailForPullPushRequestStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullRequestReject() throws Exception {
        ShipmentServiceImplV3 spyService = spy(shipmentServiceImplV3);
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consolidationDetailsEntity)));
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetailsEntity));
        ConsoleShipmentMapping consoleShipmentMapping = ConsoleShipmentMapping.builder().shipmentId(1L).consolidationId(2L).build();
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        spyService.sendEmailForPullRequestReject(1L, List.of(2L), new HashSet<>(), "rejectRemarks", List.of(consoleShipmentMapping));
        verify(commonUtils).sendEmailForPullPushRequestStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void sendEmailForPushRequested() throws Exception {
        ShipmentServiceImplV3 spyService = spy(shipmentServiceImplV3);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetailsEntity));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetailsEntity));
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        spyService.sendEmailForPushRequested(1L, 2L, new HashSet<>());
        verify(commonUtils).sendEmailForPullPushRequestStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void attachListShipment_success() {
        // Mock data
        long consolidationId = 1L;
        AttachListShipmentRequest attachListShipmentRequest = new AttachListShipmentRequest();
        attachListShipmentRequest.setConsolidationId(consolidationId);
        attachListShipmentRequest.setEtdMatch(true); // Set other properties as needed
        attachListShipmentRequest.setEtaMatch(false);
        attachListShipmentRequest.setScheduleMatch(true);
        attachListShipmentRequest.setFilterCriteria(new ArrayList<>());
        attachListShipmentRequest.setIncludeColumns(List.of("shipmentId"));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(attachListShipmentRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCarrierDetails(new CarrierDetails());
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));


        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        // Set up shipmentDetailsList as needed for your test

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);


        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );
        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentServiceImplV3.attachListShipment(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, result);
    }

    @Test
    void attachListShipment_success_SEA_FCL() {
        // Mock data
        long consolidationId = 1L;
        AttachListShipmentRequest attachListShipmentRequest = new AttachListShipmentRequest();
        attachListShipmentRequest.setConsolidationId(consolidationId);
        attachListShipmentRequest.setEtdMatch(true); // Set other properties as needed
        attachListShipmentRequest.setEtaMatch(false);
        attachListShipmentRequest.setScheduleMatch(true);
        attachListShipmentRequest.setFilterCriteria(new ArrayList<>());
        attachListShipmentRequest.setIncludeColumns(List.of("shipmentId"));
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(attachListShipmentRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        consolidationDetails.setShipmentsList(Set.of(shipmentDetails));
        consolidationDetails.setCarrierDetails(new CarrierDetails());
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));


        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        // Set up shipmentDetailsList as needed for your test

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);


        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );
        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentServiceImplV3.attachListShipment(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, result);
    }

    @Test
    void attachListShipment_success_SEA_LCL() {
        // Mock data
        long consolidationId = 1L;
        AttachListShipmentRequest attachListShipmentRequest = new AttachListShipmentRequest();
        attachListShipmentRequest.setConsolidationId(consolidationId);
        attachListShipmentRequest.setEtdMatch(true); // Set other properties as needed
        attachListShipmentRequest.setEtaMatch(false);
        attachListShipmentRequest.setScheduleMatch(true);
        attachListShipmentRequest.setFilterCriteria(new ArrayList<>());
        attachListShipmentRequest.setIncludeColumns(List.of("shipmentId"));
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(attachListShipmentRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        consolidationDetails.setShipmentsList(Set.of(shipmentDetails));
        consolidationDetails.setCarrierDetails(new CarrierDetails());
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));


        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        // Set up shipmentDetailsList as needed for your test

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);


        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );
        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentServiceImplV3.attachListShipment(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, result);
    }

    @Test
    void attachListShipmentEtaNotNull_success() {
        // Mock data
        long consolidationId = 1L;
        AttachListShipmentRequest attachListShipmentRequest = new AttachListShipmentRequest();
        attachListShipmentRequest.setConsolidationId(consolidationId);
        attachListShipmentRequest.setEtdMatch(false); // Set other properties as needed
        attachListShipmentRequest.setEtaMatch(true);
        attachListShipmentRequest.setScheduleMatch(true);
        attachListShipmentRequest.setFilterCriteria(new ArrayList<>());
        attachListShipmentRequest.setIncludeColumns(List.of("shipmentId"));
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(attachListShipmentRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCarrierDetails(CarrierDetails.builder().originPort(Constants.ORIGIN_PORT).destinationPort(Constants.DESTINATION_PORT).eta(LocalDateTime.now()).etd(LocalDateTime.now()).vessel(Constants.VESSEL).voyage(Constants.VOYAGE).build());
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));


        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        // Set up shipmentDetailsList as needed for your test

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);


        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );
        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentServiceImplV3.attachListShipment(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, result);
    }

    @Test
    void attachListShipmentEtaNotNullTransportModeNotAir_success() {
        // Mock data
        long consolidationId = 1L;
        AttachListShipmentRequest attachListShipmentRequest = new AttachListShipmentRequest();
        attachListShipmentRequest.setConsolidationId(consolidationId);
        attachListShipmentRequest.setEtdMatch(false); // Set other properties as needed
        attachListShipmentRequest.setEtaMatch(true);
        attachListShipmentRequest.setScheduleMatch(true);
        attachListShipmentRequest.setFilterCriteria(new ArrayList<>());
        attachListShipmentRequest.setIncludeColumns(List.of("shipmentId"));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(attachListShipmentRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCarrierDetails(CarrierDetails.builder().eta(LocalDateTime.now()).etd(LocalDateTime.now()).shippingLine(Constants.SHIPPING_LINE).flightNumber(Constants.FLIGHT_NUMBER).build());
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirDGFlag(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));


        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        // Set up shipmentDetailsList as needed for your test

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);


        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );
        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentServiceImplV3.attachListShipment(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, result);
    }

    private List<IRunnerResponse> convertEntityListToDtoListForAttachListShipment(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<AttachListShipmentResponse> attachListShipmentResponse = AttachListShipmentMapper.INSTANCE.toAttachListShipmentResponse(lst);
        for (var i : attachListShipmentResponse) {
            if (i.getStatus() != null && i.getStatus() < ShipmentStatus.values().length)
                i.setShipmentStatus(ShipmentStatus.values()[i.getStatus()].toString());
            responseList.add(i);
        }

        return responseList;
    }

    @Test
    void testGetPendingNotificationsSuccess() {
        AibNotificationRequest request = new AibNotificationRequest();
        request.setId(1L);

        ConsolidationDetails mockConsol1 = jsonTestUtility.getTestConsolidation();
        mockConsol1.setTenantId(1);
        mockConsol1.setId(1L);
        var mockConsol2 = objectMapper.convertValue(mockConsol1, ConsolidationDetails.class);
        mockConsol2.setId(2L);
        var mockConsol3 = objectMapper.convertValue(mockConsol1, ConsolidationDetails.class);
        mockConsol3.setId(3L);

        V1TenantSettingsResponse tenantSettingsResponse = new V1TenantSettingsResponse();
        tenantSettingsResponse.setIsMAWBColoadingEnabled(true);
        tenantSettingsResponse.setIsColoadingMAWBStationEnabled(true);

        List<ConsoleShipmentMapping> mappings = new ArrayList<>();
        mappings.add(ConsoleShipmentMapping.builder()
                .shipmentId(1L).consolidationId(1L).requestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED).isAttachmentDone(false).build());
        mappings.add(ConsoleShipmentMapping.builder()
                .shipmentId(1L).consolidationId(2L).requestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED).isAttachmentDone(false).build());
        mappings.add(ConsoleShipmentMapping.builder()
                .shipmentId(2L).consolidationId(1L).requestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED).isAttachmentDone(false).build());
        mappings.add(ConsoleShipmentMapping.builder()
                .shipmentId(2L).consolidationId(3L).requestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED).isAttachmentDone(false).build());

        List<ConsolidationDetails> consolidationDetailsList = List.of(mockConsol1, mockConsol2, mockConsol3);

        // mocking
        TenantSettingsDetailsContext.setCurrentTenantSettings(tenantSettingsResponse);
        mockTenantSettings();
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(mappings));
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(consolidationDetailsList));
        // Test
        var httpResponse = shipmentServiceImplV3.aibPendingNotification(CommonRequestModel.buildRequest(request));
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        var runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        JavaType javaType = objectMapper.getTypeFactory().constructParametricType(PendingNotificationResponse.class, PendingShipmentActionsResponse.class);
        PendingNotificationResponse<PendingShipmentActionsResponse> responseBody = objectMapper.convertValue(runnerResponse.getData(), javaType);
        assertEquals(2, responseBody.getNotificationMap().size()); // number of shipments with pending notifications
        assertEquals(2, responseBody.getNotificationMap().get(1L).size()); // notification count of shipment with id 1L
    }

    @Test
    void testCreateShipmentFromEntityTransfer() throws RunnerException, NoSuchFieldException, JsonProcessingException,
            InvocationTargetException, IllegalAccessException, NoSuchMethodException {

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(true);
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setMasterBill(null);
        AdditionalDetails additionalDetails = getMockAdditionalDetails(LocalDateTime.now(), true, true, true);
        CarrierDetails mockCarrierDetails = mockShipment.getCarrierDetails();
        mockCarrierDetails.setEta(LocalDateTime.now());
        mockCarrierDetails.setEtd(LocalDateTime.now());
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        mockShipment.setContainersList(Set.of(new Containers()));

        ShipmentEtV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentEtV3Request.class);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);
        mockShipmentRequest.setIsChargableEditable(true);
        mockShipment.setId(2L);
        mockShipmentSettings();
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        when(shipmentsV3Util.generateShipmentId(any())).thenReturn("ShipmentId");
        when(shipmentDao.save(any(), anyBoolean())).thenReturn(mockShipment);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);
        doNothing().when(auditLogService).addAuditLog(any());
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        ShipmentDetailsResponse actualResponse = shipmentServiceImplV3.createShipmentFromEntityTransfer(mockShipmentRequest, true);
        assertNotNull(actualResponse);
        assertEquals(mockShipmentResponse, actualResponse);

    }

    @Test
    void testCreateShipmentFromEntityTransfer2() throws RunnerException, NoSuchFieldException, JsonProcessingException,
            InvocationTargetException, IllegalAccessException, NoSuchMethodException {

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(true);
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setMasterBill(null);
        AdditionalDetails additionalDetails = getMockAdditionalDetails(LocalDateTime.now(), true, true, true);
        CarrierDetails mockCarrierDetails = mockShipment.getCarrierDetails();
        mockCarrierDetails.setEta(LocalDateTime.now());
        mockCarrierDetails.setEtd(LocalDateTime.now());
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ShipmentEtV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentEtV3Request.class);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);
        mockShipmentRequest.setIsChargableEditable(true);
        mockShipment.setId(2L);
        mockShipmentSettings();
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        when(shipmentsV3Util.generateShipmentId(any())).thenReturn("ShipmentId");
        when(shipmentDao.save(any(), anyBoolean())).thenReturn(mockShipment);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);
        doNothing().when(auditLogService).addAuditLog(any());
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        ShipmentDetailsResponse actualResponse = shipmentServiceImplV3.createShipmentFromEntityTransfer(mockShipmentRequest, true);
        assertNotNull(actualResponse);
        assertEquals(mockShipmentResponse, actualResponse);

    }

    @Test
    void testCreateShipmentFromEntityTransfer3() throws RunnerException {

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(true);
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setMasterBill(null);
        AdditionalDetails additionalDetails = getMockAdditionalDetails(LocalDateTime.now(), true, true, true);
        CarrierDetails mockCarrierDetails = mockShipment.getCarrierDetails();
        mockCarrierDetails.setEta(LocalDateTime.now());
        mockCarrierDetails.setEtd(LocalDateTime.now());
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ShipmentEtV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentEtV3Request.class);
        mockShipmentRequest.setIsChargableEditable(true);
        mockShipment.setId(2L);
        mockShipmentSettings();
        when(shipmentsV3Util.generateShipmentId(any())).thenReturn("ShipmentId");
        when(shipmentDao.save(any(), anyBoolean())).thenReturn(mockShipment);

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        doThrow(RunnerException.class).when(shipmentsV3Util).afterSaveforEt(any(), any(), eq(true), any(), any(), eq(true));
        assertThrows(ValidationException.class, () -> shipmentServiceImplV3.createShipmentFromEntityTransfer(mockShipmentRequest, true));

    }

    @Test
    void testCreateShipmentFromEntityTransfer4() throws RunnerException, NoSuchFieldException, JsonProcessingException,
            InvocationTargetException, IllegalAccessException, NoSuchMethodException {

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(true);
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setMasterBill(null);
        AdditionalDetails additionalDetails = getMockAdditionalDetails(LocalDateTime.now(), true, true, true);
        CarrierDetails mockCarrierDetails = mockShipment.getCarrierDetails();
        mockCarrierDetails.setEta(LocalDateTime.now());
        mockCarrierDetails.setEtd(LocalDateTime.now());
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        mockShipment.setContainersList(Set.of(new Containers()));

        ShipmentEtV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentEtV3Request.class);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);
        mockShipmentRequest.setIsChargableEditable(true);
        mockShipment.setId(2L);
        mockShipmentSettings();
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        when(shipmentsV3Util.generateShipmentId(any())).thenReturn("ShipmentId");
        when(shipmentDao.save(any(), anyBoolean())).thenReturn(mockShipment);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);

        doThrow(IllegalAccessException.class).when(auditLogService).addAuditLog(any());

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        ShipmentDetailsResponse actualResponse = shipmentServiceImplV3.createShipmentFromEntityTransfer(mockShipmentRequest, true);
        assertNotNull(actualResponse);
        assertEquals(mockShipmentResponse, actualResponse);

    }

    @Test
    void testCompleteUpdateShipmentFromEntityTransfer_success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(true);
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setMasterBill(null);
        mockShipment.setShipmentId("SHIP001");
        AdditionalDetails additionalDetails = getMockAdditionalDetails(LocalDateTime.now(), true, true, true);
        CarrierDetails mockCarrierDetails = mockShipment.getCarrierDetails();
        mockCarrierDetails.setEta(LocalDateTime.now());
        mockCarrierDetails.setEtd(LocalDateTime.now());
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ShipmentEtV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentEtV3Request.class);

        ShipmentV3Request mockShipmentRequest2 = objectMapper.convertValue(mockShipment, ShipmentV3Request.class);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);
        mockShipmentRequest.setIsChargableEditable(true);
        mockShipmentRequest.setId(1L);
        commonRequestModel.setData(mockShipmentRequest);
        mockShipment.setId(2L);

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(mockShipment));
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);

        when(objectMapperMocked.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);

        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        when(shipmentDao.update(any(), anyBoolean())).thenReturn(mockShipment);
        when(jsonHelper.convertValue(any(), eq(ShipmentV3Request.class))).thenReturn(mockShipmentRequest2);
        doNothing().when(auditLogService).addAuditLog(any());
        when(jsonHelper.convertToJson(any())).thenReturn("Shipment");

        when(shipmentDetailsMapper.map(any(ShipmentDetails.class))).thenReturn(mockShipmentResponse);

        ShipmentDetailsResponse actualResponse = shipmentServiceImplV3.completeUpdateShipmentFromEntityTransfer(mockShipmentRequest);

        assertNotNull(actualResponse);
        assertEquals(mockShipmentResponse, actualResponse);
    }

    @Test
    void testCompleteUpdateShipmentFromEntityTransfer_Exception() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(true);
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setMasterBill(null);
        mockShipment.setShipmentId("SHIP001");
        AdditionalDetails additionalDetails = getMockAdditionalDetails(LocalDateTime.now(), true, true, true);
        CarrierDetails mockCarrierDetails = mockShipment.getCarrierDetails();
        mockCarrierDetails.setEta(LocalDateTime.now());
        mockCarrierDetails.setEtd(LocalDateTime.now());
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ShipmentEtV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentEtV3Request.class);

        ShipmentV3Request mockShipmentRequest2 = objectMapper.convertValue(mockShipment, ShipmentV3Request.class);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        mockShipmentRequest.setIsChargableEditable(true);
        mockShipmentRequest.setId(1L);
        commonRequestModel.setData(mockShipmentRequest);
        mockShipment.setId(2L);

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.empty());

        when(jsonHelper.convertValue(any(), eq(ShipmentV3Request.class))).thenReturn(mockShipmentRequest2);

        assertThrows(DataRetrievalFailureException.class, () -> shipmentServiceImplV3.completeUpdateShipmentFromEntityTransfer(mockShipmentRequest));
    }

    @Test
    void testCompleteUpdateShipmentFromEntityTransfer_Exception2() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(true);
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setMasterBill(null);
        mockShipment.setShipmentId("SHIP001");
        AdditionalDetails additionalDetails = getMockAdditionalDetails(LocalDateTime.now(), true, true, true);
        CarrierDetails mockCarrierDetails = mockShipment.getCarrierDetails();
        mockCarrierDetails.setEta(LocalDateTime.now());
        mockCarrierDetails.setEtd(LocalDateTime.now());
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ShipmentEtV3Request mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentEtV3Request.class);

        ShipmentV3Request mockShipmentRequest2 = objectMapper.convertValue(mockShipment, ShipmentV3Request.class);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        mockShipmentRequest.setIsChargableEditable(true);
        mockShipmentRequest.setId(1L);
        commonRequestModel.setData(mockShipmentRequest);
        mockShipment.setId(2L);

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(mockShipment));
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);

        when(objectMapperMocked.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        when(shipmentDao.update(any(), anyBoolean())).thenReturn(mockShipment);
        when(jsonHelper.convertValue(any(), eq(ShipmentV3Request.class))).thenReturn(mockShipmentRequest2);
        doNothing().when(auditLogService).addAuditLog(any());
        when(jsonHelper.convertToJson(any())).thenReturn("Shipment");

        doThrow(RunnerException.class).when(shipmentsV3Util).afterSaveforEt(any(), any(), eq(false), any(), any(), eq(false));
        assertThrows(ValidationException.class, () -> shipmentServiceImplV3.completeUpdateShipmentFromEntityTransfer(mockShipmentRequest));
    }


    @Test
    void testGetPendingNotificationsReturnsEmptyResponseIfTenantSettingsNotEnabled() {
        AibNotificationRequest request = new AibNotificationRequest();
        request.setId(1L);

        V1TenantSettingsResponse tenantSettingsResponse = new V1TenantSettingsResponse();
        TenantSettingsDetailsContext.setCurrentTenantSettings(tenantSettingsResponse);

        // Mock
        mockTenantSettings();
        // Test
        var httpResponse = shipmentServiceImplV3.aibPendingNotification(CommonRequestModel.buildRequest(request));
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        var responseBody = objectMapper.convertValue(httpResponse.getBody(), PendingNotificationResponse.class);
        assertNull(responseBody.getNotificationMap());
    }

    @Test
    void testGetPendingNotificationsReturnsEmptyResponseForEmptyList() {
        AibNotificationRequest request = new AibNotificationRequest();
        request.setId(null);

        var httpResponse = shipmentServiceImplV3.aibPendingNotification(CommonRequestModel.buildRequest(request));

        PendingNotificationResponse mockResponse = new PendingNotificationResponse();

        assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);
    }

    @Test
    void testUpdateContainerFromCargo() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(1L);
        newShipment.setContainerAssignedToShipmentCargo(1L);
        newShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setNoOfPacks(10);
        oldShipment.setWeight(BigDecimal.valueOf(100.0));
        oldShipment.setWeightUnit("KG");
        oldShipment.setVolume(BigDecimal.valueOf(20.0));
        oldShipment.setVolumeUnit("CBM");
        oldShipment.setVolumetricWeight(BigDecimal.valueOf(120.0));
        oldShipment.setVolumetricWeightUnit("KG");

        shipmentServiceImplV3.updateContainerFromCargo(newShipment, oldShipment);
        verify(containerV3Service).updateAttachedContainersData(List.of(1L));
    }

    @Test
    void testUpdateContainerFromCargo1() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(1L);
        newShipment.setContainerAssignedToShipmentCargo(1L);
        newShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setWeight(BigDecimal.valueOf(100.0));
        oldShipment.setWeightUnit("KG");
        oldShipment.setVolume(BigDecimal.valueOf(20.0));
        oldShipment.setVolumeUnit("CBM");
        oldShipment.setVolumetricWeight(BigDecimal.valueOf(120.0));
        oldShipment.setVolumetricWeightUnit("KG");

        shipmentServiceImplV3.updateContainerFromCargo(newShipment, oldShipment);
        verify(containerV3Service).updateAttachedContainersData(List.of(1L));
    }

    @Test
    void testUpdateContainerFromCargo2() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(1L);
        newShipment.setContainerAssignedToShipmentCargo(1L);
        newShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setWeightUnit("KG");
        oldShipment.setVolume(BigDecimal.valueOf(20.0));
        oldShipment.setVolumeUnit("CBM");
        oldShipment.setVolumetricWeight(BigDecimal.valueOf(120.0));
        oldShipment.setVolumetricWeightUnit("KG");

        shipmentServiceImplV3.updateContainerFromCargo(newShipment, oldShipment);
        verify(containerV3Service).updateAttachedContainersData(List.of(1L));
    }

    @Test
    void testUpdateContainerFromCargo3() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(1L);
        newShipment.setContainerAssignedToShipmentCargo(1L);
        newShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setVolume(BigDecimal.valueOf(20.0));
        oldShipment.setVolumeUnit("CBM");
        oldShipment.setVolumetricWeight(BigDecimal.valueOf(120.0));
        oldShipment.setVolumetricWeightUnit("KG");

        shipmentServiceImplV3.updateContainerFromCargo(newShipment, oldShipment);
        verify(containerV3Service).updateAttachedContainersData(List.of(1L));
    }

    @Test
    void testUpdateContainerFromCargo4() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(1L);
        newShipment.setContainerAssignedToShipmentCargo(1L);
        newShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setVolumeUnit("CBM");
        oldShipment.setVolumetricWeight(BigDecimal.valueOf(120.0));
        oldShipment.setVolumetricWeightUnit("KG");

        shipmentServiceImplV3.updateContainerFromCargo(newShipment, oldShipment);
        verify(containerV3Service).updateAttachedContainersData(List.of(1L));
    }

    @Test
    void testUpdateContainerFromCargo5() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(1L);
        newShipment.setContainerAssignedToShipmentCargo(1L);
        newShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ShipmentDetails oldShipment = new ShipmentDetails();
        oldShipment.setVolumetricWeight(BigDecimal.valueOf(120.0));
        oldShipment.setVolumetricWeightUnit("KG");

        shipmentServiceImplV3.updateContainerFromCargo(newShipment, oldShipment);
        verify(containerV3Service, times(0)).updateAttachedContainersData(List.of(1L));
    }

    @Test
    void testSendOceanDGApprovalEmail_NullObject() {
        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentServiceImplV3.sendOceanDGApprovalEmail(null);
        });
    }

    @Test
    void testSendOceanDGApprovalEmail_shipmentNotFound() {
        OceanDGApprovalRequest request = OceanDGApprovalRequest
                .builder()
                .shipmentId(1l)
                .remarks("Shipment_Not_Found")
                .build();

        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentServiceImplV3.sendOceanDGApprovalEmail(request);
        });
    }

    @Test
    void testSendOceanDGApprovalEmail() throws RunnerException {
        try (MockedStatic<UserContext> userContextMockedStatic = Mockito.mockStatic(
                UserContext.class)) {
            OceanDGApprovalRequest request = OceanDGApprovalRequest
                    .builder()
                    .shipmentId(1l)
                    .remarks("Non_DG_USER")
                    .build();

            Packing packing = new Packing();
            packing.setHazardous(true);
            packing.setDGClass("1.2");
            ShipmentDetails shipmentDetails = ShipmentDetails
                    .builder()
                    .oceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED)
                    .packingList(List.of(packing))
                    .build();

            when(shipmentDao.findById(request.getShipmentId())).thenReturn(
                    Optional.ofNullable(shipmentDetails));


            UsersDto user = UsersDto.builder().build();
            userContextMockedStatic.when(UserContext::getUser).thenReturn(user);
            userContextMockedStatic.when(UserContext::isOceanDgUser).thenReturn(false);

            when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
            Integer roleId = 1;
            List<String> users = new ArrayList<>();
            users.add("abc@email.com");
            when(commonUtils.getRoleId(any())).thenReturn(roleId);
            when(commonUtils.getUserEmailsByRoleId(any())).thenReturn(users);
            when(commonUtils.createTaskMDM(any(), any())).thenReturn(new TaskCreateResponse());

            assertThrows(RunnerException.class, () -> shipmentServiceImplV3.sendOceanDGApprovalEmail(request));
        }
    }


    @Test
    void testSendOceanDGApprovalEmail_Commercial() throws RunnerException {
        try (MockedStatic<UserContext> userContextMockedStatic = Mockito.mockStatic(
                UserContext.class)) {
            OceanDGApprovalRequest request = OceanDGApprovalRequest
                    .builder()
                    .shipmentId(1l)
                    .remarks("Non_DG_USER")
                    .build();

            ShipmentDetails shipmentDetails = ShipmentDetails
                    .builder()
                    .oceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED)
                    .containersList(Set.of(Containers.builder().hazardous(true).dgClass("1.2").build()))
                    .build();

            when(shipmentDao.findById(request.getShipmentId())).thenReturn(
                    Optional.ofNullable(shipmentDetails));


            UsersDto user = UsersDto.builder().build();
            userContextMockedStatic.when(UserContext::getUser).thenReturn(user);
            userContextMockedStatic.when(UserContext::isOceanDgUser).thenReturn(false);

            when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
            Integer roleId = 1;
            List<String> users = new ArrayList<>();
            users.add("abc@email.com");
            TaskCreateResponse taskCreateResponse = TaskCreateResponse.builder().build();
            when(commonUtils.getRoleId(any())).thenReturn(roleId);
            when(commonUtils.getUserEmailsByRoleId(any())).thenReturn(users);
            lenient().when(commonUtils.createTask(any(), any())).thenReturn(taskCreateResponse);

            assertThrows(RunnerException.class, () -> shipmentServiceImplV3.sendOceanDGApprovalEmail(request));
            verify(shipmentDao).findById(any());
        }
    }

    @Test
    void testSendOceanDGApprovalEmail_DgUser() throws RunnerException {
        try (MockedStatic<UserContext> userContextMockedStatic = Mockito.mockStatic(
                UserContext.class)) {
            OceanDGApprovalRequest request = OceanDGApprovalRequest
                    .builder()
                    .shipmentId(1l)
                    .remarks("Non_DG_USER")
                    .build();

            Packing packing = new Packing();
            packing.setHazardous(true);
            packing.setDGClass("1.23");

            ShipmentDetails shipmentDetails = ShipmentDetails
                    .builder()
                    .oceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED)
                    .containersList(Set.of(Containers.builder().hazardous(true).dgClass("2.1").build()))
                    .packingList(List.of(packing))
                    .build();

            when(shipmentDao.findById(request.getShipmentId())).thenReturn(
                    Optional.ofNullable(shipmentDetails));

            UsersDto user = UsersDto.builder().build();
            userContextMockedStatic.when(UserContext::getUser).thenReturn(user);
            userContextMockedStatic.when(UserContext::isOceanDgUser).thenReturn(true);

            shipmentServiceImplV3.sendOceanDGApprovalEmail(request);
            verify(shipmentDao).findById(any());
        }
    }

    @Test
    void testSendOceanDGApprovalEmail_Imp() throws RunnerException {
        OceanDGApprovalRequest request = OceanDGApprovalRequest
                .builder()
                .shipmentId(1l)
                .remarks("")
                .build();
        ShipmentDetails shipmentDetails = ShipmentDetails
                .builder()
                .oceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED)
                .containersList(Set.of(Containers.builder().hazardous(true).dgClass("2.1").build()))
                .direction(Constants.IMP)
                .build();

        when(shipmentDao.findById(request.getShipmentId())).thenReturn(
                Optional.ofNullable(shipmentDetails));
        shipmentServiceImplV3.sendOceanDGApprovalEmail(request);
        verify(shipmentDao).findById(any());
    }

    @Test
    void testDgApprovalResponse_Imp() throws RunnerException {
        OceanDGRequestV3 request = OceanDGRequestV3
                .builder()
                .shipmentId(1l)
                .remarks("")
                .build();
        ShipmentDetails shipmentDetails = ShipmentDetails
                .builder()
                .oceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED)
                .containersList(Set.of(Containers.builder().hazardous(true).dgClass("2.1").build()))
                .direction(Constants.IMP)
                .build();

        when(shipmentDao.findById(request.getShipmentId())).thenReturn(
                Optional.ofNullable(shipmentDetails));
        shipmentServiceImplV3.dgApprovalResponse(request);
        verify(shipmentDao).findById(any());
    }

    @Test
    void testDgApprovalResponse_NullRequest() {
        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentServiceImplV3.dgApprovalResponse(null);
        });
    }

    @Test
    void testDgApprovalResponse_ShipmentNotFound() {
        OceanDGRequestV3 request = OceanDGRequestV3.builder().shipmentId(1l).build();

        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentServiceImplV3.dgApprovalResponse(request);
        });
    }

    @Test
    void testDgApprovalResponse_InvalidDGStatus() {
        OceanDGRequestV3 request = OceanDGRequestV3.builder().shipmentId(1l).build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().oceanDGStatus(OceanDGStatus.APPROVE).build();
        when(shipmentDao.findById(request.getShipmentId())).thenReturn(
                Optional.ofNullable(shipmentDetails));

        assertThrows(RunnerException.class, () -> {
            shipmentServiceImplV3.dgApprovalResponse(request);
        });
        verify(shipmentDao).findById(any());
    }

    @Test
    void testDgApprovalResponse_ValidDgApprove() throws RunnerException {
        OceanDGRequestV3 request = OceanDGRequestV3.builder().shipmentId(1l).status(TaskStatus.APPROVED).build();

        Packing packing = new Packing();
        packing.setHazardous(true);
        packing.setDGClass("1.23");

        ShipmentDetails shipmentDetails = ShipmentDetails
                .builder()
                .oceanDGStatus(OceanDGStatus.OCEAN_DG_REQUESTED)
                .containersList(Set.of(Containers.builder().hazardous(true).dgClass("2.1").build()))
                .packingList(List.of(packing))
                .build();
        when(shipmentDao.findById(request.getShipmentId())).thenReturn(
                Optional.ofNullable(shipmentDetails));

        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);

        when(mdmServiceAdapter.getTaskList(any(), any(), any(), any())).thenReturn(new ArrayList<>());
        String response = shipmentServiceImplV3.dgApprovalResponse(request);
        assertNotNull(response);
    }

    @Test
    void generateEmailBody_test1() {
        String htmlTemplate = """
                <table border="1" cellpadding="1" cellspacing="1" style="width:500px">
                       <tbody>
                           <tr>
                               <td>&nbsp;No &amp; Type of Package&nbsp;</td>
                               <td>Container Number</td>
                               <td>&nbsp;DG Class</td>
                               <td>UN Number</td>
                               <td>Proper Shipping Name</td>
                               <td>Packing Group&nbsp;</td>
                               <td>Minimum Flash Point</td>
                               <td>Marine Pollutant</td>
                           </tr>
                           <tr>
                               <td>{#PACKAGE_DETAILS}</td>
                               <td>{#CONTAINER_NUMBER}&nbsp;</td>
                               <td>{#DG_CLASS}</td>
                               <td>{#UN_NUMBER}</td>
                               <td>{#SHIPPING_NAME}</td>
                               <td>{#PACKING_GROUP}</td>
                               <td>{#FLASH_POINT}&nbsp;</td>
                               <td>{#MARINE_POLLUTANT}</td>
                           </tr>
                       </tbody>
                   </table>
                """;

        String emailBody = "EmailBody";
        when(commonUtils.replaceTagsFromData(anyMap(), anyString())).thenReturn(emailBody);
        String result = shipmentServiceImplV3.generateEmailBody(new HashMap<>(), shipmentDetailsEntity, htmlTemplate);
        assertEquals(emailBody, result);
    }

    @Test
    void generateEmailBody_test2() {
        String htmlTemplate = """
                    <table border="1" cellpadding="1" cellspacing="1" style="width:500px">
                    <tbody>
                        <tr>
                            <td>&nbsp;No &amp; Type of Package&nbsp;</td>
                            <td>Container Number</td>
                            <td>&nbsp;DG Class</td>
                            <td>UN Number</td>
                            <td>Proper Shipping Name</td>
                            <td>Packing Group&nbsp;</td>
                            <td>Minimum Flash Point</td>
                            <td>Marine Pollutant</td>
                        </tr>
                        <tr>
                            <td>#PACKAGE_DETAILS}</td>
                            <td>{#CONTAINER_NUMBER}&nbsp;</td>
                            <td>{#DG_CLASS}</td>
                            <td>{#UN_NUMBER}</td>
                            <td>{#SHIPPING_NAME}</td>
                            <td>{#PACKING_GROUP}</td>
                            <td>{#FLASH_POINT}&nbsp;</td>
                            <td>{#MARINE_POLLUTANT}</td>
                        </tr>
                    </tbody>
                </table>
                """;

        String emailBody = "EmailBody";
        when(commonUtils.replaceTagsFromData(anyMap(), anyString())).thenReturn(emailBody);
        Containers containers = new Containers();
        containers.setHazardous(true);
        shipmentDetailsEntity.setContainersList(Set.of(containers));

        Packing packing = new Packing();
        packing.setHazardous(true);

        shipmentDetailsEntity.setPackingList(List.of(packing));

        String result = shipmentServiceImplV3.generateEmailBody(new HashMap<>(), shipmentDetailsEntity, htmlTemplate);
        assertEquals(emailBody, result);
    }

    @Test
    void sendEmailForDGApproval_TestOceanDGRequested() throws RunnerException {
        Map<OceanDGStatus, EmailTemplatesRequest> emailTemplatesRequestMap = new EnumMap<>(OceanDGStatus.class);
        EmailTemplatesRequest emailTemplatesRequest = EmailTemplatesRequest.builder()
                .body("<Table")
                .build();
        emailTemplatesRequestMap.put(OceanDGStatus.OCEAN_DG_REQUESTED, emailTemplatesRequest);

        List<String> toEmailIds = List.of("him@gmail.com");
        VesselsResponse vesselsResponse = new VesselsResponse();
        OceanDGStatus templateStatus = OceanDGStatus.OCEAN_DG_REQUESTED;
        TaskCreateResponse taskCreateResponse = TaskCreateResponse.builder().build();

        shipmentServiceImplV3.sendEmailForDGApproval(emailTemplatesRequestMap, toEmailIds, vesselsResponse, templateStatus, shipmentDetailsEntity, "remarks", taskCreateResponse);
        verify(notificationService).sendEmail(any(), any(), anyList(), anyList());
    }

    @Test
    void sendEmailForDGApproval_OCEANDG_COMMERCIAL_REQUESTED() throws RunnerException {
        Map<OceanDGStatus, EmailTemplatesRequest> emailTemplatesRequestMap = new EnumMap<>(OceanDGStatus.class);
        EmailTemplatesRequest emailTemplatesRequest = EmailTemplatesRequest.builder()
                .body("<Table")
                .build();
        emailTemplatesRequestMap.put(OceanDGStatus.OCEAN_DG_COMMERCIAL_REQUESTED, emailTemplatesRequest);

        List<String> toEmailIds = List.of("him@gmail.com");
        VesselsResponse vesselsResponse = new VesselsResponse();
        OceanDGStatus templateStatus = OceanDGStatus.OCEAN_DG_COMMERCIAL_REQUESTED;
        TaskCreateResponse taskCreateResponse = TaskCreateResponse.builder().build();

        shipmentServiceImplV3.sendEmailForDGApproval(emailTemplatesRequestMap, toEmailIds, vesselsResponse, templateStatus, shipmentDetailsEntity, "remarks", taskCreateResponse);
        verify(notificationService).sendEmail(any(), any(), anyList(), anyList());
    }

    @Test
    void sendEmailForDGApproval_EmptyEmailId() {
        Map<OceanDGStatus, EmailTemplatesRequest> emailTemplatesRequestMap = new EnumMap<>(OceanDGStatus.class);
        emailTemplatesRequestMap.put(OceanDGStatus.OCEAN_DG_REQUESTED, EmailTemplatesRequest.builder().build());

        List<String> toEmailIds = new ArrayList<>();
        VesselsResponse vesselsResponse = new VesselsResponse();
        OceanDGStatus templateStatus = OceanDGStatus.OCEAN_DG_REQUESTED;
        TaskCreateResponse taskCreateResponse = TaskCreateResponse.builder().build();

        assertThrows(RunnerException.class, () ->
                shipmentServiceImplV3.sendEmailForDGApproval(emailTemplatesRequestMap, toEmailIds,
                        vesselsResponse, templateStatus, shipmentDetailsEntity, "remarks", taskCreateResponse));
    }

    @Test
    void testFetchDgUserTask_SuccessfulCase() throws RunnerException {
        // Given
        Long shipmentId = 123L;
        String shipmentGuid = "guid-123";
        String userEmail = "test@example.com";
        String taskGuid = "task-guid-001";

        OceanDGRequestV3 request = OceanDGRequestV3.builder()
                .shipmentId(shipmentId)
                .shipmentGuid(shipmentGuid)
                .build();

        // Mocking the JSON helper
        when(jsonHelper.convertToJson(any(CommonV1ListRequest.class))).thenReturn("{}");

        // Mocking MDM service adapter response
        Map<String, Object> taskMap = new HashMap<>();
        taskMap.put("uuid", taskGuid);
        taskMap.put("userEmail", userEmail);

        List<Map<String, Object>> mockTaskList = Collections.singletonList(taskMap);
        when(mdmServiceAdapter.getTaskList(eq(shipmentGuid), eq(SHIPMENTS_WITH_SQ_BRACKETS), eq(PENDING_ACTION_TASK), eq(DG_OCEAN_APPROVAL)))
                .thenReturn(mockTaskList);

        // When
        shipmentServiceImplV3.fetchDgUserTask(request);

        // Then
        assertNotNull(request.getTaskGuids());
        assertEquals(1, request.getTaskGuids().size());
        assertEquals(taskGuid, request.getTaskGuids().get(0));
        assertEquals(userEmail, request.getUserEmail());
    }

    @Test
    void testFetchDgUserTask_MdmServiceThrowsException() {
        OceanDGRequestV3 request = OceanDGRequestV3.builder()
                .shipmentId(123L)
                .shipmentGuid("guid-123")
                .build();

        when(jsonHelper.convertToJson(any())).thenReturn("{}");

        when(mdmServiceAdapter.getTaskList(anyString(), any(), any(), any()))
                .thenThrow(new RuntimeException("MDM down"));

        RunnerException thrown = assertThrows(RunnerException.class, () -> {
            shipmentServiceImplV3.fetchDgUserTask(request);
        });

        assertTrue(thrown.getMessage().contains("MDM down"));
    }

    @Test
    void testCreateConsolidationInV3NullCheck() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(false).build());
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        List<Containers> containers = new ArrayList<>();
        mockShipmentSettings();
        assertEquals(null, shipmentServiceImplV3.createConsolidationInV3(shipmentDetails, containers));
    }

    @Test
    void testCreateConsolidationConsolidationInV3Lite() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(true).consolidationLite(false).build());
        CarrierDetails carrierDetails = CarrierDetails.builder().build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().transportMode(Constants.TRANSPORT_MODE_SEA).carrierDetails(carrierDetails).build();
        List<Containers> containers = new ArrayList<>();
        String errorMessage = "Not able to create consolidation, before adding 'New Containers', please provide ‘Origin’ and ‘Destination’ values.";
        mockShipmentSettings();
        Exception e = assertThrows(ValidationException.class, () -> shipmentServiceImplV3.createConsolidationInV3(shipmentDetails, containers));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void testCreateConsolidationConsolidationInV3LiteSameOriginDestination() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(true).consolidationLite(false).build());
        CarrierDetails carrierDetails = CarrierDetails.builder().originPort("OriginPort").destinationPort("OriginPort").build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().transportMode(Constants.TRANSPORT_MODE_SEA).carrierDetails(carrierDetails).build();
        List<Containers> containers = new ArrayList<>();
        String errorMessage = "‘Origin’ and ‘Destination’ can't be same.";
        mockShipmentSettings();
        Exception e = assertThrows(ValidationException.class, () -> shipmentServiceImplV3.createConsolidationInV3(shipmentDetails, containers));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void testCreateConsolidationInV3() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(true).consolidationLite(false).build());

        PartyRequestV2 partyRequestV2 = new PartyRequestV2();
        partyRequestV2.setTenantId(1);

        Parties parties = Parties.builder().orgCode("1").build();

        doNothing().when(consolidationV3Service).generateConsolidationNumber(any(ConsolidationDetails.class));
        CarrierDetails carrierDetails = CarrierDetails.builder().originPort("OriginPort").destinationPort("DestinationPort").build();
        Routings routings = new Routings();
        routings.setTenantId(1);
        routings.setMode("mode");

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .carrierDetails(carrierDetails)
                .direction(Constants.DIRECTION_IMP)
                .masterBill("1234")
                .routingsList(Arrays.asList(routings))
                .build();
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(carrierDetails);

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().carrierDetails(carrierDetails).sendingAgent(parties).receivingAgent(parties).build();
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), eq(false), anyBoolean())).thenReturn(consolidationDetails);
        mockShipmentSettings();
        commonUtils.getShipmentSettingFromContext().setEnableRouteMaster(true);
        ConsolidationDetails result = shipmentServiceImplV3.createConsolidationInV3(shipmentDetails, new ArrayList<>());

        assertNotNull(result);
        assertEquals(carrierDetails, result.getCarrierDetails());
    }

    @Test
    void testCreateConsolidationInV3DefaultDirectionExp() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(true).consolidationLite(false).build());

        PartyRequestV2 partyRequestV2 = new PartyRequestV2();
        partyRequestV2.setTenantId(1);

        Parties parties = Parties.builder().orgCode("1").build();

        doNothing().when(consolidationV3Service).generateConsolidationNumber(any(ConsolidationDetails.class));
        CarrierDetails carrierDetails = CarrierDetails.builder().originPort("OriginPort").destinationPort("DestinationPort").build();

        Routings routings = new Routings();
        routings.setTenantId(1);
        routings.setMode("mode");

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .carrierDetails(carrierDetails)
                .direction(Constants.DIRECTION_EXP)
                .masterBill("1234")
                .routingsList(Arrays.asList(routings))
                .build();

        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(carrierDetails);

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().carrierDetails(carrierDetails).shipmentType(Constants.DIRECTION_EXP).sendingAgent(parties).receivingAgent(parties).build();
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), eq(false), anyBoolean())).thenReturn(consolidationDetails);
        mockShipmentSettings();
        ConsolidationDetails result = shipmentServiceImplV3.createConsolidationInV3(shipmentDetails, new ArrayList<>());

        assertNotNull(result);
        assertEquals(carrierDetails, result.getCarrierDetails());
    }

    @Test
    void testCreateConsolidationInV3Routes() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(true).consolidationLite(false).build());

        PartyRequestV2 partyRequestV2 = new PartyRequestV2();
        partyRequestV2.setTenantId(1);

        Parties parties = Parties.builder().orgCode("1").build();

        doNothing().when(consolidationV3Service).generateConsolidationNumber(any(ConsolidationDetails.class));
        CarrierDetails carrierDetails = CarrierDetails.builder().originPort("OriginPort").destinationPort("DestinationPort").build();
        Routings routings = new Routings();
        routings.setTenantId(1);
        routings.setMode(Constants.TRANSPORT_MODE_SEA);
        routings.setLeg(1L);
        when(jsonHelper.convertValue(any(), eq(Routings.class))).thenReturn(routings);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .carrierDetails(carrierDetails)
                .direction(Constants.DIRECTION_IMP)
                .masterBill("1234")
                .routingsList(Arrays.asList(routings))
                .build();
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(carrierDetails);

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().carrierDetails(carrierDetails).sendingAgent(parties).receivingAgent(parties).build();
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), eq(false), anyBoolean())).thenReturn(consolidationDetails);
        mockShipmentSettings();
        ConsolidationDetails result = shipmentServiceImplV3.createConsolidationInV3(shipmentDetails, new ArrayList<>());

        assertNotNull(result);
        assertEquals(carrierDetails, result.getCarrierDetails());
    }

    @Test
    void testValidateRequiredParams_BothNull_ThrowsException() {
        ValidationException exception = assertThrows(
                ValidationException.class,
                () -> ShipmentServiceImplV3.validateRequiredParams(null, null)
        );
        assertEquals("Required parameters missing: consoleId and consoleGuid", exception.getMessage());
    }

    @Test
    void testValidateRequiredParams_ConsoleIdNotNull_DoesNotThrow() {
        assertDoesNotThrow(() -> ShipmentServiceImplV3.validateRequiredParams(123L, null));
    }

    @Test
    void testValidateRequiredParams_ConsoleGuidNotNull_DoesNotThrow() {
        assertDoesNotThrow(() -> ShipmentServiceImplV3.validateRequiredParams(null, "guid-123"));
    }

    @Test
    void testValidateRequiredParams_BothNotNull_DoesNotThrow() {
        assertDoesNotThrow(() -> ShipmentServiceImplV3.validateRequiredParams(456L, "guid-456"));
    }
}