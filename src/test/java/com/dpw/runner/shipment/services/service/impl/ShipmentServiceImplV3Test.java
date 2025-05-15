package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.AutoUpdateWtVolRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.AutoUpdateWtVolResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.mapper.ShipmentMapper;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsListResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentListResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentRetrieveLiteResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksUnAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentSailingScheduleRequest;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkRoutingResponse;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.helpers.ShipmentMasterDataHelperV3;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import com.dpw.runner.shipment.services.service.interfaces.IEventsV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsV3Service;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.EventsV3Util;
import com.dpw.runner.shipment.services.utils.v3.ShipmentValidationV3Util;
import com.dpw.runner.shipment.services.utils.v3.ShipmentsV3Util;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.http.auth.AuthenticationException;
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
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;

import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

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

    private ShipmentDetails shipmentDetails;
    private ConsolidationDetails consolidationDetails;
    private static JsonTestUtility jsonTestUtility;
    private static ShipmentDetails testShipment;
    private static ObjectMapper objectMapper;
    private static ConsolidationDetails testConsol;

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
    }

    @BeforeEach
    void setup() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(false).build());
        shipmentServiceImplV3.executorService = Executors.newFixedThreadPool(2);
        shipmentDetails = new ShipmentDetails();
        consolidationDetails = new ConsolidationDetails();
        testShipment = jsonTestUtility.getTestShipment();
        testConsol = jsonTestUtility.getJson("MAWB_CONSOLIDATION", ConsolidationDetails.class);

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
        ShipmentListResponse shipmentListResponse = new ShipmentListResponse();
        shipmentListResponse.setContainsHazardous(false);
        shipmentDetailsList.add(new ShipmentDetails());
        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        PageImpl<Long> shipmentIdPage = new PageImpl<>(List.of(1L));
        when(shipmentRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(), any())).thenReturn(shipmentListResponse);
        when(shipmentDao.getIdWithPendingActions(eq(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED), any())).thenReturn(shipmentIdPage);

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
        List<ShipmentListResponse> shipmentListResponses  = ShipmentMapper.INSTANCE.toShipmentListResponses(lst);
        for(var i: shipmentListResponses) {
            if (i.getStatus() != null && i.getStatus() < ShipmentStatus.values().length)
                i.setShipmentStatus(ShipmentStatus.values()[i.getStatus()].toString());
            if (ObjectUtils.isNotEmpty(i.getShipmentOrders()))
                i.setOrdersCount(i.getShipmentOrders().size());
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
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>()));
        ShipmentPacksAssignContainerTrayDto.Shipments shipments = ShipmentPacksAssignContainerTrayDto.Shipments.builder().id(4L).build();
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(shipments));
        when(shipmentsContainersMappingDao.findByContainerId(any())).thenReturn(new ArrayList<>());
        ShipmentPacksAssignContainerTrayDto response =
                shipmentServiceImplV3.getShipmentAndPacksForConsolidationAssignContainerTray(1L, 2L);
        assertNotNull(response);
    }

    @Test
    void testGetShipmentAndPacksForConsolidationAssignContainerTrayAssignedFCL() {
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>()));
        ShipmentPacksAssignContainerTrayDto.Shipments shipments = ShipmentPacksAssignContainerTrayDto.Shipments.builder()
                .id(4L)
                .shipmentType(Constants.CARGO_TYPE_FCL)
                .build();
        ShipmentPacksAssignContainerTrayDto.Shipments shipments1 = ShipmentPacksAssignContainerTrayDto.Shipments.builder()
                .id(5L)
                .build();
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(shipments1, shipments));
        when(shipmentsContainersMappingDao.findByContainerId(any())).thenReturn(List.of(ShipmentsContainersMapping.builder().shipmentId(4L).build()));
        ShipmentPacksAssignContainerTrayDto response =
                shipmentServiceImplV3.getShipmentAndPacksForConsolidationAssignContainerTray(1L, 2L);
        assertNotNull(response);
    }

    @Test
    void testGetShipmentAndPacksForConsolidationAssignContainerTrayAssigned() {
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>()));
        ShipmentPacksAssignContainerTrayDto.Shipments shipments = ShipmentPacksAssignContainerTrayDto.Shipments.builder()
                .id(4L)
                .build();
        ShipmentPacksAssignContainerTrayDto.Shipments shipments1 = ShipmentPacksAssignContainerTrayDto.Shipments.builder()
                .id(5L)
                .build();
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(shipments1, shipments));
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
    void testGetPendingNotificationData(){
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.ofNullable(ShipmentDetails.builder().build()));
        when(commonUtils.setIncludedFieldsToResponse(any(), any(), any())).thenReturn(new ShipmentPendingNotificationResponse());
        ShipmentPendingNotificationResponse shipmentPendingNotificationResponse = shipmentServiceImplV3.getPendingNotificationData(request);
        assertDoesNotThrow(() -> shipmentPendingNotificationResponse);
    }

    @Test
    void testGetPendingNotificationData_ThrowsException(){
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
//        when(notesDao.findByEntityIdAndEntityType(any(), any())).thenReturn(Arrays.asList(Notes.builder().build()));

        ShipmentOrder shipmentOrder = ShipmentOrder.builder().shipmentId(1L).orderGuid(UUID.fromString("eaf227f3-de85-42b4-8180-cf48ccf568f9")).build();
        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        Parties importBroker = Parties.builder().orgCode("1223").build();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setImportBroker(importBroker);
        additionalDetails.setExportBroker(importBroker);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").build().setReferenceNumbersList(Collections.singletonList(referenceNumbers)).setAdditionalDetails(additionalDetails).setGoodsDescription("Abcd");
        shipmentDetails.setGuid(UUID.randomUUID());
        shipmentDetails.setShipmentOrders(Collections.singletonList(shipmentOrder));
        shipmentDetails.setAdditionalDetails(new AdditionalDetails());
        shipmentDetails.setCarrierDetails(CarrierDetails.builder().build());

        when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(ConsolidationDetailsRequest.builder().build());
        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolRequest.class))).thenReturn(new AutoUpdateWtVolRequest());
        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolResponse.class))).thenReturn(new AutoUpdateWtVolResponse());
        doReturn(ConsolidationDetailsResponse.builder().build()).when(consolidationV3Service).createConsolidationForBooking(any());

        ReferenceNumbersRequest referenceNumberObj2 = ReferenceNumbersRequest.builder().build();

        when(jsonHelper.convertValue(anyList(), any(TypeReference.class))).thenReturn(Collections.singletonList(referenceNumberObj2));

        ContainerRequest containerRequest = ContainerRequest.builder().build();
//        when(jsonHelper.convertValueToList(any(), eq(ContainerRequest.class))).thenReturn(List.of(containerRequest));
        PackSummaryResponse packSummaryResponse = mock(PackSummaryResponse.class);
        when(packingService.calculatePackSummary(anyList(), any(), any(), any())).thenReturn(packSummaryResponse);

        when(jsonHelper.convertValueToList(any(), eq(Packing.class))).thenReturn(Collections.singletonList(new Packing()));
        when(jsonHelper.convertValueToList(any(), eq(ReferenceNumbers.class))).thenReturn(Collections.singletonList(referenceNumbers));
        when(referenceNumbersDao.saveEntityFromShipment(any(), any())).thenReturn(Collections.singletonList(referenceNumbers));
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        when(shipmentDao.save(any(), eq(false))).thenReturn(shipmentDetails);
        //when(notesDao.findByEntityIdAndEntityType(any(), any())).thenReturn(null);

        ShipmentDetailsV3Response shipmentDetailsV3Response = jsonHelper.convertValue(shipmentDetails, ShipmentDetailsV3Response.class);
        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsV3Response.class)).thenReturn(shipmentDetailsV3Response);

        //when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(new CarrierDetails());
       // when(routingsDao.generateDefaultRouting(any(), any())).thenReturn(List.of());

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setEnableRouteMaster(true);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        when(commonUtils.getCurrentTenantSettings()).thenReturn(V1TenantSettingsResponse.builder().P100Branch(true).transportModeConfig(true).build());
        when(orderManagementAdapter.getOrderByGuid(any())).thenReturn(shipmentDetails);

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

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));

        Optional<ShipmentDetails> result = shipmentServiceImplV3.retrieveByIdOrGuid(request);

        assertTrue(result.isPresent());
        assertEquals(shipmentDetails, result.get());
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

        when(shipmentDao.findByGuid(guid)).thenReturn(Optional.of(shipmentDetails));

        Optional<ShipmentDetails> result = shipmentServiceImplV3.retrieveByIdOrGuid(request);

        assertTrue(result.isPresent());
        assertEquals(shipmentDetails, result.get());
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
        consolidationDetails.setReceivingBranch(5L);
        shipmentDetails.setReceivingBranch(999L);

        boolean result = shipmentServiceImplV3.isNotAllowedToViewShipment(null, shipmentDetails, 5L, consolidationDetails);

        assertFalse(result);
    }

    @Test
    void isNotAllowedToViewShipmentTest2() {
        TriangulationPartner partner = new TriangulationPartner();
        partner.setTriangulationPartner(5L);
        consolidationDetails.setTriangulationPartnerList(List.of(partner));
        shipmentDetails.setReceivingBranch(999L);

        boolean result = shipmentServiceImplV3.isNotAllowedToViewShipment(null, shipmentDetails, 5L, consolidationDetails);

        assertFalse(result);
    }

    @Test
    void isNotAllowedToViewShipmentTest3() {
        consolidationDetails.setTriangulationPartner(5L);
        shipmentDetails.setReceivingBranch(999L);

        boolean result = shipmentServiceImplV3.isNotAllowedToViewShipment(null, shipmentDetails, 5L, consolidationDetails);

        assertFalse(result);
    }

    @Test
    void isNotAllowedToViewShipmentTest4() {
        shipmentDetails.setReceivingBranch(5L);

        boolean result = shipmentServiceImplV3.isNotAllowedToViewShipment(null, shipmentDetails, 5L, null);

        assertFalse(result);
    }

    @Test
    void isNotAllowedToViewShipmentTest5() {
        TriangulationPartner partner = new TriangulationPartner();
        partner.setTriangulationPartner(5L);
        shipmentDetails.setReceivingBranch(999L);

        boolean result = shipmentServiceImplV3.isNotAllowedToViewShipment(List.of(partner), shipmentDetails, 5L, null);

        assertFalse(result);
    }

    @Test
    void isNotAllowedToViewShipmentTest6() {
        shipmentDetails.setTriangulationPartner(5L);

        boolean result = shipmentServiceImplV3.isNotAllowedToViewShipment(null, shipmentDetails, 5L, null);

        assertFalse(result);
    }

    @Test
    void isNotAllowedToViewShipmentTest7() {
        shipmentDetails.setReceivingBranch(999L);
        shipmentDetails.setTriangulationPartner(888L);
        consolidationDetails.setReceivingBranch(777L);
        consolidationDetails.setTriangulationPartner(666L);
        consolidationDetails.setTriangulationPartnerList(List.of());

        boolean result = shipmentServiceImplV3.isNotAllowedToViewShipment(List.of(), shipmentDetails, 5L, consolidationDetails);

        assertTrue(result);
    }

    @Test
    void testRetrieveShipmentData_byId_success() throws Exception {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(123L).build();
        CommonRequestModel requestModel = CommonRequestModel.builder().data(getRequest).build();
        populateShipmentDetails();
        ShipmentRetrieveLiteResponse shipmentRetrieveLiteResponse = new ShipmentRetrieveLiteResponse();
        shipmentRetrieveLiteResponse.setStatus(0);

        when(shipmentDao.findById(123L)).thenReturn(Optional.of(shipmentDetails));
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

        when(shipmentDao.findByGuid(guid)).thenReturn(Optional.of(shipmentDetails));
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

        when(shipmentDao.findShipmentByIdWithQuery(999L)).thenReturn(Optional.of(shipmentDetails));

        assertThrows(AuthenticationException.class, () ->  shipmentServiceImplV3.retireveShipmentData(requestModel, "network_transfer"));
    }

    @Test
    void testRetrieveShipmentData_nteSource2() {
        populateShipmentDetails();
        UUID guid = UUID.randomUUID();
        CommonGetRequest getRequest = CommonGetRequest.builder().guid(guid.toString()).build();
        CommonRequestModel requestModel = CommonRequestModel.builder().data(getRequest).build();

        when(shipmentDao.findShipmentByGuidWithQuery(guid)).thenReturn(Optional.of(shipmentDetails));

        assertThrows(AuthenticationException.class, () ->  shipmentServiceImplV3.retireveShipmentData(requestModel, "network_transfer"));
    }

    @Test
    void testRetrieveShipmentData_nteSource3() {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(999L).build();
        CommonRequestModel requestModel = CommonRequestModel.builder().data(getRequest).build();

        when(shipmentDao.findShipmentByIdWithQuery(999L)).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, () ->  shipmentServiceImplV3.retireveShipmentData(requestModel, "network_transfer"));
    }

    @Test
    void testRetrieveShipmentData_nteSource4() throws AuthenticationException, RunnerException {
        populateShipmentDetails();
        shipmentDetails.setPackingList(null);
        shipmentDetails.setReceivingBranch(5L);
        consolidationDetails.setReceivingBranch(777L);
        shipmentDetails.setConsolidationList(Set.of(consolidationDetails));

        CommonGetRequest getRequest = CommonGetRequest.builder().id(999L).build();
        CommonRequestModel requestModel = CommonRequestModel.builder().data(getRequest).build();

        when(shipmentDao.findShipmentByIdWithQuery(999L)).thenReturn(Optional.of(shipmentDetails));
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
    void test_changeConsolidationDGValues_makeConsoleDG_true() {
        Long consolidationId = 1L;
        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));
        when(consolidationDetailsDao.save(any(), anyBoolean(), anyBoolean())).thenReturn(consolidationDetails);

        ConsolidationDetails result = shipmentServiceImplV3.changeConsolidationDGValues(
                true, new AtomicBoolean(false), consolidationId, shipmentDetails, null);

        assertNotNull(result);
        verify(consolidationDetailsDao).save(any(), eq(false), eq(true));
    }

    @Test
    void test_changeConsolidationDGValues_makeConsoleNonDG_true_allNonDG() {
        Long consolidationId = 1L;
        shipmentDetails.setId(10L);
        consolidationDetails.setHazardous(true);

        ConsoleShipmentMapping consoleShipmentMapping1 = new ConsoleShipmentMapping();
        consoleShipmentMapping1.setShipmentId(5L);
        consoleShipmentMapping1.setConsolidationId(consolidationId);
        ConsoleShipmentMapping consoleShipmentMapping2 = new ConsoleShipmentMapping();
        consoleShipmentMapping2.setShipmentId(7L);
        consoleShipmentMapping2.setConsolidationId(consolidationId);

        when(shipmentDao.findByShipmentIdInAndContainsHazardous(anyList(), eq(true)))
                .thenReturn(Collections.emptyList());
        when(consoleShipmentMappingDao.findByConsolidationId(consolidationId)).thenReturn(List.of(consoleShipmentMapping1, consoleShipmentMapping2));
        when(consolidationDetailsDao.save(any(), eq(false), eq(false))).thenReturn(consolidationDetails);

        AtomicBoolean makeConsoleNonDG = new AtomicBoolean(true);
        ConsolidationDetails result = shipmentServiceImplV3.changeConsolidationDGValues(
                false, makeConsoleNonDG, consolidationId, shipmentDetails, consolidationDetails);

        assertNotNull(result);
        verify(consolidationDetailsDao).save(any(), eq(false), eq(false));
    }

    @Test
    void test_changeConsolidationDGValues_makeConsoleNonDG_true_notAllNonDG() {
        Long consolidationId = 1L;
        shipmentDetails.setId(10L);

        ConsoleShipmentMapping consoleShipmentMapping1 = new ConsoleShipmentMapping();
        consoleShipmentMapping1.setShipmentId(5L);
        consoleShipmentMapping1.setConsolidationId(consolidationId);
        ConsoleShipmentMapping consoleShipmentMapping2 = new ConsoleShipmentMapping();
        consoleShipmentMapping2.setShipmentId(7L);
        consoleShipmentMapping2.setConsolidationId(consolidationId);

        when(shipmentDao.findByShipmentIdInAndContainsHazardous(anyList(), eq(true)))
                .thenReturn(List.of(shipmentDetails)); // at least one DG shipment
        when(consoleShipmentMappingDao.findByConsolidationId(consolidationId)).thenReturn(List.of(consoleShipmentMapping1, consoleShipmentMapping2));

        AtomicBoolean makeConsoleNonDG = new AtomicBoolean(true);
        ConsolidationDetails result = shipmentServiceImplV3.changeConsolidationDGValues(
                false, makeConsoleNonDG, consolidationId, shipmentDetails, consolidationDetails);

        assertNull(result);
        verify(consolidationDetailsDao, never()).save(any(), anyBoolean(), anyBoolean());
    }

    @Test
    void test_changeConsolidationDGValues_neither_condition_true() {
        shipmentDetails.setId(10L);

        ConsolidationDetails result = shipmentServiceImplV3.changeConsolidationDGValues(
                false, new AtomicBoolean(false), 1L, shipmentDetails, consolidationDetails);

        assertNull(result);
    }

    @Test
    void test_getConsolidationDetails_whenPresent() {
        Long id = 1L;
        when(consolidationDetailsDao.findById(id)).thenReturn(Optional.of(consolidationDetails));

        ConsolidationDetails result = shipmentServiceImplV3.getConsolidationDetails(id, null);
        assertEquals(consolidationDetails, result);
    }

    @Test
    void test_getConsolidationDetails_whenPassed() {
        ConsolidationDetails result = shipmentServiceImplV3.getConsolidationDetails(1L, consolidationDetails);
        assertEquals(consolidationDetails, result);
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
                .thenReturn(List.of(shipmentDetails));

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
        consolidationDetails.setHazardous(false);
        when(consolidationDetailsDao.save(any(), eq(false), eq(true))).thenReturn(consolidationDetails);

        ConsolidationDetails result = shipmentServiceImplV3.saveConsolidationDGValue(true, consolidationDetails);
        assertTrue(result.getHazardous());
    }

    @Test
    void test_saveConsolidationDGValue_dgFlag_false_changeRequired() {
        consolidationDetails.setHazardous(true);
        when(consolidationDetailsDao.save(any(), eq(false), eq(false))).thenReturn(consolidationDetails);

        ConsolidationDetails result = shipmentServiceImplV3.saveConsolidationDGValue(false, consolidationDetails);
        assertFalse(result.getHazardous());
    }

    @Test
    void test_saveConsolidationDGValue_noChangeNeeded() {
        consolidationDetails.setHazardous(true);

        ConsolidationDetails result = shipmentServiceImplV3.saveConsolidationDGValue(true, consolidationDetails);
        assertNull(result);
    }

    private void populateShipmentDetails() {
        shipmentDetails.setId(123L);
        shipmentDetails.setGuid(UUID.randomUUID());
        shipmentDetails.setStatus(1);
        shipmentDetails.setPackingList(List.of(new Packing()));
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
        Long shipmentId = 1L;
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

        shipmentServiceImplV3.updateCargoDetailsInShipment(shipmentId, response);

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

        shipmentDetails.setTransportMode("SEA");

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(routingsV3Service.updateBulk(any(), eq(SHIPMENT))).thenReturn(new BulkRoutingResponse());
        doNothing().when(shipmentDao).updateSailingScheduleRelatedInfo(any(), anyLong());

        shipmentServiceImplV3.updateSailingScheduleDataToShipment(request);

        verify(shipmentDao).updateSailingScheduleRelatedInfo(request, 1L);
    }

    @Test
    void testUpdateSailingScheduleDataToShipment_forAirMode_shouldCallAirUpdate() throws RunnerException {
        ShipmentSailingScheduleRequest request = new ShipmentSailingScheduleRequest();
        RoutingsRequest routing = new RoutingsRequest();
        routing.setShipmentId(1L);
        request.setRoutings(List.of(routing));

        ShipmentDetails details = new ShipmentDetails();
        details.setTransportMode("AIR");

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

        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));

        when(commonUtils.setIncludedFieldsToResponse(eq(shipmentDetails), anySet(), any(ShipmentDetailsResponse.class)))
                .thenReturn(shipmentDetailsResponse);

        ShipmentServiceImplV3 spyService = Mockito.spy(shipmentServiceImplV3);
        doReturn(dummyMasterData).when(spyService).fetchAllMasterDataByKey(eq(shipmentDetails), eq(shipmentDetailsResponse));

        Map<String, Object> result = spyService.getAllMasterData(shipmentId, xSource);

        assertNotNull(result);
        assertEquals("value1", result.get("key1"));

        verify(shipmentDao).findById(shipmentId);
        verify(commonUtils).setIncludedFieldsToResponse(eq(shipmentDetails), anySet(), any(ShipmentDetailsResponse.class));
        verify(spyService).fetchAllMasterDataByKey(shipmentDetails, shipmentDetailsResponse);
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

        // Mocks
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
    void testCreate_success2() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAutoEventCreate(false).setIsNetworkTransferEntityEnabled(true).setIsAutomaticTransferEnabled(true);
        TenantSettingsDetailsContext.getCurrentTenantSettings().setP100Branch(true);
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setMasterBill(null);
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

        shipmentServiceImplV3.afterSave(newShipment, null, true, shipmentRequest, ShipmentSettingsDetailsContext.getCurrentTenantSettings(), false, false);

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

        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        mockTenantSettings();
        mockShipmentSettings();
        doNothing().when(awbDao).updatedAwbInformationEvent(any(), any());

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, false, new ShipmentV3Request(), ShipmentSettingsDetailsContext.getCurrentTenantSettings(), false, false);

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

        ShipmentV3Request shipmentRequest = new ShipmentV3Request();

        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        mockTenantSettings();
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(testConsol));

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, false, shipmentRequest, ShipmentSettingsDetailsContext.getCurrentTenantSettings(), false, false);

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

        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, null, true, request, ShipmentSettingsDetailsContext.getCurrentTenantSettings(), false, false);

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

        ConsolidationDetailsProjection projection = mock(ConsolidationDetailsProjection.class);
        when(projection.getConsolidationNumber()).thenReturn("C123");
        when(projection.getTenantId()).thenReturn(5);

        when(consolidationDetailsDao.findMblNumberInDifferentTenant(any())).thenReturn(List.of(projection));
        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, false, new ShipmentV3Request(), ShipmentSettingsDetailsContext.getCurrentTenantSettings(), false, false);

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

        mockTenantSettings();
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(details));

        assertThrows(RunnerException.class, () -> {
            shipmentServiceImplV3.afterSave(shipment, oldShipment, false, new ShipmentV3Request(), ShipmentSettingsDetailsContext.getCurrentTenantSettings(), false, false);
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

        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setIsMAWBColoadingEnabled(true);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        mockShipmentSettings();

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, false, shipmentRequest,
                ShipmentSettingsDetailsContext.getCurrentTenantSettings(), false, false);

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

        when(eventsV3Util.createOrUpdateEvents(any(), any(), any(), anyBoolean()))
                .thenReturn(eventsList);
        when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString()))
                .thenReturn(eventsList);
        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, false, shipmentRequest,
                shipmentSettings, false, false);

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

        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, null, true, new ShipmentV3Request(),
                shipmentSettings, false, false);

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

        when(masterDataUtils.withMdc(any())).thenAnswer(invocation -> invocation.getArgument(0));
        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, null, true, new ShipmentV3Request(),
                shipmentSettings, false, false);

        verify(masterDataUtils, times(2)).withMdc(any());
    }

    @Test
    void testAfterSave_withP100Branch_shouldUpdateBookingInPlatform() {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(9L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setAdditionalDetails(new AdditionalDetails());

        mockTenantSettings();

        assertDoesNotThrow(() -> shipmentServiceImplV3.afterSave(newShipment, null, true, new ShipmentV3Request(),
                new ShipmentSettingsDetails(), false, false));
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

        mockTenantSettings();
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidation));
        when(consolidationDetailsDao.save(any(), anyBoolean(), anyBoolean())).thenReturn(consolidation);

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, false, new ShipmentV3Request(),
                new ShipmentSettingsDetails(), false, false);

        verify(consolidationDetailsDao).save(any(ConsolidationDetails.class), eq(false), eq(true));
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

        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(new ArrayList<>());

        mockTenantSettings();
        mockShipmentSettings();
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidation));

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, false, new ShipmentV3Request(),
                new ShipmentSettingsDetails(), false, false);

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

        mockTenantSettings();
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidation));
        when(consolidationDetailsDao.save(any(), anyBoolean(), anyBoolean())).thenReturn(consolidation);
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(new ArrayList<>());

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, false, new ShipmentV3Request(),
                new ShipmentSettingsDetails(), false, false);

        verify(consolidationDetailsDao).save(any(ConsolidationDetails.class), eq(false), anyBoolean());
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

        mockTenantSettings();
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidation));
        when(consolidationDetailsDao.save(any(), anyBoolean(), anyBoolean())).thenReturn(consolidation);

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, false, new ShipmentV3Request(),
                new ShipmentSettingsDetails(), false, false);

        verify(consolidationDetailsDao).save(any(ConsolidationDetails.class), eq(false), anyBoolean());

        ArgumentCaptor<ConsolidationDetails> consolidationCaptor = ArgumentCaptor.forClass(ConsolidationDetails.class);
        verify(consolidationDetailsDao).save(consolidationCaptor.capture(), eq(false), anyBoolean());
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

        mockTenantSettings();
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidation));
        when(consolidationDetailsDao.save(any(), anyBoolean(), anyBoolean())).thenReturn(consolidation);
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(mappings);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(Arrays.asList(shipment1, shipment2));

        shipmentServiceImplV3.afterSave(newShipment, oldShipment, false, new ShipmentV3Request(),
                new ShipmentSettingsDetails(), false, false);

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

        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, null, true, shipmentRequest,
                new ShipmentSettingsDetails(), false, false);

        verify(dependentServiceHelper).pushShipmentDataToDependentService(
                eq(newShipment), eq(true), eq(true), isNull());
    }

    @Test
    void testAfterSave_withAutoEventCreateTrue_shouldGenerateCreateEvent() throws RunnerException {
        ShipmentDetails newShipment = new ShipmentDetails();
        newShipment.setId(18L);
        newShipment.setGuid(UUID.randomUUID());
        newShipment.setAdditionalDetails(new AdditionalDetails());

        ShipmentSettingsDetails shipmentSettings = new ShipmentSettingsDetails();
        shipmentSettings.setAutoEventCreate(true);

        mockTenantSettings();

        shipmentServiceImplV3.afterSave(newShipment, null, true, new ShipmentV3Request(),
                shipmentSettings, false, false);

        verify(eventsV3Util).autoGenerateCreateEvent(newShipment);
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
}