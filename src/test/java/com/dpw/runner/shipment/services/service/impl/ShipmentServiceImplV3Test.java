package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
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
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsListResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentListResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.helpers.ShipmentMasterDataHelperV3;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import org.apache.commons.lang3.ObjectUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.Executors;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentServiceImplV3Test {

    @InjectMocks
    private ShipmentServiceImplV3 shipmentServiceImplV3;
    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Mock
    private INotificationDao notificationDao;
    @Mock
    private CommonUtils commonUtils;
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

    private static JsonTestUtility jsonTestUtility;
    private static ShipmentDetails shipmentDetails;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
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
        shipmentDetails = jsonTestUtility.getTestShipment();
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
        shipmentDetails.setId(4L);
        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetails);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(shipmentDetailsList));
        ShipmentPacksAssignContainerTrayDto.Shipments shipments = ShipmentPacksAssignContainerTrayDto.Shipments.builder().id(4L).packsList(new ArrayList<>()).build();
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(shipments));
        when(shipmentsContainersMappingDao.findByContainerId(any())).thenReturn(new ArrayList<>());
        ShipmentPacksAssignContainerTrayDto response =
                shipmentServiceImplV3.getShipmentAndPacksForConsolidationAssignContainerTray(1L, 2L);
        assertNotNull(response);
    }

    @Test
    void testGetShipmentAndPacksForConsolidationAssignContainerTrayAssignedFCL() {
        shipmentDetails.setId(4L);
        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetails);
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
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(shipments1, shipments));
        when(shipmentsContainersMappingDao.findByContainerId(any())).thenReturn(List.of(ShipmentsContainersMapping.builder().shipmentId(4L).build()));
        ShipmentPacksAssignContainerTrayDto response =
                shipmentServiceImplV3.getShipmentAndPacksForConsolidationAssignContainerTray(1L, 2L);
        assertNotNull(response);
    }

    @Test
    void testGetShipmentAndPacksForConsolidationAssignContainerTrayAssigned() {
        List<ShipmentDetails> shipmentDetailsList = List.of(shipmentDetails);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(shipmentDetailsList));
        ShipmentPacksAssignContainerTrayDto.Shipments shipments = ShipmentPacksAssignContainerTrayDto.Shipments.builder()
                .id(4L)
                .packsList(new ArrayList<>())
                .build();
        ShipmentPacksAssignContainerTrayDto.Shipments shipments1 = ShipmentPacksAssignContainerTrayDto.Shipments.builder()
                .id(5L)
                .packsList(new ArrayList<>())
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
        doReturn(ConsolidationDetailsResponse.builder().build()).when(consolidationV3Service).createConsolidationForBooking(any(), any());

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

}