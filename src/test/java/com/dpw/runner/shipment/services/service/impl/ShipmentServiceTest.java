package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.ApiError;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.ShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.patchRequest.CarrierPatchRequest;
import com.dpw.runner.shipment.services.dto.patchRequest.ShipmentPatchRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.request.CheckActiveInvoiceRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TIContainerListRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TIListRequest;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.CustomerCategoryRates;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.mapper.CarrierDetailsMapper;
import com.dpw.runner.shipment.services.mapper.ShipmentDetailsMapper;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.Entity.AuditLogRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PartyRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.poi.ss.usermodel.Workbook;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.openapitools.jackson.nullable.JsonNullable;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import javax.servlet.ServletOutputStream;
import javax.servlet.WriteListener;
import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentServiceTest {

    @InjectMocks
    private ShipmentService shipmentService;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private IShipmentRepository shipmentRepository;
    @Mock
    private IShipmentSync shipmentSync;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private CarrierDetailsMapper carrierDetailsMapper;
    @Mock
    private IV1Service v1Service;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private KafkaProducer kafkaProducer;
    @Mock
    private ITrackingServiceAdapter trackingServiceAdapter;
    @Mock
    private ProductIdentifierUtility productEngine;
    @Mock
    private IContainerDao containerDao;
    @Mock
    private IBookingCarriageDao bookingCarriageDao;
    @Mock
    private ITruckDriverDetailsDao truckDriverDetailsDao;
    @Mock
    private IPackingDao packingDao;
    @Mock
    private IELDetailsDao elDetailsDao;
    @Mock
    private IEventDao eventDao;
    @Mock
    private INotesDao notesDao;
    @Mock
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;
    @Mock
    private IReferenceNumbersDao referenceNumbersDao;
    @Mock
    private IRoutingsDao routingsDao;
    @Mock
    private IServiceDetailsDao serviceDetailsDao;
    @Mock
    private IPartiesDao partiesDao;
    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Mock
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;
    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock
    private IAdditionalDetailDao additionalDetailDao;
    @Mock
    private ShipmentDetailsMapper shipmentDetailsMapper;


    @Mock
    private IEventService eventService;
    @Mock
    private IAuditLogService auditLogService;
    @Mock
    private IContainerService containerService;
    @Mock
    private IPackingService packingService;
    @Mock
    private IOrderManagementAdapter orderManagementAdapter;
    @Mock
    private V1ServiceUtil v1ServiceUtil;

    @Mock
    private CommonUtils commonUtils;
    @Mock
    private ObjectMapper mockObjectMapper;
    @Mock
    ConsolidationService consolidationService;
    @Mock
    private HttpServletResponse response;

    @Captor
    private ArgumentCaptor<Workbook> workbookCaptor;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
    private static ShipmentDetails testShipment;
    private static ConsolidationDetails testConsol;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
    }

    @BeforeEach
    void setup() {
        testShipment = jsonTestUtility.getTestShipment();
        testConsol = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetails.class);
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
    }

    @Test
    void create_success() throws RunnerException {
        ShipmentDetails mockShipment = testShipment;
        mockShipment.setShipmentId("AIR-CAN-00001");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        // Mock
        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.create(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeUpdate_success() throws RunnerException {
        testShipment.setId(1L);
        ShipmentDetails mockShipment = testShipment;
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

    // Mock
    when(shipmentDao.findById(any()))
        .thenReturn(
            Optional.of(
                testShipment
                    .setConsolidationList(new ArrayList<>())
                    .setContainersList(new ArrayList<>())));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(testShipment);
//        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
//        when(jsonHelper.convertValueToList(any(), eq(ConsolidationDetails.class))).thenReturn(new ArrayList<ConsolidationDetails>());
//        when(containerDao.updateEntityFromShipmentConsole(eq(List.of()), eq(null), eq(null), anyBoolean())).thenReturn(List.of());
//        when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment);
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
//        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }


    @Test
    void toggleLock_lock() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);
        ShipmentDetails mockShipment = testShipment;

        Mockito.when(shipmentDao.findById(1L)).thenReturn(Optional.of(testShipment));
        Mockito.when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment.setIsLocked(true).setLockedBy("user"));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.toggleLock(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
        verify(shipmentSync, times(1)).syncLockStatus(any());

    }

    @Test
    void toggleLock_unlock() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        ShipmentDetails mockShipment = testShipment;
        mockShipment.setIsLocked(true);
        mockShipment.setLockedBy("user");

        Mockito.when(shipmentDao.findById(1L)).thenReturn(Optional.of(mockShipment));
        Mockito.when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment.setIsLocked(false));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.toggleLock(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
        verify(shipmentSync, times(1)).syncLockStatus(any());
    }

    @Test
    void pushShipmentDataToDependentService_success() {
        //Test
        shipmentService.pushShipmentDataToDependentService(testShipment, false);
        verify(kafkaProducer, atLeast(1)).produceToKafka(any(), any(), any());
    }


    @Test
    void cloneShipment() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        // Mock
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(testShipment));
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(
                objectMapper.convertValue(testShipment, ShipmentRequest.class));

        ShipmentDetails mockShip = testShipment;
        mockShip.setHouseBill(null);
        mockShip.setBookingNumber(null);
        mockShip.setContainersList(null);
        mockShip.setRoutingsList(null);
        mockShip.setShipmentId(null);
        mockShip.setMasterBill(null);
        mockShip.setConsolidationList(null);
        mockShip.setStatus(ShipmentStatus.Created.getValue());
        mockShip.setConsolRef(null);
        mockShip.setEventsList(null);
        mockShip.setShipmentCreatedOn(LocalDateTime.now());

        ShipmentDetailsResponse mockShipResponse = objectMapper.convertValue(mockShip, ShipmentDetailsResponse.class);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipResponse);

        //Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.cloneShipment(commonRequestModel);
        //Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipResponse), httpResponse);
    }

    @Test
    void generateCustomHouseBL_restrictHblGen() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().restrictHblGen(true).build());

        String hbl = shipmentService.generateCustomHouseBL(null);
        String mockHbl = null;

        assertEquals(mockHbl, hbl);
    }

    @Test
    void generateCustomHouseBL_product_sequence() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().customisedSequence(true).build());

        ShipmentDetails mockShipment = testShipment;
        mockShipment.setHouseBill(null);
        String mockHbl = "hblPrefix-hblSuffix-001";

        // Mock
        when(productEngine.getCustomizedBLNumber(any(), any())).thenReturn(mockHbl);
        // Test
        String hbl = shipmentService.generateCustomHouseBL(mockShipment);

        assertEquals(mockHbl, hbl);
    }

    @Test
    void generateCustomHouseBL_random() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder()
                .customisedSequence(false).housebillNumberGeneration("Random").build());

        ShipmentDetails mockShipment = testShipment;
        mockShipment.setHouseBill(null);

        // Test
        String hbl = shipmentService.generateCustomHouseBL(mockShipment);

        assertNotNull(hbl);
        assertEquals(10, hbl.length());
    }

    @Test
    void generateCustomHouseBL_serial() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder()
                .customisedSequence(false).housebillNumberGeneration("Serial").build());

        ShipmentDetails mockShipment = testShipment;
        mockShipment.setHouseBill(null);

        //Mock
        when(v1Service.getShipmentSerialNumber()).thenReturn("112344");

        // Test
        String hbl = shipmentService.generateCustomHouseBL(mockShipment);

        assertNotNull(hbl);
        assertEquals("112344", hbl);
    }

    @Test
    void getDefaultShipment() {
        // Mock data
        ShipmentSettingsDetails tenantSettings = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        UsersDto user = new UsersDto();
        user.setTenantId(1);
        UserContext.setUser(user);

        TenantModel tenantModel = new TenantModel();
        V1RetrieveResponse mockTenantResponse = new V1RetrieveResponse();
        mockTenantResponse.setEntity(tenantModel);
        when(v1Service.retrieveTenant()).thenReturn(mockTenantResponse);

        LocalDateTime mockDateTime = LocalDateTime.now();

        ShipmentDetailsResponse expectedResponse = new ShipmentDetailsResponse();
        expectedResponse.setSource(Constants.SYSTEM);
        expectedResponse.setStatus(ShipmentStatus.Created.getValue());
        expectedResponse.setAdditionalDetails(new AdditionalDetailResponse());
        expectedResponse.setCarrierDetails(new CarrierDetailResponse());
        expectedResponse.setCustomerCategory(CustomerCategoryRates.CATEGORY_5);
        expectedResponse.setShipmentCreatedOn(mockDateTime);
        expectedResponse.setSourceTenantId(1L);

        // Mocking behavior of dependencies
//        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
//        when(jsonHelper.buildSuccessResponse(any())).thenReturn(ResponseHelper.buildSuccessResponse(expectedResponse));

        // Execute the method under test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.getDefaultShipment();
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
//        ShipmentDetailsResponse shipmentDetailsResponse = objectMapper.convertValue(runnerResponse.getData(), ShipmentDetailsResponse.class);
//        shipmentDetailsResponse.setShipmentCreatedOn(mockDateTime);
//        assertEquals(expectedResponse.getSourceTenantId(), shipmentDetailsResponse.getSourceTenantId());

        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }


    @Test
    void calculateContainerSummary_throws_exception() throws RunnerException {
        CalculateContainerSummaryRequest containerSummaryRequest = new CalculateContainerSummaryRequest();
        List<ContainerRequest> containerRequests = new ArrayList<>();
        containerSummaryRequest.setContainersList(containerRequests);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(containerSummaryRequest);
        ContainerSummaryResponse containerSummaryResponse = new ContainerSummaryResponse();
        String errorMessage = "error while calculating summary !";
        // Mock
        when(containerService.calculateContainerSummary(anyList(), any(), any()))
                .thenThrow(new RunnerException(errorMessage));
        // Test
        ResponseEntity<IRunnerResponse> httpResponse =  shipmentService.calculateContainerSummary(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        RunnerResponse runnerResponse  = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }

    @Test
    void calculateContainerSummary_success() throws RunnerException {
        CalculateContainerSummaryRequest containerSummaryRequest = new CalculateContainerSummaryRequest();
        List<ContainerRequest> containerRequests = new ArrayList<>();
        containerSummaryRequest.setContainersList(containerRequests);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(containerSummaryRequest);

        ContainerSummaryResponse containerSummaryResponse = new ContainerSummaryResponse();
        // Mock
        when(containerService.calculateContainerSummary(anyList(), any(), any())).thenReturn(containerSummaryResponse);
        // Test
        ResponseEntity<IRunnerResponse> httpResponse =  shipmentService.calculateContainerSummary(commonRequestModel);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(containerSummaryResponse), httpResponse);

    }

    @Test
    void calculatePackSummary_throws_exception() throws RunnerException {
        CalculatePackSummaryRequest calculatePackSummaryRequest = new CalculatePackSummaryRequest();
        List<PackingRequest> packingRequests = new ArrayList<>();
        calculatePackSummaryRequest.setPackingList(packingRequests);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(calculatePackSummaryRequest);
        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        String errorMessage = "error while calculating summary !";
        // Mock
        when(packingService.calculatePackSummary(anyList(), any(), any(),any()))
                .thenThrow(new RunnerException(errorMessage));
        // Test
        ResponseEntity<IRunnerResponse> httpResponse =  shipmentService.calculatePackSummary(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        RunnerResponse runnerResponse  = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }

    @Test
    void calculatePackSummary_success() throws RunnerException {
        CalculatePackSummaryRequest calculatePackSummaryRequest = new CalculatePackSummaryRequest();
        List<PackingRequest> packingRequests = new ArrayList<>();
        calculatePackSummaryRequest.setPackingList(packingRequests);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(calculatePackSummaryRequest);

        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        // Mock
        when(packingService.calculatePackSummary(anyList(), any(), any(),any())).thenReturn(packSummaryResponse);
        // Test
        ResponseEntity<IRunnerResponse> httpResponse =  shipmentService.calculatePackSummary(commonRequestModel);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(packSummaryResponse), httpResponse);
    }


    @Test
    void getIdFromGuid_success() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        when(shipmentDao.findByGuid(eq(UUID.fromString("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06")))).thenReturn(Optional.of(testShipment));
        ShipmentDetailsResponse mockShipmentResponse = ShipmentDetailsResponse.builder().id(
                testShipment.getId()
        ).build();

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.getIdFromGuid(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void getGuidFromId_success() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(testShipment));
        ShipmentDetailsResponse mockShipmentResponse = ShipmentDetailsResponse.builder().guid(
                testShipment.getGuid()
        ).build();

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.getGuidFromId(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }


    @Test
    void retrieveByOrderId_success() throws RunnerException {
        String mockOrder = "test_order_id";
        ShipmentDetailsResponse mockShipResponse = objectMapper.convertValue(testShipment, ShipmentDetailsResponse.class);
        //Mock
        when(orderManagementAdapter.getOrder(mockOrder)).thenReturn(testShipment);
        when(jsonHelper.convertValue(eq(testShipment), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipResponse);

        //Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.retrieveByOrderId(mockOrder);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipResponse), httpResponse);
    }

    @Test
    void containerListForTI_fails_when_request_is_null() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.containerListForTI(commonRequestModel);
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);

        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, runnerResponse.getError().getMessage());
    }

    @Test
    void containerListForTI_fails_when_shipment_doesnt_exist() {
        TIContainerListRequest tiContainerListRequest = new TIContainerListRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(tiContainerListRequest);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.containerListForTI(commonRequestModel);
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);

        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, runnerResponse.getError().getMessage());
    }

    @Test
    void updateDateAndStatus_success() throws RunnerException {
        ShipmentDetails mockShipment = testShipment;

        LocalDateTime mockDateTime = LocalDateTime.now();
        Integer mockStatus = 2;
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(mockShipment));

        // Update with inputs
        mockShipment.getAdditionalDetails().setDateOfIssue(mockDateTime);
        mockShipment.setStatus(mockStatus);
        // Mock
        when(shipmentDao.save(eq(mockShipment), eq(false))).thenReturn(mockShipment);
        // Test
        shipmentService.updateDateAndStatus(1L, mockDateTime, mockStatus);
        // Assert
        verify(shipmentDao, times(1)).save(any(), anyBoolean());
        verify(shipmentSync, times(1)).sync(any(), eq(null), eq(null), any(), eq(false));

    }

    @Test
    void transportInstructionList_fails_on_empty_request() {
        TIListRequest tiListRequest = null;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(tiListRequest);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.transportInstructionList(commonRequestModel);
        // Assert
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, runnerResponse.getError().getMessage());
    }

    @Test
    void transportInstructionList_fails_on_empty_shipmentGuid() {
        TIListRequest tiListRequest = new TIListRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(tiListRequest);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.transportInstructionList(commonRequestModel);
        // Assert
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, runnerResponse.getError().getMessage());
    }

    @Test
    void transportInstructionList_success() {
        TIListRequest tiListRequest = new TIListRequest();
        tiListRequest.setShipmentGuid(UUID.fromString("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06"));
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(tiListRequest);

        // Mock
        when(v1Service.fetchTransportInstructionList(tiListRequest)).thenReturn(new V1DataResponse());
        List<TIResponse> mockTIResponse = List.of(new TIResponse());
        when(jsonHelper.convertValueToList(any(), eq(TIResponse.class))).thenReturn(mockTIResponse);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.transportInstructionList(commonRequestModel);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockTIResponse), httpResponse);
    }


    @Test
    void containerListForTI_success() {
        TIContainerListRequest tiContainerListRequest = new TIContainerListRequest();
        tiContainerListRequest.setShipmentGuid(UUID.fromString("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06"));
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(tiContainerListRequest);

        // Mock
        V1DataResponse mockV1DataResponse = new V1DataResponse();
        when(v1Service.fetchContainersListForTI(eq(tiContainerListRequest))).thenReturn(mockV1DataResponse);
        List<TIContainerResponse> mockContainerResponseList = new ArrayList<>();
        when(jsonHelper.convertValueToList(any(), eq(TIContainerResponse.class))).thenReturn(mockContainerResponseList);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.containerListForTI(commonRequestModel);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockContainerResponseList), httpResponse);
    }

    @Test
    void getShipmentFromConsol_fails_when_consol_is_not_present() {
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.empty());

        Long consolidationId = 100L;

        Exception e = assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentService.getShipmentFromConsol(consolidationId, "");
        });

        String errorMessage ="Failed to fetch the consolidation with id " + consolidationId;
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void getShipmentFromConsol_success() {

        Long consolidationId = 1L;
        String bookingNumber = "bookingNumber";
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().restrictHblGen(true).build());
        ConsolidationDetailsResponse consolidationDetailsResponse = objectMapper.convertValue(testConsol, ConsolidationDetailsResponse.class);
        ConsolidationListResponse consolidationListResponse = objectMapper.convertValue(testConsol, ConsolidationListResponse.class);


        // Mock
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(testConsol));
        when(modelMapper.map(any(), eq(ConsolidationDetailsResponse.class))).thenReturn(consolidationDetailsResponse);
        when(modelMapper.map(any(), eq(ConsolidationListResponse.class))).thenReturn(consolidationListResponse);
        when(jsonHelper.convertValue(any(), eq(PartiesResponse.class))).thenReturn(new PartiesResponse());

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.getShipmentFromConsol(consolidationId, bookingNumber);
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        ShipmentDetailsResponse shipmentResponse = objectMapper.convertValue(runnerResponse.getData(), ShipmentDetailsResponse.class);
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals(Constants.SYSTEM, shipmentResponse.getSource());
        assertEquals(Constants.SHIPMENT_TYPE_STD, shipmentResponse.getJobType());
        assertEquals(bookingNumber, shipmentResponse.getBookingNumber());
    }


    @Test
    void getMasterDataMappings_success() throws RunnerException, NoSuchFieldException, ClassNotFoundException, IllegalAccessException {
        TenantContext.setCurrentTenant(1);
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        List<MasterDataDescriptionResponse> mockResponse = new ArrayList<>();

        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(masterDataUtils.getMasterDataDescription(any())).thenReturn(mockResponse);


        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.getMasterDataMappings();

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);

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
                convertEntityListToDtoList(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );

        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentService.attachListShipment(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, result);
    }

    @Test
    void fetchShipmentsForConsoleId_success() throws RunnerException {
        Long consoleId = 1L;
        CommonGetRequest request = CommonGetRequest.builder().id(consoleId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        List<ConsoleShipmentMapping> consoleShipmentMappings = new ArrayList<>();
        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setShipmentId(1L);
        mapping.setConsolidationId(1L);
        consoleShipmentMappings.add(mapping);
        testShipment.setEventsList(null);
        // Set up consoleShipmentMappings as needed for your test
        when(consoleShipmentMappingDao.findByConsolidationId(consoleId)).thenReturn(consoleShipmentMappings);

        List<ShipmentDetails> shipments = List.of(testShipment);
        List<IRunnerResponse> shipmentResponse = convertEntityListToDtoList(shipments);
        PageImpl page = new PageImpl(List.of(testShipment));

        ResponseEntity<IRunnerResponse> expectedResponse = ResponseHelper.buildListSuccessResponse(
                shipmentResponse,
                page.getTotalPages(),
                page.getTotalElements()
        );

        when(shipmentDao.findAll(any(), any())).thenReturn(page);
        // Moved the below mocking part to convertEntityListToDtoList method
//        when(modelMapper.map(any(), eq(ShipmentListResponse.class))).thenReturn(objectMapper.convertValue(testShipment, ShipmentListResponse.class));

        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentService.fetchShipmentsForConsoleId(commonRequestModel);


        assertEquals(expectedResponse, result);
    }

    @Test
    void fetchActiveInvoices_throws_exception() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        String errorMessage = "Shipment Guid can't be null";

        Exception e = assertThrows(RunnerException.class, () -> shipmentService.fetchActiveInvoices(commonRequestModel));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void fetchActiveInvoices_success() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        CheckActiveInvoiceResponse mockCheckActiveInvoiceResponse = CheckActiveInvoiceResponse.builder().IsAnyActiveInvoiceFound(true).build();
        //Mock
        when(v1Service.getActiveInvoices(any())).thenReturn(mockCheckActiveInvoiceResponse);
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.fetchActiveInvoices(commonRequestModel);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockCheckActiveInvoiceResponse), httpResponse);
    }

    @Test
    void showAssignAllContainers_success_shows_dialog() {
        ShipmentConsoleIdDto request = new ShipmentConsoleIdDto();
        request.setShipmentId(1L);
        request.setConsolidationId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        AssignAllDialogDto mockResponse = new AssignAllDialogDto();
        mockResponse.setShowDialog(true);
        mockResponse.setNumberOfShipments(1);

        // Mock
        ShipmentsContainersMapping mockShipContainerMapping = new ShipmentsContainersMapping();
        mockShipContainerMapping.setShipmentId(1L);
        mockShipContainerMapping.setContainerId(1L);
        List<ShipmentsContainersMapping> mockShipContainerList = List.of(mockShipContainerMapping);

        ConsoleShipmentMapping mockConsolShipMapping = new ConsoleShipmentMapping();
        mockConsolShipMapping.setConsolidationId(1L);
        mockConsolShipMapping.setShipmentId(1L);
        List<ConsoleShipmentMapping> mockConsolShipMappingList = List.of(mockConsolShipMapping);

        Containers consolContainer1 = Containers.builder().consolidationId(1L).build();
        consolContainer1.setId(2L);
        Containers consolContainer2 = Containers.builder().consolidationId(1L).build();
        consolContainer2.setId(2L);

        when(shipmentsContainersMappingDao.findByShipmentId(eq(1L))).thenReturn(mockShipContainerList);
        when(containerDao.findByConsolidationId(1L)).thenReturn(List.of(consolContainer1, consolContainer2));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(List.of(
                ShipmentSettingsDetails.builder().build()));
        when(consoleShipmentMappingDao.findByConsolidationId(eq(1L))).thenReturn(mockConsolShipMappingList);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.showAssignAllContainers(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);

    }

    @Test
    void showAssignAllContainers_success_doesnt_show_dialog() {
        ShipmentConsoleIdDto request = new ShipmentConsoleIdDto();
        request.setShipmentId(1L);
        request.setConsolidationId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        AssignAllDialogDto mockResponse = new AssignAllDialogDto();
        mockResponse.setShowDialog(false);
        mockResponse.setNumberOfShipments(1);

        // Mock
        ShipmentsContainersMapping mockShipContainerMapping = new ShipmentsContainersMapping();
        mockShipContainerMapping.setShipmentId(1L);
        mockShipContainerMapping.setContainerId(1L);
        List<ShipmentsContainersMapping> mockShipContainerList = List.of(mockShipContainerMapping);

        ConsoleShipmentMapping mockConsolShipMapping = new ConsoleShipmentMapping();
        mockConsolShipMapping.setConsolidationId(1L);
        mockConsolShipMapping.setShipmentId(1L);
        List<ConsoleShipmentMapping> mockConsolShipMappingList = List.of(mockConsolShipMapping);

        Containers consolcontainer = Containers.builder().consolidationId(1L).build();
        consolcontainer.setId(2L);

        when(shipmentsContainersMappingDao.findByShipmentId(eq(1L))).thenReturn(mockShipContainerList);
        when(containerDao.findByConsolidationId(1L)).thenReturn(List.of(consolcontainer));
        when(consoleShipmentMappingDao.findByConsolidationId(eq(1L))).thenReturn(mockConsolShipMappingList);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.showAssignAllContainers(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);

    }

    @Test
    void fetchCreditLimit_throws_exception_on_empty_orgcode() {
        String errorMessage = "OrgCode to fetch creditLimit can't be null";
        Exception e = assertThrows(RunnerException.class, () -> {
            shipmentService.fetchCreditLimit(null, null);
        });

        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void fetchCreditLimit_success() throws RunnerException {
        String orgCode = "ORG123";
        String addressCode = "Default1";

        //Mock
        V1DataResponse mockV1Response = new V1DataResponse();
        CreditLimitResponse mockCreditLimitResponse = new CreditLimitResponse();
        mockV1Response.setEntities(List.of(mockCreditLimitResponse));
        when(v1Service.fetchCreditLimit(any())).thenReturn(mockV1Response);
        when(jsonHelper.convertValueToList(any(), eq(CreditLimitResponse.class))).thenReturn(List.of(mockCreditLimitResponse));

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.fetchCreditLimit(orgCode, addressCode);
        // Assert
        assertEquals(ResponseHelper.buildDependentServiceResponse(mockCreditLimitResponse, 0, 0), httpResponse);
    }


    @Test
    void checkCreditLimitFromV1_empty_shipment_id() {
        CheckCreditLimitFromV1Request checkCreditLimitFromV1Request = CheckCreditLimitFromV1Request.builder()
                .docType(ReportConstants.HOUSE_BILL)
                .shipmentId(1L)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(checkCreditLimitFromV1Request);

        CheckCreditLimitFromV1Response mockCreditV1Response = new CheckCreditLimitFromV1Response();
        String errorResponse = "Shipment not exist for given id";
        // Mock
        when(shipmentDao.findById(1L)).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.checkCreditLimitFromV1(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(errorResponse, runnerResponse.getError().getMessage());
    }

    @Test
    void checkCreditLimitFromV1_invalid_doc_type() {
        CheckCreditLimitFromV1Request checkCreditLimitFromV1Request = CheckCreditLimitFromV1Request.builder()
                .docType("unsupported doc")
                .shipmentId(1L)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(checkCreditLimitFromV1Request);

        CheckCreditLimitFromV1Response mockCreditV1Response = new CheckCreditLimitFromV1Response();
        String errorResponse = "Please send a valid doc type for check credit limit.";

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.checkCreditLimitFromV1(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(errorResponse, runnerResponse.getError().getMessage());
    }

    @Test
    void checkCreditLimitFromV1_success() {
        CheckCreditLimitFromV1Request checkCreditLimitFromV1Request = CheckCreditLimitFromV1Request.builder()
                .docType(ReportConstants.HOUSE_BILL)
                .shipmentId(1L)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(checkCreditLimitFromV1Request);

        CheckCreditLimitFromV1Response mockCreditV1Response = new CheckCreditLimitFromV1Response();

        // Mock
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(testShipment));
        when(v1ServiceUtil.validateCreditLimit(any(), any(), any(), any())).thenReturn(mockCreditV1Response);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.checkCreditLimitFromV1(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockCreditV1Response), httpResponse);
    }

    @Test
    void testCreateConsolidationNullCheck() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(false).build());
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        List<Containers> containers = new ArrayList<>();
        assertEquals(null, shipmentService.createConsolidation(shipmentDetails, containers));
    }

    @Test
    void testCreateConsolidationConsolidationLite() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(true).consolidationLite(false).build());
        CarrierDetails carrierDetails = CarrierDetails.builder().build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().transportMode(Constants.TRANSPORT_MODE_SEA).carrierDetails(carrierDetails).build();
        List<Containers> containers = new ArrayList<>();
        String errorMessage = "Not able to create consolidation, before adding 'New Containers' , please provide ‘Origin’ and ‘Destination’ values.";

        Exception e = assertThrows(ValidationException.class, () -> shipmentService.createConsolidation(shipmentDetails, containers));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void testCreateConsolidationConsolidationLiteSameOriginDestination() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(true).consolidationLite(false).build());
        CarrierDetails carrierDetails = CarrierDetails.builder().originPort("OriginPort").destinationPort("OriginPort").build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().transportMode(Constants.TRANSPORT_MODE_SEA).carrierDetails(carrierDetails).build();
        List<Containers> containers = new ArrayList<>();
        String errorMessage = "‘Origin’ and ‘Destination’ can't be same";

        Exception e = assertThrows(ValidationException.class, () -> shipmentService.createConsolidation(shipmentDetails, containers));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void testCreateConsolidation() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(true).consolidationLite(false).build());

        PartyRequestV2 partyRequestV2 = new PartyRequestV2();
        partyRequestV2.setTenantId(1);

        Parties parties = Parties.builder().orgCode("1").build();

        when(v1Service.getDefaultOrg()).thenReturn(partyRequestV2);
        when(modelMapper.map(any(), any())).thenReturn(parties);

        doNothing().when(consolidationService).generateConsolidationNumber(any(ConsolidationDetails.class));
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
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), eq(false))).thenReturn(consolidationDetails);

        ConsolidationDetails result = shipmentService.createConsolidation(shipmentDetails, new ArrayList<>());

        assertNotNull(result);
        assertEquals(carrierDetails, result.getCarrierDetails());
    }

    @Test
    void testCreateConsolidationDefaultDirectionExp() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(true).consolidationLite(false).build());

        PartyRequestV2 partyRequestV2 = new PartyRequestV2();
        partyRequestV2.setTenantId(1);

        Parties parties = Parties.builder().orgCode("1").build();

        when(v1Service.getDefaultOrg()).thenReturn(partyRequestV2);
        when(modelMapper.map(any(), any())).thenReturn(parties);

        doNothing().when(consolidationService).generateConsolidationNumber(any(ConsolidationDetails.class));
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
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), eq(false))).thenReturn(consolidationDetails);

        ConsolidationDetails result = shipmentService.createConsolidation(shipmentDetails, new ArrayList<>());

        assertNotNull(result);
        assertEquals(carrierDetails, result.getCarrierDetails());
    }


    @Test
    public void testExportExcel_NullRequest() throws IOException, IllegalAccessException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(null).build();
        String errorMessage = "Shipment List Request is Null";
        Exception e = assertThrows(ValidationException.class, () -> shipmentService.exportExcel(response, commonRequestModel));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    public void testExportExcel() throws IOException, IllegalAccessException {

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        CarrierDetails carrierDetails = CarrierDetails.builder()
                .origin("origin_name")
                .originPort("originPort_name")
                .destination("destination_name")
                .destinationPort("destinationPort_name")
                .build();

        shipmentDetailsList.add(ShipmentDetails.builder().status(1).carrierDetails(carrierDetails).build());

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );

        HttpServletResponse response = mock(HttpServletResponse.class);
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ServletOutputStream servletOutputStream = new ServletOutputStream() {
            @Override
            public void write(int b) throws IOException {
                outputStream.write(b);
            }

            @Override
            public boolean isReady() {
                return true;
            }

            @Override
            public void setWriteListener(javax.servlet.WriteListener writeListener) {}
        };
        when(response.getOutputStream()).thenReturn(servletOutputStream);

        ListCommonRequest sampleRequest = constructListCommonRequest("id", 1, "=");
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(sampleRequest).build();
        shipmentService.exportExcel(response, commonRequestModel);

        verify(response, times(1)).setContentType(anyString());
        verify(response, times(1)).setHeader(anyString(), anyString());
        verify(response, times(1)).getOutputStream();
        assertNotNull(outputStream.toByteArray()); // Verify that the output stream contains data
    }

    @Test
    void completeV1ShipmentCreateAndUpdate() throws RunnerException {
        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).shipmentId("AIR-CAN-00001").shipmentCreatedOn(LocalDateTime.now()).consolidationList(Arrays.asList(consolidationDetails)).build();
        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);
        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);


        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(shipmentRequest, ShipmentDetailsResponse.class);
        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateBookingCarriageNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        List<BookingCarriageRequest> bookingCarriageRequestList = Arrays.asList(BookingCarriageRequest.builder().build());
        List<BookingCarriage> bookingCarriageList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).shipmentId("AIR-CAN-00001").shipmentCreatedOn(LocalDateTime.now()).consolidationList(Arrays.asList(consolidationDetails)).bookingCarriagesList(bookingCarriageRequestList).build();
        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(bookingCarriageDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(bookingCarriageDao.updateEntityFromShipment(bookingCarriageList, 1L, new ArrayList<>())).thenReturn(bookingCarriageList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateContainersListNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        List<ContainerRequest> containerRequests = Arrays.asList(ContainerRequest.builder().build());
        List<Containers> containersList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L)
                .shipmentId("AIR-CAN-00001")
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(consolidationDetails))
                .containersList(containerRequests).build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateElDetailsNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        List<ELDetailsRequest> elDetailsRequests = Arrays.asList(ELDetailsRequest.builder().build());
        List<ELDetails> elDetailsList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).shipmentId("AIR-CAN-00001").shipmentCreatedOn(LocalDateTime.now()).consolidationList(Arrays.asList(consolidationDetails)).elDetailsList(elDetailsRequests).build();
        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(elDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(elDetailsDao.updateEntityFromShipment(elDetailsList, 1L, new ArrayList<>())).thenReturn(elDetailsList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateTruckDriverDetailsNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequests = Arrays.asList(TruckDriverDetailsRequest.builder().build());
        List<TruckDriverDetails> truckDriverDetails = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).shipmentId("AIR-CAN-00001").shipmentCreatedOn(LocalDateTime.now()).consolidationList(Arrays.asList(consolidationDetails)).truckDriverDetails(truckDriverDetailsRequests).build();
        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(truckDriverDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(truckDriverDetailsDao.updateEntityFromShipment(truckDriverDetails, 1L, new ArrayList<>())).thenReturn(truckDriverDetails);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateEventsRequestListNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        List<EventsRequest> eventsRequestList = Arrays.asList(EventsRequest.builder().build());
        List<Events> eventsList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L).shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(consolidationDetails))
                .eventsList(eventsRequestList)
                .build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(eventDao.updateEntityFromOtherEntity(eventsList, 1L, Constants.SHIPMENT, new ArrayList<>())).thenReturn(eventsList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateReferenceNumbersRequestListNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = Arrays.asList(ReferenceNumbersRequest.builder().build());
        List<ReferenceNumbers> referenceNumbersList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L).shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(consolidationDetails))
                .referenceNumbersList(referenceNumbersRequestList)
                .build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(referenceNumbersDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(referenceNumbersDao.updateEntityFromShipment(referenceNumbersList, 1L, new ArrayList<>())).thenReturn(referenceNumbersList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateRoutingsRequestListNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        List<RoutingsRequest> routingsRequestList = Arrays.asList(RoutingsRequest.builder().build());
        List<Routings> routingsList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L)
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(consolidationDetails))
                .routingsList(routingsRequestList).build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(routingsDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(routingsDao.updateEntityFromShipment(routingsList, 1L, new ArrayList<>())).thenReturn(routingsList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateServiceDetailsRequestListNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        List<ServiceDetailsRequest> serviceDetailsRequests = Arrays.asList(ServiceDetailsRequest.builder().build());
        List<ServiceDetails> serviceDetailsList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L)
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(consolidationDetails))
                .servicesList(serviceDetailsRequests)
                .build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(serviceDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(serviceDetailsDao.updateEntityFromShipment(serviceDetailsList, 1L, new ArrayList<>())).thenReturn(serviceDetailsList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateShipmentAddressesRequestListNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        List<PartiesRequest> partiesRequests = Arrays.asList(PartiesRequest.builder().build());
        List<Parties> partiesList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L)
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(consolidationDetails))
                .shipmentAddresses(partiesRequests).build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(partiesDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(partiesDao.updateEntityFromOtherEntity(partiesList, 1L, Constants.SHIPMENT_ADDRESSES, new ArrayList<>())).thenReturn(partiesList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateNotesRequestListNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        List<NotesRequest> notesRequests = Arrays.asList(NotesRequest.builder().id(1L).build());
        List<Notes> notesList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L)
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(consolidationDetails))
                .notesList(notesRequests).build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(consoleDetails));

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(notesDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(notesDao.updateEntityFromOtherEntity(notesList, 1L, Constants.SHIPMENT, new ArrayList<>())).thenReturn(notesList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), notesRequests, true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateNonEmptyConsolidation() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        List<ContainerRequest> containerRequests = Arrays.asList(ContainerRequest.builder().build());

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L)
                .shipmentId("AIR-CAN-00001")
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(consolidationDetails))
                .containersList(containerRequests).build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdatePackingRequestListNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        List<PackingRequest> packingRequestList = Arrays.asList(PackingRequest.builder().build());
        List<Packing> packingList= new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L)
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(consolidationDetails))
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .packingList(packingRequestList).build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(consoleDetails));

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(packingDao.updateEntityFromShipment(anyList(), anyLong(), anyList(), any(), any(), any())).thenReturn(packingList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void partialUpdateIdNullTest() {
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();
        String errorMessage = "Request Id is null";
        Exception e = assertThrows(RunnerException.class, () -> shipmentService.partialUpdate(commonRequestModel, true));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void partialUpdateTestEntityNotPresent() {
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();
        String errorMessage = "Failed to fetch data for given constraint.";
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> shipmentService.partialUpdate(commonRequestModel, true));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void partialUpdateTest() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consoleDetails));

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(ConsolidationDetails.builder().build()))
                .containersList(Arrays.asList(Containers.builder().build()))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());

        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void partialUpdateTestShipmentIdNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().shipmentId(JsonNullable.of("AIR-CAN-00001")).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(ConsolidationDetails.builder().build()))
                .containersList(Arrays.asList(Containers.builder().build()))
                .build();

        doNothing().when(shipmentDetailsMapper).update(any(), any());

        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(Arrays.asList(ShipmentDetails.builder().build()));
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void partialUpdateTestShipmentIdNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).shipmentId(null).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();
        String errorMessage = "Failed to fetch data for given constraint.";
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> shipmentService.partialUpdate(commonRequestModel, true));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void partialUpdateTestMultipleShipments() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().shipmentId(JsonNullable.of("AIR-CAN-00001")).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(ConsolidationDetails.builder().build()))
                .containersList(Arrays.asList(Containers.builder().build()))
                .build();

        ArrayList<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(shipmentDetails);
        shipmentDetailsList.add(shipmentDetails);

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        String errorMessage = "Multiple entries found for unique field, data issue found.";
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> shipmentService.partialUpdate(commonRequestModel, true));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void partialUpdateTestAdditionalDetailsNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).additionalDetail(AdditionalDetailRequest.builder().build()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consoleDetails));

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setShipmentId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(ConsolidationDetails.builder().build()))
                .containersList(Arrays.asList(Containers.builder().build()))
                .additionalDetails(additionalDetails)
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);

        when(additionalDetailDao.updateEntityFromShipment(any())).thenReturn(additionalDetails);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void partialUpdateTestCarrierDetailsNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).carrierDetails(CarrierPatchRequest.builder().build()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consoleDetails));

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(ConsolidationDetails.builder().build()))
                .containersList(Arrays.asList(Containers.builder().build()))
                .carrierDetails(CarrierDetails.builder().build())
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        doNothing().when(carrierDetailsMapper).update(any(), any());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void partialUpdateTestTruckDriverDetailsNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).truckDriverDetails(Arrays.asList(TruckDriverDetailsRequest.builder().build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consoleDetails));

        List<TruckDriverDetails> truckDriverDetailsList = Arrays.asList(TruckDriverDetails.builder().build());

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(ConsolidationDetails.builder().build()))
                .containersList(Arrays.asList(Containers.builder().build()))
                .truckDriverDetails(truckDriverDetailsList)
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(truckDriverDetailsDao.updateEntityFromShipment(any(), any())).thenReturn(truckDriverDetailsList);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void partialUpdateTestPackingRequestNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).packingList(Arrays.asList(PackingRequest.builder().build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consoleDetails));

        Packing packing = new Packing();
        packing.setShipmentId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(ConsolidationDetails.builder().build()))
                .containersList(Arrays.asList(Containers.builder().build()))
                .packingList(Arrays.asList(packing))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(packingDao.updateEntityFromShipment(any(), any(), any())).thenReturn(Arrays.asList(packing));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void partialUpdateTestElDetailsNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).elDetailsList(Arrays.asList(ELDetailsRequest.builder().build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consoleDetails));

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(ConsolidationDetails.builder().build()))
                .containersList(Arrays.asList(Containers.builder().build()))
                .elDetailsList(Arrays.asList(ELDetails.builder().build()))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(elDetailsDao.updateEntityFromShipment(any(), any())).thenReturn(Arrays.asList(ELDetails.builder().build()));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void partialUpdateTestEventRequestNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).eventsList(Arrays.asList(EventsRequest.builder().build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consoleDetails));

        Events events = new Events();
        events.setId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(ConsolidationDetails.builder().build()))
                .containersList(Arrays.asList(Containers.builder().build()))
                .eventsList(Arrays.asList(events))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(eventDao.updateEntityFromOtherEntity(any(), any(), any())).thenReturn(Arrays.asList(events));
        doNothing().when(eventService).updateAtaAtdInShipment(any(), any(), any());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void partialUpdateTestNoteRequestNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).notesList(Arrays.asList(NotesRequest.builder().build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consoleDetails));

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(ConsolidationDetails.builder().build()))
                .containersList(Arrays.asList(Containers.builder().build()))
                .notesList(Arrays.asList(Notes.builder().build()))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(notesDao.updateEntityFromOtherEntity(any(), any(), any())).thenReturn(Arrays.asList(Notes.builder().build()));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void partialUpdateTestReferenceNumberNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).referenceNumbersList(Arrays.asList(ReferenceNumbersRequest.builder().build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consoleDetails));

        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        referenceNumbers.setShipmentId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(ConsolidationDetails.builder().build()))
                .containersList(Arrays.asList(Containers.builder().build()))
                .referenceNumbersList(Arrays.asList(referenceNumbers))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(referenceNumbersDao.updateEntityFromShipment(any(), any())).thenReturn(Arrays.asList(referenceNumbers));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void partialUpdateTestRoutingsNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).routingsList(Arrays.asList(RoutingsRequest.builder().build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consoleDetails));

        Routings routings = new Routings();
        routings.setTenantId(1);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(ConsolidationDetails.builder().build()))
                .containersList(Arrays.asList(Containers.builder().build()))
                .routingsList(Arrays.asList(routings))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(routingsDao.updateEntityFromShipment(any(), any())).thenReturn(Arrays.asList(routings));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void partialUpdateTestServiceDetailNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder()
                .id(JsonNullable.of(1L))
                .servicesList(Arrays.asList(ServiceDetailsRequest.builder().build()))
                .build();

        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consoleDetails));

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setShipmentId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(ConsolidationDetails.builder().build()))
                .containersList(Arrays.asList(Containers.builder().build()))
                .servicesList(Arrays.asList(serviceDetails))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(serviceDetailsDao.updateEntityFromShipment(any(), any())).thenReturn(Arrays.asList(serviceDetails));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void partialUpdateTestBookingCarriageNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).bookingCarriagesList(Arrays.asList(BookingCarriageRequest.builder().build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consoleDetails));

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(Arrays.asList(ConsolidationDetails.builder().build()))
                .containersList(Arrays.asList(Containers.builder().build()))
                .bookingCarriagesList(Arrays.asList(BookingCarriage.builder().build()))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(bookingCarriageDao.updateEntityFromShipment(any(), any())).thenReturn(Arrays.asList(BookingCarriage.builder().build()));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void createShipmentInV2Test() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        CustomerBookingRequest customerBookingRequest = CustomerBookingRequest.builder().id(1L).cargoType(Constants.CARGO_TYPE_FCL).carrierDetails(CarrierDetailRequest.builder().build()).build();
        when(notesDao.findByEntityIdAndEntityType(any(), any())).thenReturn(Arrays.asList(Notes.builder().build()));

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").build();
        shipmentDetails.setGuid(UUID.randomUUID());
        ConsolidationDetailsResponse mockConsolidationDetailsResponse = new ConsolidationDetailsResponse();

        when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(ConsolidationDetailsRequest.builder().build());
        doReturn(ResponseHelper.buildSuccessResponse(ConsolidationDetailsResponse.builder().build())).when(consolidationService).createFromBooking(any());

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(shipmentDao.save(any(), eq(false))).thenReturn(shipmentDetails);
        when(notesDao.findByEntityIdAndEntityType(any(), any())).thenReturn(null);

        ShipmentDetailsResponse shipmentDetailsResponse = jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(shipmentDetailsResponse);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.createShipmentInV2(customerBookingRequest);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        for(var i: lst) {
            ShipmentListResponse response = objectMapper.convertValue(i, ShipmentListResponse.class);
            when(modelMapper.map(eq(i), eq(ShipmentListResponse.class))).thenReturn(response);
            containerCountUpdate(i, response);
            setEventData(i, response);
            if (i.getStatus() != null && i.getStatus() < ShipmentStatus.values().length)
                response.setShipmentStatus(ShipmentStatus.values()[i.getStatus()].toString());
            responseList.add(response);
        }

        return responseList;
    }
    private void containerCountUpdate(ShipmentDetails shipmentDetail, ShipmentListResponse response) {
        Long container20Count = 0L;
        Long container40Count = 0L;
        Long container20GPCount = 0L;
        Long container20RECount = 0L;
        Long container40GPCount = 0L;
        Long container40RECount = 0L;
        Set<String> containerNumber = new HashSet<>();
        if (shipmentDetail.getContainersList() != null) {
            for (Containers container : shipmentDetail.getContainersList()) {
                if(container.getContainerCode() != null) {
                    if (container.getContainerCode().contains(Constants.Cont20)) {
                        ++container20Count;
                    } else if (container.getContainerCode().contains(Constants.Cont40)) {
                        ++container40Count;
                    } else if (container.getContainerCode().equals(Constants.Cont20GP)) {
                        ++container20GPCount;
                    } else if (container.getContainerCode().equals(Constants.Cont20RE)) {
                        ++container20RECount;
                    } else if (container.getContainerCode().equals(Constants.Cont40GP)) {
                        ++container40GPCount;
                    } else if (container.getContainerCode().equals(Constants.Cont40RE)) {
                        ++container40RECount;
                    }
                }
                if (StringUtility.isNotEmpty(container.getContainerNumber())) {
                    containerNumber.add(container.getContainerNumber());
                }
            }
//            container20Count = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().contains(Constants.Cont20)).count();
//            container40Count = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().contains(Constants.Cont40)).count();
//            container20GPCount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont20GP)).count();
//            container20RECount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont20RE)).count();
//            container40GPCount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont40GP)).count();
//            container40RECount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont40RE)).count();
        }
        response.setContainer20Count(container20Count);
        response.setContainer40Count(container40Count);
        response.setContainer20GPCount(container20GPCount);
        response.setContainer20RECount(container20RECount);
        response.setContainer40GPCount(container40GPCount);
        response.setContainer40RECount(container40RECount);
        response.setContainerNumbers(containerNumber);
    }
    private void setEventData(ShipmentDetails shipmentDetail, ShipmentListResponse response) {
        if (shipmentDetail.getEventsList() != null) {
            for (Events events : shipmentDetail.getEventsList()) {
                if (StringUtility.isNotEmpty(events.getEventCode())) {
                    if (events.getEventCode().equalsIgnoreCase(Constants.INVGNTD)) {
                        response.setInvoiceDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(Constants.TAXSG)) {
                        response.setTaxDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(Constants.CSEDI)) {
                        response.setCustomsFilingDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(Constants.AMSEDI)) {
                        response.setAmsFilingDate(events.getActual());
                    }
                }
            }
        }
    }

}