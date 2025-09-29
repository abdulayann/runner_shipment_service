package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.adapters.config.BridgeServiceConfig;
import com.dpw.runner.shipment.services.adapters.impl.BridgeServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITransactionHistoryDao;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingBridgeRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SubmitAmendInttraRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SyncBookingToService;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingListResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.CarrierRouting;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.entity.enums.OperationType;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionStatus;
import com.dpw.runner.shipment.services.entity.enums.VerifiedGrossMassStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.CarrierBookingMasterDataHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.dto.inttra.DateInfo;
import com.dpw.runner.shipment.services.kafka.dto.inttra.Equipment;
import com.dpw.runner.shipment.services.kafka.dto.inttra.Haulage;
import com.dpw.runner.shipment.services.kafka.dto.inttra.HaulageDate;
import com.dpw.runner.shipment.services.kafka.dto.inttra.HaulageParty;
import com.dpw.runner.shipment.services.kafka.dto.inttra.HaulagePoint;
import com.dpw.runner.shipment.services.kafka.dto.inttra.InttraCarrierBookingEventDto;
import com.dpw.runner.shipment.services.kafka.dto.inttra.Location;
import com.dpw.runner.shipment.services.kafka.dto.inttra.LocationDate;
import com.dpw.runner.shipment.services.kafka.dto.inttra.Reference;
import com.dpw.runner.shipment.services.kafka.dto.inttra.TransportLeg;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsV3Service;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.IntraCommonKafkaHelper;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingInttraUtil;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingUtil;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingValidationUtil;
import org.junit.Assert;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ExecutorService;

import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_REQUEST_NULL_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.MAIN_CARRIAGE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARRIER_BOOKING_EMAIL_TEMPLATE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.VOLUME_UNIT_M3;
import static com.dpw.runner.shipment.services.commons.constants.Constants.WEIGHT_UNIT_KG;
import static com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus.Requested;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CarrierBookingServiceTest extends CommonMocks {

    @Mock
    private ICarrierBookingDao carrierBookingDao;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private CarrierBookingMasterDataHelper carrierBookingMasterDataHelper;
    @Mock
    private CarrierBookingValidationUtil carrierBookingValidationUtil;
    @Mock
    private CommonUtils commonUtils;
    @Mock
    private INotificationService notificationService;
    @Mock
    private IV1Service iv1Service;
    @Mock
    private CarrierBookingUtil carrierBookingUtil;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private ExecutorService executorServiceMasterData;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock
    private IConsolidationV3Service consolidationV3Service;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private ITransactionHistoryDao transactionHistoryDao;
    @Mock
    private IntraCommonKafkaHelper kafkaHelper;
    @Mock
    private IRoutingsV3Service routingsV3Service;
    @Mock
    private CarrierBookingInttraUtil carrierBookingInttraUtil;
    @Mock
    private BridgeServiceConfig bridgeServiceConfig;
    @Mock
    private BridgeServiceAdapter bridgeServiceAdapter;

    @Spy
    @InjectMocks
    private CarrierBookingService carrierBookingService;

    private static CarrierBookingRequest carrierBookingRequest;
    private static CarrierBooking carrierBooking;
    private static ConsolidationDetails consolidationDetails;
    private static CarrierBookingResponse carrierBookingResponse;
    private static CarrierBookingListResponse carrierBookingListResponse;
    private static ShippingInstruction shippingInstruction;
    private static VerifiedGrossMass verifiedGrossMass;
    private static ReferenceNumbers referenceNumbers;

    @BeforeAll
    static void init() {
        carrierBookingRequest = new CarrierBookingRequest();
        carrierBookingRequest.setEntityType(Constants.CONSOLIDATION);

        carrierBooking = new CarrierBooking();

        consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCarrierDetails(new CarrierDetails());

        carrierBookingResponse = new CarrierBookingResponse();

        carrierBookingListResponse = new CarrierBookingListResponse();
        shippingInstruction = new ShippingInstruction();
        verifiedGrossMass = new VerifiedGrossMass();
        referenceNumbers = new ReferenceNumbers();

    }

    @BeforeEach
    void setUp() {
        UsersDto usersDto = new UsersDto();
        usersDto.setEmail("test@abc.com");
        UserContext.setUser(usersDto);
    }
    @Test
    void testConvertEntityListToDtoList_WithValidData_ShouldReturnResponseList() {
        // Arrange
        List<CarrierBooking> carrierBookingList = Arrays.asList(carrierBooking);
        Set<String> includeColumns = new HashSet<>(Arrays.asList("column1", "column2"));

        shippingInstruction.setStatus(ShippingInstructionStatus.Draft);
        verifiedGrossMass.setStatus(VerifiedGrossMassStatus.Draft);
        referenceNumbers.setType(CarrierBookingConstants.CON);
        referenceNumbers.setReferenceNumber("REF123");

        carrierBooking.setShippingInstruction(shippingInstruction);
        carrierBooking.setVerifiedGrossMass(verifiedGrossMass);
        carrierBooking.setReferenceNumbersList(Arrays.asList(referenceNumbers));

        when(jsonHelper.convertValue(carrierBooking, CarrierBookingListResponse.class))
                .thenReturn(carrierBookingListResponse);

        // Act
        List<IRunnerResponse> result = carrierBookingService.convertEntityListToDtoList(
                carrierBookingList, true, includeColumns);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());
        assertTrue(result.get(0) instanceof CarrierBookingListResponse);

        CarrierBookingListResponse response = (CarrierBookingListResponse) result.get(0);
        assertEquals(ShippingInstructionStatus.Draft, response.getSiStatus());
        assertEquals(VerifiedGrossMassStatus.Draft, response.getVgmStatus());
        assertEquals("REF123", response.getContractNo());

        verify(jsonHelper).convertValue(carrierBooking, CarrierBookingListResponse.class);
        verify(carrierBookingMasterDataHelper).getMasterDataForList(
                carrierBookingList, result, true, true, includeColumns);
    }

    @Test
    void testConvertEntityListToDtoList_WithEmptyList_ShouldReturnEmptyList() {
        // Arrange
        List<CarrierBooking> emptyList = new ArrayList<>();
        Set<String> includeColumns = new HashSet<>();

        // Act
        List<IRunnerResponse> result = carrierBookingService.convertEntityListToDtoList(
                emptyList, false, includeColumns);

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(carrierBookingMasterDataHelper).getMasterDataForList(
                emptyList, result, false, true, includeColumns);
        verifyNoInteractions(jsonHelper);
    }

    @Test
    void testConvertEntityListToDtoList_WithNullVerifiedGrossMass_ShouldNotSetVgmStatus() {
        // Arrange
        List<CarrierBooking> carrierBookingList = Arrays.asList(carrierBooking);
        carrierBooking.setVerifiedGrossMass(null);

        when(jsonHelper.convertValue(carrierBooking, CarrierBookingListResponse.class))
                .thenReturn(carrierBookingListResponse);

        // Act
        List<IRunnerResponse> result = carrierBookingService.convertEntityListToDtoList(
                carrierBookingList, true, null);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());

        CarrierBookingListResponse response = (CarrierBookingListResponse) result.get(0);
        assertNull(response.getVgmStatus());

    }

    @Test
    void testConvertEntityListToDtoList_WithEmptyReferenceNumbersList_ShouldNotSetContractNo() {
        // Arrange
        List<CarrierBooking> carrierBookingList = Arrays.asList(carrierBooking);
        carrierBooking.setReferenceNumbersList(new ArrayList<>());

        when(jsonHelper.convertValue(carrierBooking, CarrierBookingListResponse.class))
                .thenReturn(carrierBookingListResponse);

        // Act
        List<IRunnerResponse> result = carrierBookingService.convertEntityListToDtoList(
                carrierBookingList, false, null);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());

        CarrierBookingListResponse response = (CarrierBookingListResponse) result.get(0);
        assertNull(response.getContractNo());
    }

    @Test
    void testConvertEntityListToDtoList_WithNullReferenceNumbersList_ShouldNotSetContractNo() {
        // Arrange
        List<CarrierBooking> carrierBookingList = Arrays.asList(carrierBooking);
        carrierBooking.setReferenceNumbersList(null);

        when(jsonHelper.convertValue(carrierBooking, CarrierBookingListResponse.class))
                .thenReturn(carrierBookingListResponse);

        // Act
        List<IRunnerResponse> result = carrierBookingService.convertEntityListToDtoList(
                carrierBookingList, false, null);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());

        CarrierBookingListResponse response = (CarrierBookingListResponse) result.get(0);
        assertNull(response.getContractNo());
    }

    @Test
    void testConvertEntityListToDtoList_WithNonCONReferenceType_ShouldNotSetContractNo() {
        // Arrange
        List<CarrierBooking> carrierBookingList = Arrays.asList(carrierBooking);

        ReferenceNumbers nonCONReference = new ReferenceNumbers();
        nonCONReference.setType("OTHER");
        nonCONReference.setReferenceNumber("OTHER123");

        carrierBooking.setReferenceNumbersList(Arrays.asList(nonCONReference));

        when(jsonHelper.convertValue(carrierBooking, CarrierBookingListResponse.class))
                .thenReturn(carrierBookingListResponse);

        // Act
        List<IRunnerResponse> result = carrierBookingService.convertEntityListToDtoList(
                carrierBookingList, true, null);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());

        CarrierBookingListResponse response = (CarrierBookingListResponse) result.get(0);
        assertNull(response.getContractNo());
    }

    @Test
    void testConvertEntityListToDtoList_WithMultipleReferenceNumbers_ShouldSetFirstCONReference() {
        // Arrange
        List<CarrierBooking> carrierBookingList = Arrays.asList(carrierBooking);

        ReferenceNumbers otherRef = new ReferenceNumbers();
        otherRef.setType("OTHER");
        otherRef.setReferenceNumber("OTHER123");

        ReferenceNumbers conRef1 = new ReferenceNumbers();
        conRef1.setType(CarrierBookingConstants.CON);
        conRef1.setReferenceNumber("CON123");

        ReferenceNumbers conRef2 = new ReferenceNumbers();
        conRef2.setType(CarrierBookingConstants.CON);
        conRef2.setReferenceNumber("CON456");

        carrierBooking.setReferenceNumbersList(Arrays.asList(otherRef, conRef1, conRef2));

        when(jsonHelper.convertValue(carrierBooking, CarrierBookingListResponse.class))
                .thenReturn(carrierBookingListResponse);

        // Act
        List<IRunnerResponse> result = carrierBookingService.convertEntityListToDtoList(
                carrierBookingList, true, null);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());

        CarrierBookingListResponse response = (CarrierBookingListResponse) result.get(0);
        assertEquals("CON123", response.getContractNo()); // Should be first CON reference
    }

    @Test
    void testConvertEntityListToDtoList_WithMultipleCarrierBookings_ShouldProcessAll() {
        // Arrange
        CarrierBooking carrierBooking1 = new CarrierBooking();
        CarrierBooking carrierBooking2 = new CarrierBooking();
        List<CarrierBooking> carrierBookingList = Arrays.asList(carrierBooking1, carrierBooking2);

        CarrierBookingListResponse response1 = new CarrierBookingListResponse();
        CarrierBookingListResponse response2 = new CarrierBookingListResponse();

        when(jsonHelper.convertValue(carrierBooking1, CarrierBookingListResponse.class))
                .thenReturn(response1);
        when(jsonHelper.convertValue(carrierBooking2, CarrierBookingListResponse.class))
                .thenReturn(response2);

        // Act
        List<IRunnerResponse> result = carrierBookingService.convertEntityListToDtoList(
                carrierBookingList, false, null);

        // Assert
        assertNotNull(result);
        assertEquals(2, result.size());
        assertTrue(result.contains(response1));
    }

    @Test
    void testConvertEntityListToDtoList_WithGetMasterDataFalse_ShouldCallMasterDataHelper() {
        // Arrange
        List<CarrierBooking> carrierBookingList = Arrays.asList(carrierBooking);
        Set<String> includeColumns = new HashSet<>();

        when(jsonHelper.convertValue(carrierBooking, CarrierBookingListResponse.class))
                .thenReturn(carrierBookingListResponse);

        // Act
        List<IRunnerResponse> result = carrierBookingService.convertEntityListToDtoList(
                carrierBookingList, false, includeColumns);

        // Assert
        assertNotNull(result);
        verify(carrierBookingMasterDataHelper).getMasterDataForList(
                carrierBookingList, result, false, true, includeColumns);
    }

    @Test
    void createCarrierBookingSuccess() throws RunnerException {
        doNothing().when(carrierBookingValidationUtil).validateServiceType(any());
        when(carrierBookingValidationUtil.validateRequest(any(), any())).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(any(), eq(CarrierBooking.class))).thenReturn(carrierBooking);

        when(carrierBookingDao.create(any())).thenReturn(carrierBooking);
        when(jsonHelper.convertValue(any(), eq(CarrierBookingResponse.class))).thenReturn(carrierBookingResponse);

        CarrierBookingResponse response = carrierBookingService.create(carrierBookingRequest);
        assertNotNull(response);
    }

    @Test
    void createCarrierBookingSuccess1() throws RunnerException {
        doNothing().when(carrierBookingValidationUtil).validateServiceType(any());
        when(carrierBookingValidationUtil.validateRequest(any(), any())).thenReturn(new ConsolidationDetails());
        CarrierBooking carrierBooking1 = new CarrierBooking();
        carrierBooking1.setEntityType(Constants.CONSOLIDATION);
        when(jsonHelper.convertValue(any(), eq(CarrierBooking.class))).thenReturn(carrierBooking1);
        UsersDto usersDto = new UsersDto();
        UserContext.setUser(usersDto);
        when(carrierBookingDao.create(any())).thenReturn(carrierBooking);
        when(jsonHelper.convertValue(any(), eq(CarrierBookingResponse.class))).thenReturn(carrierBookingResponse);

        CarrierBookingResponse response = carrierBookingService.create(carrierBookingRequest);
        assertNotNull(response);
    }

    @Test
    void updateCarrierBookingNotFound() {
        when(carrierBookingDao.findById(any())).thenThrow(new ValidationException("Not found"));
        assertThrows(ValidationException.class, () -> carrierBookingService.update(carrierBookingRequest));
    }

    @Test
    void updateCarrierBookingSuccess() throws RunnerException {
        when(carrierBookingDao.findById(any())).thenReturn(Optional.of(carrierBooking));
        doNothing().when(carrierBookingValidationUtil).validateServiceType(any());
        when(carrierBookingValidationUtil.validateRequest(any(), any())).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(any(), eq(CarrierBooking.class))).thenReturn(carrierBooking);

        when(carrierBookingDao.create(any())).thenReturn(carrierBooking);
        when(jsonHelper.convertValue(any(), eq(CarrierBookingResponse.class))).thenReturn(carrierBookingResponse);

        CarrierBookingResponse response = carrierBookingService.update(carrierBookingRequest);
        assertNotNull(response);
    }

    @Test
    void updateCarrierBookingSuccess1() throws RunnerException {
        CarrierBooking carrierBooking1 = new CarrierBooking();
        carrierBooking1.setEntityType(Constants.CONSOLIDATION);
        when(carrierBookingDao.findById(any())).thenReturn(Optional.of(carrierBooking1));
        doNothing().when(carrierBookingValidationUtil).validateServiceType(any());
        when(carrierBookingValidationUtil.validateRequest(any(), any())).thenReturn(new ConsolidationDetails());


        when(jsonHelper.convertValue(any(), eq(CarrierBooking.class))).thenReturn(carrierBooking1);

        when(carrierBookingDao.create(any())).thenReturn(carrierBooking);
        when(jsonHelper.convertValue(any(), eq(CarrierBookingResponse.class))).thenReturn(carrierBookingResponse);

        CarrierBookingResponse response = carrierBookingService.update(carrierBookingRequest);
        assertNotNull(response);
    }

    @Test
    void delete_Success() {
        // Mock
        when(carrierBookingDao.findById(any())).thenReturn(Optional.of(carrierBooking));

        // Test
        carrierBookingService.delete(1L);

        // Verify
        verify(carrierBookingDao).delete(carrierBooking);
    }


    @Test
    void list_Exception_NullRequest() {
        // Setup
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();

        // Test & Assert
        ValidationException exception = assertThrows(ValidationException.class, () ->
                carrierBookingService.list(commonRequestModel, true)
        );
        assertEquals(CARRIER_LIST_REQUEST_NULL_ERROR, exception.getMessage());
    }


    @Test
    void listCarrierBooking() {
        // Arrange: Build ListCommonRequest
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        listCommonRequest.setIncludeColumns(List.of("id", "containersList"));
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(listCommonRequest);

        // Mock a ShippingInstruction entity
        CarrierBooking carrierBooking1 = new CarrierBooking();
        carrierBooking1.setId(1L);
        carrierBooking1.setCarrierBlNo("CX");

        Page<CarrierBooking> resultPage = new PageImpl<>(List.of(carrierBooking1));

        // Mock repository behavior
        when(carrierBookingDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(resultPage);

        // Mock convertEntityListToDtoList behavior
        CarrierBookingListResponse mockResponse = new CarrierBookingListResponse();
        lenient().when(jsonHelper.convertValue(any(), eq(CarrierBookingListResponse.class)))
                .thenReturn(mockResponse);

        // Act: Call service
        ResponseEntity<IRunnerResponse> response = carrierBookingService.list(commonRequestModel, true);

        // Assert: Verify response
        Assert.assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        Assert.assertNotNull(response.getBody());
    }

    @Test
    void retrieveByIdTest() {
        when(carrierBookingDao.findById(any())).thenThrow(new ValidationException("Invalid id"));
        assertThrows(ValidationException.class, () -> carrierBookingService.retrieveById(1L));
    }

    @Test
    void retrieveByIdTest1() {
        carrierBooking.setEntityType(Constants.CONSOLIDATION);
        when(carrierBookingDao.findById(any())).thenReturn(Optional.of(carrierBooking));
        when(jsonHelper.convertValue(any(), eq(CarrierBookingResponse.class))).thenReturn(carrierBookingResponse);
        when(carrierBookingValidationUtil.validateRequest(any(), any())).thenReturn(consolidationDetails);
        when(carrierBookingUtil.detectContainerMismatches(any(), any())).thenReturn(new ArrayList<>());

        CarrierBookingResponse response = carrierBookingService.retrieveById(1L);
        assertNotNull(response);
    }

    @Test
    void syncCarrierBookingToService_InvalidEntityType_ThrowsException() {
        SyncBookingToService request = new SyncBookingToService();
        request.setEntityType("INVALID");
        request.setEntityId(1L);

        assertThrows(ValidationException.class,
                () -> carrierBookingService.syncCarrierBookingToService(request));
    }

    @Test
    void syncCarrierBookingToService_InvalidCarrierBookingId_ThrowsException() {
        SyncBookingToService request = new SyncBookingToService();
        request.setEntityType(CarrierBookingConstants.CARRIER_BOOKING);
        request.setEntityId(99L);

        when(carrierBookingDao.findById(any())).thenReturn(Optional.empty());

        assertThrows(ValidationException.class,
                () -> carrierBookingService.syncCarrierBookingToService(request));
    }

    @Test
    void syncCarrierBookingToService_ValidConsolidation_Success() throws RunnerException {
        carrierBooking.setStatus(CarrierBookingStatus.Draft);
        SailingInformation sailingInformation = new SailingInformation();
        carrierBooking.setSailingInformation(sailingInformation);
        CarrierRouting carrierRouting = new CarrierRouting();
        carrierRouting.setCarriageType(RoutingCarriage.ON_CARRIAGE);
        carrierRouting.setPol("DXB");
        carrierRouting.setPod("BOM");
        carrierRouting.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        carrierBooking.setCarrierRoutingList(List.of(carrierRouting));
        carrierRouting.setId(1L);
        carrierBooking.setEntityId(1L);
        carrierBooking.setEntityType(EntityType.CONSOLIDATION.name());
        SyncBookingToService request = new SyncBookingToService();
        request.setEntityType(CarrierBookingConstants.CARRIER_BOOKING);
        request.setEntityId(1L);
        Routings routings = new Routings();
        routings.setId(1L);
        routings.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routings.setVoyage("0123");
        routings.setMode(Constants.TRANSPORT_MODE_SEA);

        carrierBooking.setEntityType(Constants.CONSOLIDATION);
        when(carrierBookingDao.findById(any())).thenReturn(Optional.of(carrierBooking));
        when(carrierBookingValidationUtil.validateRequest(any(), any())).thenReturn(consolidationDetails);
        when(routingsV3Service.getRoutingsByConsolidationId(anyLong())).thenReturn(List.of(routings));
        carrierBookingService.syncCarrierBookingToService(request);

        verify(consolidationDetailsDao, times(1)).save(consolidationDetails);
    }

    @Test
    void syncCarrierBookingToService_ValidConsolidation_Success1() throws RunnerException {
        carrierBooking.setStatus(CarrierBookingStatus.Draft);
        SailingInformation sailingInformation = new SailingInformation();
        sailingInformation.setVerifiedGrossMassCutoff(LocalDateTime.now());
        sailingInformation.setReeferCutoff(LocalDateTime.now());
        sailingInformation.setShipInstructionCutoff(LocalDateTime.now());
        sailingInformation.setHazardousBookingCutoff(LocalDateTime.now());
        sailingInformation.setEmptyContainerPickupCutoff(LocalDateTime.now());
        sailingInformation.setLoadedContainerGateInCutoff(LocalDateTime.now());
        carrierBooking.setSailingInformation(sailingInformation);
        CarrierRouting carrierRouting = new CarrierRouting();
        carrierRouting.setCarriageType(RoutingCarriage.ON_CARRIAGE);
        carrierRouting.setPol("DXB");
        carrierRouting.setPod("BOM");
        carrierRouting.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        carrierBooking.setCarrierRoutingList(List.of(carrierRouting));
        carrierRouting.setId(1L);
        carrierBooking.setEntityId(1L);
        carrierBooking.setEntityType(EntityType.CONSOLIDATION.name());
        Routings routings = new Routings();
        routings.setId(1L);
        routings.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routings.setVoyage("0123");
        routings.setMode(Constants.TRANSPORT_MODE_SEA);

        SyncBookingToService request = new SyncBookingToService();
        request.setEntityType(CarrierBookingConstants.CARRIER_BOOKING);
        request.setEntityId(1L);

        carrierBooking.setEntityType(Constants.CONSOLIDATION);
        when(carrierBookingDao.findById(any())).thenReturn(Optional.of(carrierBooking));
        when(routingsV3Service.getRoutingsByConsolidationId(anyLong())).thenReturn(List.of(routings));
        when(carrierBookingValidationUtil.validateRequest(any(), any())).thenReturn(consolidationDetails);

        carrierBookingService.syncCarrierBookingToService(request);

        verify(consolidationDetailsDao, times(1)).save(consolidationDetails);
    }

    @Test
    void updateCarrierDataToBooking_NoCrBookingId_ReturnsEarly() {
        InttraCarrierBookingEventDto dto = new InttraCarrierBookingEventDto();
        dto.setReferences(Collections.emptyList()); // no CR_BOOKING_ID reference

        carrierBookingService.updateCarrierDataToBooking(dto);

        verify(carrierBookingDao, never()).save(any());
    }

    @Test
    void updateCarrierDataToBooking_InvalidBookingNo_ReturnsEarly() {
        Reference reference = new Reference();
        reference.setReferenceType(CarrierBookingConstants.CR_BOOKING_ID);
        reference.setReferenceValue("INVALID_NO");

        InttraCarrierBookingEventDto dto = new InttraCarrierBookingEventDto();
        dto.setReferences(List.of(reference));

        when(carrierBookingDao.findByBookingNo("INVALID_NO")).thenReturn(null);

        carrierBookingService.updateCarrierDataToBooking(dto);

        verify(carrierBookingDao, never()).save(any());
    }

    @Test
    void updateCarrierDataToBooking_ValidBooking_UpdatesAndSaves() {
        Reference reference = new Reference();
        reference.setReferenceType(CarrierBookingConstants.CR_BOOKING_ID);
        reference.setReferenceValue("VALID_NO");

        InttraCarrierBookingEventDto dto = new InttraCarrierBookingEventDto();
        dto.setReferences(List.of(reference));
        dto.setCarrierReferenceNumber("CARR_REF_123");
        dto.setBookingResponseType("CONFIRMED");
        dto.setGeneralComments(List.of("Test Comment"));

        DateInfo vgmDueDate = new DateInfo();
        vgmDueDate.setDateValue("20250101");
        vgmDueDate.setDateFormat("yyyyMMdd");
        dto.setVgmDueDate(vgmDueDate);

        DateInfo siDueDate = new DateInfo();
        siDueDate.setDateValue("20250102");
        siDueDate.setDateFormat("yyyyMMdd");
        dto.setSiDueDate(siDueDate);

        carrierBooking = new CarrierBooking();
        SailingInformation sailingInformation = new SailingInformation();
        carrierBooking.setSailingInformation(sailingInformation);

        when(carrierBookingDao.findByBookingNo("VALID_NO")).thenReturn(carrierBooking);
        lenient().when(commonUtils.convertToLocalDateTimeFromInttra(anyString(), anyString()))
                .thenReturn(LocalDateTime.now());
        when(carrierBookingDao.save(any())).thenReturn(carrierBooking);
        carrierBookingService.updateCarrierDataToBooking(dto);

        assertEquals("CARR_REF_123", carrierBooking.getCarrierBookingNo());
        assertEquals("Test Comment", carrierBooking.getCarrierComment());
        verify(carrierBookingDao, times(1)).save(carrierBooking);
    }

    @Test
    void getAllMasterData_CarrierBookingNotFound_ReturnsFailedResponse() {
        when(carrierBookingDao.findById(anyLong())).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> response = carrierBookingService.getAllMasterData(1L);

        assertNotNull(response);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void getAllMasterData_ValidCarrierBooking_ReturnsSuccessResponse() {
        carrierBooking = new CarrierBooking();
        SailingInformation sailingInformation = new SailingInformation();
        carrierBooking.setSailingInformation(sailingInformation);

        when(carrierBookingDao.findById(1L)).thenReturn(Optional.of(carrierBooking));
        lenient().when(commonUtils.setIncludedFieldsToResponse(any(), anySet(), any()))
                .thenReturn(new CarrierBookingResponse());

        ResponseEntity<IRunnerResponse> response = carrierBookingService.getAllMasterData(1L);

        assertNotNull(response);
    }

    @Test
    void getAllMasterData_ExceptionDuringProcessing_ReturnsFailedResponse() {
        when(carrierBookingDao.findById(anyLong())).thenThrow(new RuntimeException("Unexpected error"));

        ResponseEntity<IRunnerResponse> response = carrierBookingService.getAllMasterData(1L);

        assertNotNull(response);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void test_cancel_Exception() {
        when(carrierBookingDao.findById(any())).thenThrow(new ValidationException("EX"));

        assertThrows(ValidationException.class, () -> carrierBookingService.cancel(1L));
    }


    @Test
    void test_cancel() {
        when(carrierBookingDao.findById(any())).thenReturn(Optional.of(carrierBooking));
        when(iv1Service.getEmailTemplates(any())).thenReturn(new V1DataResponse());
        EmailTemplatesRequest emailTemplatesRequest = new EmailTemplatesRequest();
        emailTemplatesRequest.setType(CARRIER_BOOKING_EMAIL_TEMPLATE);
        lenient().when(jsonHelper.convertValueToList(any(), eq(EmailTemplatesRequest.class))).thenReturn(List.of(emailTemplatesRequest));
        when(carrierBookingDao.save(any())).thenReturn(carrierBooking);
        carrierBookingService.cancel(1L);
        verify(carrierBookingDao, times(1)).findById(any());
    }

    @Test
    void test_cancel1() {
        when(carrierBookingDao.findById(any())).thenReturn(Optional.of(carrierBooking));
        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.setEntities(new CarrierBooking());
        when(iv1Service.getEmailTemplates(any())).thenReturn(v1DataResponse);
        EmailTemplatesRequest emailTemplatesRequest = new EmailTemplatesRequest();
        emailTemplatesRequest.setType(CARRIER_BOOKING_EMAIL_TEMPLATE);
        lenient().when(jsonHelper.convertValueToList(any(), eq(EmailTemplatesRequest.class))).thenReturn(List.of(emailTemplatesRequest));
        when(carrierBookingDao.save(any())).thenReturn(carrierBooking);
        carrierBookingService.cancel(1L);
        verify(carrierBookingDao, times(1)).findById(any());
    }

    @Test
    void test_setContainerEmptyAndDropOffLocationDetails() {
        InttraCarrierBookingEventDto inttraCarrierBookingEventDto = new InttraCarrierBookingEventDto();
        Equipment equipment = new Equipment();
        Haulage haulage = new Haulage();
        HaulagePoint haulagePoint = new HaulagePoint();
        HaulageParty haulageParty = new HaulageParty();
        haulageParty.setPartyName(CarrierBookingConstants.FULL_DROP_OFF);
        haulagePoint.setHaulageParty(haulageParty);
        HaulageDate haulageDate = new HaulageDate();
        haulageDate.setHaulageDateType(CarrierBookingConstants.CLOSING_DATE);
        haulagePoint.setDates(List.of(haulageDate));
        haulage.setPoints(List.of(haulagePoint));
        equipment.setHaulage(haulage);
        inttraCarrierBookingEventDto.setEquipments(List.of(equipment));

        carrierBookingService.setContainerEmptyAndDropOffLocationDetails(inttraCarrierBookingEventDto, carrierBooking);
        assertNotNull(carrierBooking);
    }

    @Test
    void test_setContainerEmptyAndDropOffLocationDetails_EMPTY_PICK_UP() {
        InttraCarrierBookingEventDto inttraCarrierBookingEventDto = new InttraCarrierBookingEventDto();
        Equipment equipment = new Equipment();
        Haulage haulage = new Haulage();
        HaulagePoint haulagePoint = new HaulagePoint();
        HaulageParty haulageParty = new HaulageParty();
        haulageParty.setPartyName(CarrierBookingConstants.EMPTY_PICK_UP);
        haulagePoint.setHaulageParty(haulageParty);
        HaulageDate haulageDate = new HaulageDate();
        haulageDate.setHaulageDateType(CarrierBookingConstants.CLOSING_DATE);
        haulagePoint.setDates(List.of(haulageDate));
        haulage.setPoints(List.of(haulagePoint));
        equipment.setHaulage(haulage);
        inttraCarrierBookingEventDto.setEquipments(List.of(equipment));

        carrierBookingService.setContainerEmptyAndDropOffLocationDetails(inttraCarrierBookingEventDto, carrierBooking);
        assertNotNull(carrierBooking);
    }

    @Test
    void test_getDefaultCarrierBookingValues_Success() {
        // Mock data setup
        Long entityId = 1L;
        EntityType type = EntityType.CONSOLIDATION;

        consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(entityId);
        consolidationDetails.setConsolidationNumber("CONSOL123");
        consolidationDetails.setServiceLevel("FCL");
        consolidationDetails.setTerminalCutoff(LocalDateTime.now());
        consolidationDetails.setVerifiedGrossMassCutoff(LocalDateTime.now());
        consolidationDetails.setShipInstructionCutoff(LocalDateTime.now());
        consolidationDetails.setReeferCutoff(LocalDateTime.now());
        consolidationDetails.setHazardousBookingCutoff(LocalDateTime.now());
        consolidationDetails.setEarliestEmptyEquPickUp(LocalDateTime.now());

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOriginPort("SGSIN");
        carrierDetails.setDestinationPort("USNYC");
        carrierDetails.setOrigin("Singapore");
        carrierDetails.setDestination("New York");
        carrierDetails.setShippingLine("MSC");
        consolidationDetails.setCarrierDetails(carrierDetails);

        Containers container = new Containers();
        consolidationDetails.setContainersList(List.of(container));

        ReferenceNumbers referenceNumber = new ReferenceNumbers();
        referenceNumber.setType("BOOKING");
        referenceNumber.setReferenceNumber("REF123");
        consolidationDetails.setReferenceNumbersList(List.of(referenceNumber));

        Parties sendingAgent = new Parties();
        Parties receivingAgent = new Parties();
        consolidationDetails.setSendingAgent(sendingAgent);
        consolidationDetails.setReceivingAgent(receivingAgent);

        // Mock method calls
        when(consolidationDetailsDao.findConsolidationsById(entityId)).thenReturn(consolidationDetails);
        lenient().when(jsonHelper.convertValue(any(), eq(PartiesResponse.class))).thenReturn(new PartiesResponse());
        lenient().when(jsonHelper.convertValue(any(), eq(PartiesResponse.class))).thenReturn(new PartiesResponse());

        // Test
        CarrierBookingResponse result = carrierBookingService.getDefaultCarrierBookingValues(type, entityId);

        // Verify
        verify(consolidationDetailsDao, times(1)).findConsolidationsById(entityId);
        assertNotNull(result);
    }

    @Test
    void test_getDefaultCarrierBookingValues_InvalidEntityType() {
        // Mock data setup
        Long entityId = 1L;
        EntityType type = EntityType.CARRIER_BOOKING; // Assuming this is an invalid type

        // Test & Verify
        ValidationException exception = assertThrows(ValidationException.class, () -> {
            carrierBookingService.getDefaultCarrierBookingValues(type, entityId);
        });

        assertEquals("Invalid value of Entity Type", exception.getMessage());
    }

    @Test
    void test_getCarrierBookingStatus_AllValidCases() {
        // Test all valid cases
        assertEquals(CarrierBookingStatus.ConditionallyAccepted, carrierBookingService.getCarrierBookingStatus("ConditionallyAccepted"));
        assertEquals(CarrierBookingStatus.RejectedByINTTRA, carrierBookingService.getCarrierBookingStatus("Rejected"));
        assertEquals(CarrierBookingStatus.AcceptedByINTTRA, carrierBookingService.getCarrierBookingStatus("Accepted"));
        assertEquals(CarrierBookingStatus.ConfirmedByCarrier, carrierBookingService.getCarrierBookingStatus("Confirmed"));
        assertEquals(CarrierBookingStatus.DeclinedByCarrier, carrierBookingService.getCarrierBookingStatus("Declined"));
        assertEquals(CarrierBookingStatus.ReplacedByCarrier, carrierBookingService.getCarrierBookingStatus("Replaced"));
        assertEquals(CarrierBookingStatus.CancelledByCarrier, carrierBookingService.getCarrierBookingStatus("Cancelled"));
        assertEquals(CarrierBookingStatus.CancelledByCarrier, carrierBookingService.getCarrierBookingStatus("Canceled"));
        assertEquals(CarrierBookingStatus.Changed, carrierBookingService.getCarrierBookingStatus("ChangeBookingRequested"));
        assertEquals(Requested, carrierBookingService.getCarrierBookingStatus("NewBookingRequested"));
    }

    @Test
    void test_getCarrierBookingStatus_DefaultCase() {
        // Test default case
        assertEquals(CarrierBookingStatus.PendingFromCarrier, carrierBookingService.getCarrierBookingStatus("UnknownStatus"));
        assertEquals(CarrierBookingStatus.PendingFromCarrier, carrierBookingService.getCarrierBookingStatus(""));
    }

    @Test
    void test_getRoutingCarriage_AllValidCases() {
        // Test all valid cases
        assertEquals(RoutingCarriage.MAIN_CARRIAGE, carrierBookingService.getRoutingCarriage("MainCarriage"));
        assertEquals(RoutingCarriage.PRE_CARRIAGE, carrierBookingService.getRoutingCarriage("PreCarriage"));
        assertEquals(RoutingCarriage.ON_CARRIAGE, carrierBookingService.getRoutingCarriage("OnCarriage"));
    }

    @Test
    void test_getRoutingCarriage_DefaultCase() {
        // Test default case
        assertNull(carrierBookingService.getRoutingCarriage("UnknownStage"));
        assertNull(carrierBookingService.getRoutingCarriage(""));
    }

    @Test
    void test_getTransportMode_AllValidCases() {
        // Test all valid cases
        assertEquals(CarrierBookingConstants.TRANSPORT_MODE_SEA, carrierBookingService.getTransportMode("MaritimeTransport"));
        assertEquals(CarrierBookingConstants.TRANSPORT_MODE_RAIL, carrierBookingService.getTransportMode("RailTransport"));
        assertEquals(CarrierBookingConstants.TRANSPORT_MODE_ROAD, carrierBookingService.getTransportMode("RoadTransport"));
        assertEquals(CarrierBookingConstants.TRANSPORT_MODE_INLAND_WATER, carrierBookingService.getTransportMode("InlandWaterTransport"));
        assertEquals(CarrierBookingConstants.TRANSPORT_MODE_RAIL_WATER, carrierBookingService.getTransportMode("Rail_WaterTransport"));
        assertEquals(CarrierBookingConstants.TRANSPORT_MODE_ROAD_WATER, carrierBookingService.getTransportMode("Road_WaterTransport"));
    }

    @Test
    void test_getTransportMode_DefaultCase() {
        // Test default case
        assertNull(carrierBookingService.getTransportMode("UnknownTransport"));
        assertNull(carrierBookingService.getTransportMode(""));
    }

    @Test
    void test_setCarrierRoutings() {
        InttraCarrierBookingEventDto inttraCarrierBookingEventDto = new InttraCarrierBookingEventDto();
        TransportLeg transportLeg = new TransportLeg();
        transportLeg.setStage(MAIN_CARRIAGE);
        transportLeg.setMode("MaritimeTransport");
        Location location = new Location();
        transportLeg.setStartLocation(location);
        transportLeg.setEndLocation(location);
        inttraCarrierBookingEventDto.setTransportLegs(List.of(transportLeg));

        SailingInformation sailingInformation = new SailingInformation();
        carrierBooking.setSailingInformation(sailingInformation);
        carrierBookingService.setCarrierRoutings(inttraCarrierBookingEventDto, carrierBooking);
        assertNotNull(inttraCarrierBookingEventDto);
    }

    @Test
    void test_getETD_EmptyList() {
        // Test with empty list
        assertNull(carrierBookingService.getETD(Collections.emptyList()));
    }

    @Test
    void test_getETD_NullList() {
        // Test with null list
        assertNull(carrierBookingService.getETD(null));
    }

    @Test
    void test_getETD_NoMatchingType() {
        // Mock data setup
        LocationDate locationDate = new LocationDate();
        locationDate.setType("OtherDateType");
        locationDate.setDateValue("2024-01-15T10:30:00");
        locationDate.setDateFormat("yyyy-MM-ddTHH:mm:ss");

        List<LocationDate> startLocationLocationDates = List.of(locationDate);

        // Test
        LocalDateTime result = carrierBookingService.getETD(startLocationLocationDates);

        // Verify
        assertNull(result);
        verify(commonUtils, never()).convertToLocalDateTimeFromInttra(any(), any());
    }


    @Test
    void test_getETA_EmptyList() {
        // Test with empty list
        assertNull(carrierBookingService.getETA(Collections.emptyList()));
    }

    @Test
    void test_getETA_NullList() {
        // Test with null list
        assertNull(carrierBookingService.getETA(null));
    }

    @Test
    void test_getETA_NoMatchingType() {
        // Mock data setup
        LocationDate locationDate = new LocationDate();
        locationDate.setType("OtherDateType");
        locationDate.setDateValue("2024-01-20T14:45:00");
        locationDate.setDateFormat("yyyy-MM-ddTHH:mm:ss");

        List<LocationDate> endLocationLocationDates = List.of(locationDate);

        // Test
        LocalDateTime result = carrierBookingService.getETA(endLocationLocationDates);

        // Verify
        assertNull(result);
        verify(commonUtils, never()).convertToLocalDateTimeFromInttra(any(), any());
    }

    @Test
    void submitAmend_Exception(){
        when(carrierBookingDao.findById(any())).thenThrow(new ValidationException("EX"));
        SubmitAmendInttraRequest submitAmendInttraRequest = new SubmitAmendInttraRequest();
        assertThrows(ValidationException.class, () -> carrierBookingService.submitOrAmend(submitAmendInttraRequest));
    }

    @Test
    void submitAmend_Submit() throws RunnerException {
        carrierBooking.setForwardingAgent(createForwardingAgent());
        when(carrierBookingDao.findById(any())).thenReturn(Optional.of(carrierBooking));
        SubmitAmendInttraRequest submitAmendInttraRequest = new SubmitAmendInttraRequest();
        submitAmendInttraRequest.setOperationType(OperationType.SUBMIT);

        when(carrierBookingDao.save(any())).thenReturn(carrierBooking);

        when(jsonHelper.convertValue(any(), eq(CarrierBookingBridgeRequest.class))).thenReturn(new CarrierBookingBridgeRequest());

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = carrierBooking;
        when(iv1Service.getEmailTemplates(any())).thenReturn(v1DataResponse);

        EmailTemplatesRequest emailTemplatesRequest = new EmailTemplatesRequest();
        emailTemplatesRequest.setType(CARRIER_BOOKING_EMAIL_TEMPLATE);

        List<EmailTemplatesRequest> emailTemplatesRequests = List.of(emailTemplatesRequest);
        when(jsonHelper.convertValueToList(any(), eq(EmailTemplatesRequest.class))).thenReturn(emailTemplatesRequests);

        carrierBookingService.submitOrAmend(submitAmendInttraRequest);

        verify(carrierBookingDao).findById(any());
    }

    @Test
    void submitAmend_Amend() throws RunnerException {

        CommonContainerResponse commonContainerResponse = new CommonContainerResponse();
        commonContainerResponse.setGrossWeight(BigDecimal.valueOf(1));
        commonContainerResponse.setVolume(BigDecimal.valueOf(1));
        commonContainerResponse.setGrossWeightUnit(WEIGHT_UNIT_KG);
        commonContainerResponse.setVolumeUnit(VOLUME_UNIT_M3);

        List<CommonContainerResponse> commonContainerResponses = List.of(commonContainerResponse);

        CarrierBookingBridgeRequest carrierBookingBridgeRequest = new CarrierBookingBridgeRequest();
        carrierBookingBridgeRequest.setContainersList(commonContainerResponses);

        when(carrierBookingDao.findById(any())).thenReturn(Optional.of(carrierBooking));
        SubmitAmendInttraRequest submitAmendInttraRequest = new SubmitAmendInttraRequest();
        submitAmendInttraRequest.setOperationType(OperationType.AMEND);

        when(carrierBookingDao.save(any())).thenReturn(carrierBooking);

        when(jsonHelper.convertValue(any(), eq(CarrierBookingBridgeRequest.class))).thenReturn(carrierBookingBridgeRequest);

        carrierBookingService.submitOrAmend(submitAmendInttraRequest);

        verify(carrierBookingDao, times(1)).save(any());
    }

    private Parties createForwardingAgent() {
        return Parties.builder()
                .orgData(Map.of(
                        "RemoteIdType", "INTRA_COMPANY_ID",
                        "RemoteId", "123"
                ))
                .addressData(Map.of(
                        "Address1", "Chandni Chowk",
                        "City", "Delhi",
                        "Country", "IND",
                        "ZipPostCode", "110006",
                        "ContactPhone", "9898776611"
                ))
                .build();
    }
}
