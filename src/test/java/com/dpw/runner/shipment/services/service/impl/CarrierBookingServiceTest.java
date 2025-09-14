package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SyncBookingToService;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
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
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingUtil;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingValidationUtil;
import org.junit.Assert;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ExecutorService;

import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_REQUEST_NULL_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.MAIN_CARRIAGE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARRIER_BOOKING_EMAIL_TEMPLATE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
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


    @InjectMocks
    private CarrierBookingService carrierBookingService;

    private static CarrierBookingRequest carrierBookingRequest;
    private static CarrierBooking carrierBooking;
    private static ConsolidationDetails consolidationDetails;
    private static CarrierBookingResponse carrierBookingResponse;

    @BeforeAll
    static void init() {
        carrierBookingRequest = new CarrierBookingRequest();
        carrierBookingRequest.setEntityType(Constants.CONSOLIDATION);

        carrierBooking = new CarrierBooking();

        consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCarrierDetails(new CarrierDetails());

        carrierBookingResponse = new CarrierBookingResponse();

    }

    @BeforeEach
    void setUp() {
    }

    @AfterEach
    void tearDown() {
    }

    @Test
    void createCarrierBookingSuccess(){
        doNothing().when(carrierBookingValidationUtil).validateServiceType(any());
        when(carrierBookingValidationUtil.validateRequest(any(), any())).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(any(), eq(CarrierBooking.class))).thenReturn(carrierBooking);

        when(carrierBookingDao.create(any())).thenReturn(carrierBooking);
        when(jsonHelper.convertValue(any(), eq(CarrierBookingResponse.class))).thenReturn(carrierBookingResponse);

        CarrierBookingResponse response = carrierBookingService.create(carrierBookingRequest);
        assertNotNull(response);
    }

    @Test
    void createCarrierBookingSuccess1(){
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
    void updateCarrierBookingNotFound(){
        when(carrierBookingDao.findById(any())).thenThrow(new ValidationException("Not found"));
        assertThrows(ValidationException.class, () -> carrierBookingService.update(carrierBookingRequest));
    }

    @Test
    void updateCarrierBookingSuccess(){
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
    void updateCarrierBookingSuccess1(){
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
        CarrierBookingResponse mockResponse = new CarrierBookingResponse();
        mockResponse.setCarrierBlNo("CX");
        lenient().when(jsonHelper.convertValue(any(), eq(CarrierBookingResponse.class)))
                .thenReturn(mockResponse);

        // Act: Call service
        ResponseEntity<IRunnerResponse> response = carrierBookingService.list(commonRequestModel, true);

        // Assert: Verify response
        Assert.assertNotNull(response);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
        Assert.assertNotNull(response.getBody());
    }

    @Test
    void retrieveByIdTest(){
        when(carrierBookingDao.findById(any())).thenThrow(new ValidationException("Invalid id"));
        assertThrows(ValidationException.class, () -> carrierBookingService.retrieveById(any()));
    }

    @Test
    void retrieveByIdTest1(){
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
        SyncBookingToService request = new SyncBookingToService();
        request.setEntityType(CarrierBookingConstants.CARRIER_BOOKING);
        request.setEntityId(1L);

        carrierBooking.setEntityType(Constants.CONSOLIDATION);
        when(carrierBookingDao.findById(any())).thenReturn(Optional.of(carrierBooking));
        when(carrierBookingValidationUtil.validateRequest(any(), any())).thenReturn(consolidationDetails);

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
        SyncBookingToService request = new SyncBookingToService();
        request.setEntityType(CarrierBookingConstants.CARRIER_BOOKING);
        request.setEntityId(1L);

        carrierBooking.setEntityType(Constants.CONSOLIDATION);
        when(carrierBookingDao.findById(any())).thenReturn(Optional.of(carrierBooking));
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
        CarrierBooking carrierBooking = new CarrierBooking();
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
    void test_cancel_Exception(){
        when(carrierBookingDao.findById(any())).thenThrow(new ValidationException("EX"));

        assertThrows(ValidationException.class, () -> carrierBookingService.cancel(1L));
    }


    @Test
    void test_cancel(){
        when(carrierBookingDao.findById(any())).thenReturn(Optional.of(carrierBooking));
        when(iv1Service.getEmailTemplates(any())).thenReturn(new V1DataResponse());
        EmailTemplatesRequest emailTemplatesRequest = new EmailTemplatesRequest();
        emailTemplatesRequest.setType(CARRIER_BOOKING_EMAIL_TEMPLATE);
        lenient().when(jsonHelper.convertValueToList(any(), eq(EmailTemplatesRequest.class))).thenReturn(List.of(emailTemplatesRequest));

        carrierBookingService.cancel(1L);
        verify(carrierBookingDao, times(1)).findById(any());
    }

    @Test
    void test_cancel1(){
        when(carrierBookingDao.findById(any())).thenReturn(Optional.of(carrierBooking));
        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.setEntities(new CarrierBooking());
        when(iv1Service.getEmailTemplates(any())).thenReturn(v1DataResponse);
        EmailTemplatesRequest emailTemplatesRequest = new EmailTemplatesRequest();
        emailTemplatesRequest.setType(CARRIER_BOOKING_EMAIL_TEMPLATE);
        lenient().when(jsonHelper.convertValueToList(any(), eq(EmailTemplatesRequest.class))).thenReturn(List.of(emailTemplatesRequest));

        carrierBookingService.cancel(1L);
        verify(carrierBookingDao, times(1)).findById(any());
    }

    @Test
    void test_setContainerEmptyAndDropOffLocationDetails(){
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
    void test_setContainerEmptyAndDropOffLocationDetails_EMPTY_PICK_UP(){
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

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
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

        Assertions.assertEquals("Invalid value of Entity Type", exception.getMessage());
    }

    @Test
    void test_getCarrierBookingStatus_AllValidCases() {
        // Test all valid cases
        Assertions.assertEquals(CarrierBookingStatus.ConditionallyAccepted, carrierBookingService.getCarrierBookingStatus("ConditionallyAccepted"));
        Assertions.assertEquals(CarrierBookingStatus.RejectedByINTTRA, carrierBookingService.getCarrierBookingStatus("Rejected"));
        Assertions.assertEquals(CarrierBookingStatus.AcceptedByINTTRA, carrierBookingService.getCarrierBookingStatus("Accepted"));
        Assertions.assertEquals(CarrierBookingStatus.ConfirmedByCarrier, carrierBookingService.getCarrierBookingStatus("Confirmed"));
        Assertions.assertEquals(CarrierBookingStatus.DeclinedByCarrier, carrierBookingService.getCarrierBookingStatus("Declined"));
        Assertions.assertEquals(CarrierBookingStatus.ReplacedByCarrier, carrierBookingService.getCarrierBookingStatus("Replaced"));
        Assertions.assertEquals(CarrierBookingStatus.CancelledByCarrier, carrierBookingService.getCarrierBookingStatus("Cancelled"));
        Assertions.assertEquals(CarrierBookingStatus.CancelledByCarrier, carrierBookingService.getCarrierBookingStatus("Canceled"));
        Assertions.assertEquals(CarrierBookingStatus.Changed, carrierBookingService.getCarrierBookingStatus("ChangeBookingRequested"));
        Assertions.assertEquals(CarrierBookingStatus.Requested, carrierBookingService.getCarrierBookingStatus("NewBookingRequested"));
    }

    @Test
    void test_getCarrierBookingStatus_DefaultCase() {
        // Test default case
        Assertions.assertEquals(CarrierBookingStatus.PendingFromCarrier, carrierBookingService.getCarrierBookingStatus("UnknownStatus"));
        Assertions.assertEquals(CarrierBookingStatus.PendingFromCarrier, carrierBookingService.getCarrierBookingStatus(""));
    }

    @Test
    void test_getRoutingCarriage_AllValidCases() {
        // Test all valid cases
        Assertions.assertEquals(RoutingCarriage.MAIN_CARRIAGE, carrierBookingService.getRoutingCarriage("MainCarriage"));
        Assertions.assertEquals(RoutingCarriage.PRE_CARRIAGE, carrierBookingService.getRoutingCarriage("PreCarriage"));
        Assertions.assertEquals(RoutingCarriage.ON_CARRIAGE, carrierBookingService.getRoutingCarriage("OnCarriage"));
    }

    @Test
    void test_getRoutingCarriage_DefaultCase() {
        // Test default case
        Assertions.assertNull(carrierBookingService.getRoutingCarriage("UnknownStage"));
        Assertions.assertNull(carrierBookingService.getRoutingCarriage(""));
    }

    @Test
    void test_getTransportMode_AllValidCases() {
        // Test all valid cases
        Assertions.assertEquals(CarrierBookingConstants.TRANSPORT_MODE_SEA, carrierBookingService.getTransportMode("MaritimeTransport"));
        Assertions.assertEquals(CarrierBookingConstants.TRANSPORT_MODE_RAIL, carrierBookingService.getTransportMode("RailTransport"));
        Assertions.assertEquals(CarrierBookingConstants.TRANSPORT_MODE_ROAD, carrierBookingService.getTransportMode("RoadTransport"));
        Assertions.assertEquals(CarrierBookingConstants.TRANSPORT_MODE_INLAND_WATER, carrierBookingService.getTransportMode("InlandWaterTransport"));
        Assertions.assertEquals(CarrierBookingConstants.TRANSPORT_MODE_RAIL_WATER, carrierBookingService.getTransportMode("Rail_WaterTransport"));
        Assertions.assertEquals(CarrierBookingConstants.TRANSPORT_MODE_ROAD_WATER, carrierBookingService.getTransportMode("Road_WaterTransport"));
    }

    @Test
    void test_getTransportMode_DefaultCase() {
        // Test default case
        Assertions.assertNull(carrierBookingService.getTransportMode("UnknownTransport"));
        Assertions.assertNull(carrierBookingService.getTransportMode(""));
    }

    @Test
    void test_setCarrierRoutings(){
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

}
