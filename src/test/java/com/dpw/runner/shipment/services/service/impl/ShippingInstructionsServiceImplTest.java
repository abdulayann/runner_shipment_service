package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.controller.ShippingInstructionsController;
import com.dpw.runner.shipment.services.dao.impl.ShippingInstructionDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerPackageSiPayload;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShippingInstructionContainerWarningResponse;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SailingInformationRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.ShippingInstructionRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ShippingInstructionMasterDataHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.notification.request.SendEmailBaseRequest;
import com.dpw.runner.shipment.services.projection.CarrierBookingInfoProjection;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.IntraCommonKafkaHelper;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.ShipmentInstructionUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assertions;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShippingInstructionsServiceImplTest {

    @InjectMocks
    private ShippingInstructionsServiceImpl service;

    @Mock
    private ShippingInstructionDao repository;
    @Mock
    private ICarrierBookingDao carrierBookingDao;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    IntraCommonKafkaHelper kafkaHelper;

    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private ExecutorService executorServiceMasterData;
    @Mock
    private CommonUtils commonUtils;
    @Mock
    private IV1Service v1Service;

    @Mock
    private INotificationService notificationService;
    @Mock
    private ShippingInstructionMasterDataHelper shippingInstructionMasterDataHelper;
    @InjectMocks
    private ShippingInstructionsController controller;
    private static ShippingInstruction testSI;
    private static ObjectMapper objectMapper;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private IPackingV3Service packingV3Service;
    private final ShipmentInstructionUtil shipmentInstructionUtil = new ShipmentInstructionUtil();

    private static JsonTestUtility jsonTestUtility;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
    }

    // --- Helpers to create simple domain objects we need ---

    private ShippingInstructionRequest buildSimpleRequest() {
        return ShippingInstructionRequest.builder()
                .entityType(EntityType.CARRIER_BOOKING)
                .entityId(100L)
                .noOfFreightCopies(2)
                .nonNegoFreightCopies(2)
                .noOfUnFreightCopies(2)
                .nonNegoUnFreightCopies(2)
                .sailingInformation(SailingInformationRequest.builder().build())
                .build();
    }

    @BeforeEach
    void setUp() {
        testSI = jsonTestUtility.getTestShippingInstruction();
        service.executorServiceMasterData = Executors.newFixedThreadPool(2);
    }


    private ShippingInstruction buildSimpleEntity() {
        ShippingInstruction si = new ShippingInstruction();
        si.setEntityType(EntityType.CARRIER_BOOKING);
        si.setEntityId(100L);
        si.setNoOfFreightCopies(2);
        si.setNonNegoFreightCopies(2);
        si.setNoOfUnFreightCopies(2);
        si.setNonNegoUnFreightCopies(2);
        si.setSailingInformation(new SailingInformation());
        return si;
    }


    private CommonContainers buildContainer(String number, int packs, String packUnit, BigDecimal weight, String weightUnit) {
        CommonContainers c = new CommonContainers();
        c.setContainerNo(number);
        c.setPacks(packs);
        c.setPacksUnit(packUnit);
        c.setGrossWeight(weight);
        c.setGrossWeightUnit(weightUnit);
        return c;
    }

    private CommonPackages buildPackage(String number, int packs, String packUnit, BigDecimal weight, String weightUnit) {
        CommonPackages p = new CommonPackages();
        p.setContainerNo(number);
        p.setPacks(packs);
        p.setPacksUnit(packUnit);
        p.setGrossWeight(weight);
        p.setGrossWeightUnit(weightUnit);
        return p;
    }

    private CarrierBooking buildCarrierBooking() {
        CarrierBooking cb = new CarrierBooking();
        cb.setId(100L);
        cb.setEntityId(200L); // used to fetch ConsolidationDetails
        cb.setStatus(CarrierBookingStatus.Draft);
        cb.setCarrierBookingNo("CB-001");
        cb.setCarrierBlNo("BL-001");
        return cb;
    }

    private ConsolidationDetails buildConsolidationDetails() {
        ConsolidationDetails consol = new ConsolidationDetails();
        consol.setId(200L);
        consol.setBookingStatus("Confirmed");
        consol.setConsolidationNumber("CON-001");
        return consol;
    }

    // ========== POSITIVE CASES ==========

    @Test
    void createShippingInstruction_ShouldValidate_Save_AndReturnResponse() {
        // Arrange
        ShippingInstructionRequest request = buildSimpleRequest();
        ShippingInstruction entity = buildSimpleEntity();
        ShippingInstruction saved = buildSimpleEntity(); // same back
        ShippingInstructionResponse response = ShippingInstructionResponse.builder()
                .status(ShippingInstructionStatus.Draft.name())
                .carrierBookingNo("CB-001")
                .carrierBlNo("BL-001")
                .build();

        CarrierBooking cb = buildCarrierBooking();
        ConsolidationDetails consol = buildConsolidationDetails();

        when(jsonHelper.convertValue(eq(request), eq(ShippingInstruction.class))).thenReturn(entity);
        when(carrierBookingDao.findById(100L)).thenReturn(Optional.of(cb));
        when(consolidationDetailsDao.findById(200L)).thenReturn(Optional.of(consol));
        when(repository.save(any(ShippingInstruction.class))).thenReturn(saved);
        when(jsonHelper.convertValue(eq(saved), eq(ShippingInstructionResponse.class))).thenReturn(response);

        // Act
        ShippingInstructionResponse out = service.createShippingInstruction(request);

        // Assert
        assertThat(out).isNotNull();
        assertThat(out.getStatus()).isEqualTo(ShippingInstructionStatus.Draft.name());
        verify(repository, times(1)).save(any(ShippingInstruction.class));
        verify(carrierBookingDao, times(1)).findById(100L);
        verify(consolidationDetailsDao, times(1)).findById(200L);
    }


    @Test
    void getShippingInstructionsById_ShouldReturnResponse_WhenFound() {// Arrange
        ShippingInstruction entity = buildSimpleEntity();
        when(repository.findById(1L)).thenReturn(Optional.of(entity));

        ShippingInstructionResponse resp =
                ShippingInstructionResponse.builder().carrierBookingNo("CB-001").build();

        // FIX: match by type instead of exact instance
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(resp);

        // Act
        ShippingInstructionResponse out = service.getShippingInstructionsById(1L);

        // Assert
        assertThat(out).isNotNull();
        assertThat(out.getCarrierBookingNo()).isEqualTo("CB-001");
        verify(repository).findById(1L);
    }

    @Test
    void updateShippingInstructions_ShouldValidate_Save_AndReturnResponse() {
        // Arrange
        ShippingInstructionRequest request = buildSimpleRequest();
        // For update, let’s go via CONSOLIDATION branch to cover it
        ShippingInstruction entity = buildSimpleEntity();
        entity.setEntityType(EntityType.CONSOLIDATION);
        entity.setEntityId(999L);
        entity.setSailingInformation(new SailingInformation());
        entity.setStatus(ShippingInstructionStatus.SIAccepted);

        // Mock ConsolidationDetails deeply to avoid building complex graphs
        ConsolidationDetails consol = mock(ConsolidationDetails.class, RETURNS_DEEP_STUBS);
        when(consol.getCarrierDetails().getOrigin()).thenReturn("ORI");
        when(consol.getCarrierDetails().getOriginPort()).thenReturn("ORIP");
        when(consol.getCarrierDetails().getDestinationPort()).thenReturn("DESTP");
        when(consol.getCarrierDetails().getDestination()).thenReturn("DEST");
        when(consol.getShipInstructionCutoff()).thenReturn(LocalDateTime.now());
        when(consol.getVerifiedGrossMassCutoff()).thenReturn(LocalDateTime.now());

        when(repository.findById(request.getId())).thenReturn(Optional.of(entity));

        when(jsonHelper.convertValue(eq(request), eq(ShippingInstruction.class))).thenReturn(entity);
        when(consolidationDetailsDao.findById(999L)).thenReturn(Optional.of(consol));

        ShippingInstruction saved = entity;
        ShippingInstructionResponse resp = ShippingInstructionResponse.builder().carrierBookingNo(null).build();
        when(repository.save(any(ShippingInstruction.class))).thenReturn(saved);
        when(jsonHelper.convertValue(eq(saved), eq(ShippingInstructionResponse.class))).thenReturn(resp);

        // Act
        ShippingInstructionResponse out = service.updateShippingInstructions(request);

        // Assert
        assertThat(out).isNotNull();
        verify(consolidationDetailsDao).findById(999L);
        verify(repository).save(any(ShippingInstruction.class));
    }

    @Test
    void deleteShippingInstructions_ShouldInvokeDao() {
        // Act
        service.deleteShippingInstructions(55L);
        // Assert
        verify(repository).delete(55L);
    }

    // ========== NEGATIVE CASES (VALIDATION) ==========
    // We drive validation through create/update (validateFetchAndSetSI is private)

    @Test
    void create_ShouldThrow_WhenNoOfFreightCopies_GreaterThan100() {
        // Arrange
        ShippingInstructionRequest req = ShippingInstructionRequest.builder().build();
        ShippingInstruction bad = mock(ShippingInstruction.class, RETURNS_DEEP_STUBS);
        when(bad.getEntityType()).thenReturn(EntityType.CARRIER_BOOKING);
        // Set just the first invalid field; others valid
        when(bad.getNoOfFreightCopies()).thenReturn(101);
        when(bad.getSailingInformation()).thenReturn(new SailingInformation());
        when(jsonHelper.convertValue(eq(req), eq(ShippingInstruction.class))).thenReturn(bad);

        // Act + Assert
        assertThatThrownBy(() -> service.createShippingInstruction(req))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid freight copies number!");
        verifyNoInteractions(repository);
    }

    @Test
    void create_ShouldThrow_WhenNonNegoFreightCopies_Negative() {
        ShippingInstructionRequest req = ShippingInstructionRequest.builder().build();
        CarrierBooking cb = buildCarrierBooking();

        ShippingInstruction bad = mock(ShippingInstruction.class, RETURNS_DEEP_STUBS);
        when(bad.getEntityType()).thenReturn(EntityType.CARRIER_BOOKING);
        when(bad.getNoOfFreightCopies()).thenReturn(1);
        when(bad.getNonNegoFreightCopies()).thenReturn(-1); // invalid
        when(bad.getSailingInformation()).thenReturn(new SailingInformation());
        when(bad.getShippingInstructionType()).thenReturn(ShippingInstructionType.EXPRESS);

        when(jsonHelper.convertValue(eq(req), eq(ShippingInstruction.class))).thenReturn(bad);

        assertThatThrownBy(() -> service.createShippingInstruction(req))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid getNonNegoFreightCopies!");
        verifyNoInteractions(repository);
    }

    @Test
    void create_ShouldThrow_WhenNoOfUnFreightCopies_GreaterThan100() {
        ShippingInstructionRequest req = ShippingInstructionRequest.builder().build();
        ShippingInstruction bad = mock(ShippingInstruction.class, RETURNS_DEEP_STUBS);
        when(bad.getEntityType()).thenReturn(EntityType.CARRIER_BOOKING);
        when(bad.getNoOfFreightCopies()).thenReturn(1);
        when(bad.getNoOfUnFreightCopies()).thenReturn(1000); // invalid
        when(bad.getSailingInformation()).thenReturn(new SailingInformation());
        when(jsonHelper.convertValue(eq(req), eq(ShippingInstruction.class))).thenReturn(bad);

        assertThatThrownBy(() -> service.createShippingInstruction(req))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid un freight copies number!");
        verifyNoInteractions(repository);
    }

    @Test
    void create_ShouldThrow_WhenNonNegoUnFreightCopies_Negative() {
        ShippingInstructionRequest req = ShippingInstructionRequest.builder().build();
        ShippingInstruction bad = mock(ShippingInstruction.class, RETURNS_DEEP_STUBS);
        when(bad.getEntityType()).thenReturn(EntityType.CARRIER_BOOKING);
//        when(bad.getEntityId()).thenReturn(1L);
        when(bad.getNoOfFreightCopies()).thenReturn(1);
        when(bad.getNonNegoFreightCopies()).thenReturn(1);
        when(bad.getNoOfUnFreightCopies()).thenReturn(1);
        when(bad.getNonNegoUnFreightCopies()).thenReturn(-5); // invalid
        when(bad.getSailingInformation()).thenReturn(new SailingInformation());
        when(bad.getShippingInstructionType()).thenReturn(ShippingInstructionType.EXPRESS);
        when(jsonHelper.convertValue(eq(req), eq(ShippingInstruction.class))).thenReturn(bad);

        assertThatThrownBy(() -> service.createShippingInstruction(req))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid getNonNegoUnFreightCopies!");
        verifyNoInteractions(repository);
    }

    @Test
    void getShippingInstructionsById_ShouldThrow_WhenNotFound() {
        when(repository.findById(999L)).thenReturn(Optional.empty());
        assertThatThrownBy(() -> service.getShippingInstructionsById(999L))
                .isInstanceOf(DataRetrievalFailureException.class)
                .hasMessageContaining(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
    }

    @Test
    void getAllMasterDataForShippingInstruction() {
        Long id = 1L;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(id);
        boolean isShipment = true;

        ShippingInstruction mockSI = testSI;
        ShippingInstructionResponse mockAwbResponse = objectMapper.convertValue(mockSI, ShippingInstructionResponse.class);

        when(repository.findById(id)).thenReturn(Optional.ofNullable(mockSI));

        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });

        var res = service.getAllMasterData(commonRequestModel.getId());

        assertNotNull(res);
        assertEquals(HttpStatus.OK, res.getStatusCode());
    }

    @Test
    void getAllMasterDataFailsOnNoAwbPresent() {
        Long id = 1L;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(id);

        when(repository.findById(id)).thenReturn(Optional.empty());

        var res = service.getAllMasterData(commonRequestModel.getId());

        Assertions.assertNotNull(res);
        assertEquals(HttpStatus.BAD_REQUEST, res.getStatusCode());
    }

    @Test
    void listShippingInstruction() {
        // Arrange: Build ListCommonRequest
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        listCommonRequest.setIncludeColumns(List.of("id", "containersList"));
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(listCommonRequest);

        // Mock a ShippingInstruction entity
        ShippingInstruction shippingInstruction = new ShippingInstruction();
        shippingInstruction.setId(1L);
        shippingInstruction.setCarrierBlNo("CX");

        Page<ShippingInstruction> resultPage = new PageImpl<>(List.of(shippingInstruction));

        // Mock repository behavior
        when(repository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(resultPage);

        // Mock convertEntityListToDtoList behavior
        ShippingInstructionResponse mockResponse = new ShippingInstructionResponse();
        mockResponse.setCarrierBlNo("CX");
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(mockResponse);

        // Act: Call service
        ResponseEntity<IRunnerResponse> response = service.list(commonRequestModel, true);

        // Assert: Verify response
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());

        // Verify repository interaction
        verify(repository, times(1)).findAll(any(Specification.class), any(Pageable.class));
    }

    @Test
    void createShippingInstruction_ShouldThrow_WhenFreightDetailMissingPayerLocation() {
        // Arrange
        ShippingInstructionRequest req = ShippingInstructionRequest.builder().build();

        ShippingInstruction si = new ShippingInstruction();
        si.setEntityType(EntityType.CONSOLIDATION);
        si.setEntityId(123L);

        FreightDetail badFreight = new FreightDetail();
        badFreight.setChargeType("OriginTerminalHandling");
        badFreight.setPaymentTerms("Prepaid");
        badFreight.setPayerType(PayerType.SHIPPER);
        badFreight.setPayerLocation(null); // MISSING mandatory field
        si.setFreightDetailList(List.of(badFreight));

        when(jsonHelper.convertValue(eq(req), eq(ShippingInstruction.class))).thenReturn(si);

        // Act + Assert
        assertThatThrownBy(() -> service.createShippingInstruction(req))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Payment Location (payerLocation) is mandatory");

        verifyNoInteractions(repository); // save should never happen
    }


    @Test
    void submitShippingInstruction_success_whenCarrierBookingConfirmed() {
        Long id = 1L;

        ShippingInstruction si = new ShippingInstruction();
        si.setId(id);
        si.setEntityType(EntityType.CARRIER_BOOKING);
        si.setEntityId(100L);
        si.setStatus(ShippingInstructionStatus.Draft);

        CarrierBooking booking = new CarrierBooking();
        booking.setId(100L);
        booking.setStatus(CarrierBookingStatus.ConfirmedByCarrier);

        when(repository.findById(id)).thenReturn(Optional.of(si));
        when(carrierBookingDao.findById(100L)).thenReturn(Optional.of(booking));
        when(repository.save(any())).thenAnswer(inv -> inv.getArgument(0));
        when(jsonHelper.convertValue(any(), eq(ShippingInstructionResponse.class)))
                .thenReturn(new ShippingInstructionResponse());
        when(jsonHelper.convertToJson(any(ContainerPackageSiPayload.class)))
                .thenReturn("{\"containerDetail\":[],\"packageDetail\":[]}");
        when(jsonHelper.convertToJson(any(ShippingInstruction.class)))
                .thenReturn("{\"id\":1,\"entityType\":\"CARRIER_BOOKING\"}");

        ShippingInstructionResponse resp = service.submitShippingInstruction(id);

        assertNotNull(resp);
        verify(repository).save(argThat(s -> s.getStatus() == ShippingInstructionStatus.SISubmitted));
        verify(kafkaHelper).sendDataToKafka(anyString(), eq(GenericKafkaMsgType.SI), eq(IntraKafkaOperationType.ORIGINAL));
    }

    @Test
    void submitShippingInstruction_shouldThrow_whenCarrierBookingNotConfirmed() {
        Long id = 2L;

        ShippingInstruction si = new ShippingInstruction();
        si.setId(id);
        si.setEntityType(EntityType.CARRIER_BOOKING);
        si.setEntityId(101L);
        si.setStatus(ShippingInstructionStatus.Draft);

        CarrierBooking booking = new CarrierBooking();
        booking.setId(101L);
        booking.setStatus(CarrierBookingStatus.Draft); // not allowed for submit

        when(repository.findById(id)).thenReturn(Optional.of(si));
        when(carrierBookingDao.findById(101L)).thenReturn(Optional.of(booking));

        ValidationException ex = assertThrows(ValidationException.class, () -> service.submitShippingInstruction(id));
        assertThat(ex.getMessage()).contains("Submit not allowed");
    }

    @Test
    void submitShippingInstruction_success_whenConsolidationAndDraft() {
        Long id = 3L;

        ShippingInstruction si = new ShippingInstruction();
        si.setId(id);
        si.setEntityType(EntityType.CONSOLIDATION);
        si.setEntityId(200L);
        si.setStatus(ShippingInstructionStatus.Draft); // must be Draft to submit

        // prevent NPE in payload builder
        si.setCommonPackagesList(Collections.emptyList());
        si.setCommonContainersList(Collections.emptyList());

        when(repository.findById(id)).thenReturn(Optional.of(si));
        when(repository.save(any(ShippingInstruction.class))).thenAnswer(inv -> inv.getArgument(0));

        // stub response conversion
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(new ShippingInstructionResponse());

        when(jsonHelper.convertToJson(any(ContainerPackageSiPayload.class)))
                .thenReturn("{\"containerDetail\":[],\"packageDetail\":[]}");

        ShippingInstructionResponse resp = service.submitShippingInstruction(id);

        assertNotNull(resp);
        verify(repository).save(argThat(s -> s.getStatus() == ShippingInstructionStatus.SISubmitted));
    }

    @Test
    void submitShippingInstruction_shouldThrow_whenConsolidationNotDraft() {
        Long id = 4L;

        ShippingInstruction si = new ShippingInstruction();
        si.setId(id);
        si.setEntityType(EntityType.CONSOLIDATION);
        si.setEntityId(200L);
        si.setStatus(ShippingInstructionStatus.SIAccepted); // not Draft

        when(repository.findById(id)).thenReturn(Optional.of(si));

        ValidationException ex = assertThrows(ValidationException.class, () -> service.submitShippingInstruction(id));
        assertThat(ex.getMessage()).contains("Submit not allowed. Shipping Instruction is not Submitted.");
    }

    @Test
    void amendShippingInstruction_success_changesStatusAndSendsDownstream() {
        Long id = 5L;

        ShippingInstruction si = new ShippingInstruction();
        si.setId(id);
        si.setStatus(ShippingInstructionStatus.SISubmitted);
        si.setCommonPackagesList(new ArrayList<>());
        si.setCommonContainersList(new ArrayList<>());

        when(repository.findById(id)).thenReturn(Optional.of(si));
        when(repository.save(any(ShippingInstruction.class))).thenAnswer(inv -> inv.getArgument(0));
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(new ShippingInstructionResponse());
        when(jsonHelper.convertToJson(any(ShippingInstruction.class)))
                .thenReturn("{\"id\":1,\"entityType\":\"CARRIER_BOOKING\"}");
        when(jsonHelper.convertToJson(any(ContainerPackageSiPayload.class)))
                .thenReturn("{\"packageDetail\":[],\"containerDetail\":[]}");

        ShippingInstructionResponse resp = service.amendShippingInstruction(id);

        assertNotNull(resp);
        verify(repository).save(argThat(s -> s.getStatus() == ShippingInstructionStatus.SIAmendRequested));
        verify(kafkaHelper).sendDataToKafka(anyString(), any(), any());

        verify(jsonHelper).convertToJson(any(ContainerPackageSiPayload.class));
        verify(jsonHelper).convertToJson(any(ShippingInstruction.class));
    }

    @Test
    void amendShippingInstruction_shouldThrow_whenNotSubmittedOrAccepted() {
        Long id = 6L;

        ShippingInstruction si = new ShippingInstruction();
        si.setId(id);
        si.setStatus(ShippingInstructionStatus.Draft); // cannot amend

        when(repository.findById(id)).thenReturn(Optional.of(si));

        ValidationException ex = assertThrows(ValidationException.class, () -> service.amendShippingInstruction(id));
        assertThat(ex.getMessage()).contains("Amendment not allowed. Shipping Instruction is not Submitted.");
    }

    @Test
    void getDefaultShippingInstructionValues_populatesBookingStatus_andReturnsDraft() {
        Long bookingId = 100L;
        Long consolidationId = 200L;

        CarrierBooking cb = new CarrierBooking();
        cb.setId(bookingId);
        cb.setEntityId(consolidationId);
        cb.setStatus(CarrierBookingStatus.ConfirmedByCarrier);

        ConsolidationDetails consol = new ConsolidationDetails();
        consol.setId(consolidationId);
        consol.setConsolidationNumber("CON-123");

        when(carrierBookingDao.findById(bookingId)).thenReturn(Optional.of(cb));
        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consol));

        // Make jsonHelper.convertValue return a response carrying the SI status if set on the passed SI
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenAnswer(inv -> {
                    ShippingInstruction arg = inv.getArgument(0);
                    ShippingInstructionResponse r = new ShippingInstructionResponse();
                    if (arg != null && arg.getStatus() != null) {
                        r.setStatus(arg.getStatus().name());
                    }
                    return r;
                });

        ShippingInstructionResponse resp = service.getDefaultShippingInstructionValues(EntityType.CARRIER_BOOKING, bookingId);

        assertNotNull(resp);
        assertThat(resp.getStatus()).isEqualTo(ShippingInstructionStatus.Draft.name());
        assertThat(resp.getBookingStatus()).isEqualTo(cb.getStatus().name());
    }

    @Test
    void createShippingInstruction_populatesCommonPackagesAndContainers_beforeSave() {
        // Arrange request -> shippingInstruction conversion
        ShippingInstructionRequest request = buildSimpleRequest();

        ShippingInstruction siFromReq = new ShippingInstruction();
        siFromReq.setEntityType(EntityType.CARRIER_BOOKING);
        siFromReq.setEntityId(100L);
        siFromReq.setSailingInformation(new SailingInformation());

        when(jsonHelper.convertValue(eq(request), eq(ShippingInstruction.class))).thenReturn(siFromReq);

        // CarrierBooking -> consolidation id
        CarrierBooking cb = new CarrierBooking();
        cb.setId(100L);
        cb.setEntityId(200L);
        cb.setStatus(CarrierBookingStatus.ConfirmedByCarrier);
//        when(carrierBookingDao.findById(100L)).thenReturn(Optional.of(cb));

        when(carrierBookingDao.findById(anyLong())).thenReturn(Optional.of(cb));

        // Consolidation -> one container
        ConsolidationDetails consol = new ConsolidationDetails();
        consol.setId(200L);
        Containers container = new Containers();
        container.setId(10L);
        container.setContainerNumber("C-10");
        // ensure containersList getter is used by service
        consol.setContainersList(List.of(container));
        when(consolidationDetailsDao.findById(200L)).thenReturn(Optional.of(consol));

        // Packing list -> one packing referencing container id 10L
        Packing packing = new Packing();
        packing.setContainerId(10L);
        packing.setPacks("5");
        packing.setPacksType("BOX");
        packing.setHSCode("HS123");
        packing.setGoodsDescription("Goods");
        when(packingV3Service.getPackingsByConsolidationId(200L)).thenReturn(List.of(packing));

        // Capture save argument
        ArgumentCaptor<ShippingInstruction> captor = ArgumentCaptor.forClass(ShippingInstruction.class);
        when(repository.save(captor.capture())).thenAnswer(inv -> inv.getArgument(0));
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(new ShippingInstructionResponse());

        // Act
        ShippingInstructionResponse out = service.createShippingInstruction(request);

        // Assert
        ShippingInstruction savedArg = captor.getValue();
        assertNotNull(savedArg);
        // commonPackages should be created from packing list and populated with container number from containersMap
        assertNotNull(savedArg.getCommonPackagesList());
        assertEquals(1, savedArg.getCommonPackagesList().size());
        assertEquals("C-10", savedArg.getCommonPackagesList().get(0).getContainerNo());

        // commonContainers should be created from consolidation containers list
        assertNotNull(savedArg.getCommonContainersList());
        assertEquals(1, savedArg.getCommonContainersList().size());
        assertEquals("C-10", savedArg.getCommonContainersList().get(0).getContainerNo());
    }

    @Test
    void createShippingInstruction_shouldThrow_whenCarrierBookingMissing() {
        ShippingInstructionRequest request = buildSimpleRequest();
        ShippingInstruction entity = buildSimpleEntity();
        // request -> entity conversion
        when(jsonHelper.convertValue(eq(request), eq(ShippingInstruction.class))).thenReturn(entity);

        // carrierBookingDao returns empty -> setDefaultValues will throw Invalid entity id
        when(carrierBookingDao.findById(anyLong())).thenReturn(Optional.empty());

        assertThatThrownBy(() -> service.createShippingInstruction(request))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid entity id");

        verify(repository, never()).save(any(ShippingInstruction.class));
    }

    @Test
    void getDefaultShippingInstructionValues_forConsolidation_setsBookingStatusFromConsolidation() {
        Long entityId = 200L;
        ConsolidationDetails consol = new ConsolidationDetails();
        consol.setId(entityId);
        consol.setBookingStatus("BOOKING-OK");

        when(consolidationDetailsDao.findById(entityId)).thenReturn(Optional.of(consol));

        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenAnswer(inv -> {
                    ShippingInstruction arg = inv.getArgument(0);
                    ShippingInstructionResponse r = new ShippingInstructionResponse();
                    if (arg != null && arg.getStatus() != null) r.setStatus(arg.getStatus().name());
                    return r;
                });

        ShippingInstructionResponse response = service.getDefaultShippingInstructionValues(EntityType.CONSOLIDATION, entityId);

        assertNotNull(response);
        assertThat(response.getBookingStatus()).isEqualTo("BOOKING-OK");
        assertThat(response.getStatus()).isEqualTo(ShippingInstructionStatus.Draft.name());
    }

    @Test
    void getShippingInstructionsById_whenEntityTypeCarrierBooking_andProjectionPresent_setsBookingStatus() {
        Long id = 1L;
        ShippingInstruction si = new ShippingInstruction();
        si.setId(id);
        si.setEntityType(EntityType.CARRIER_BOOKING);
        si.setEntityId(100L);

        when(repository.findById(id)).thenReturn(Optional.of(si));

        // create a simple projection mock/stub with booking status
        CarrierBookingInfoProjection proj = mock(CarrierBookingInfoProjection.class);
        when(proj.getBookingStatus()).thenReturn("PROJ-STATUS");
        when(repository.findBookingInfoById(100L)).thenReturn(proj);

        // json conversion just returns an empty response
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(new ShippingInstructionResponse());

        ShippingInstructionResponse out = service.getShippingInstructionsById(id);

        assertNotNull(out);
        assertThat(out.getBookingStatus()).isEqualTo("PROJ-STATUS");
    }

    @Test
    void submitShippingInstruction_shouldThrow_whenCarrierBookingStatusNotAllowed() {
        Long id = 10L;
        ShippingInstruction si = new ShippingInstruction();
        si.setId(id);
        si.setEntityType(EntityType.CARRIER_BOOKING);
        si.setEntityId(100L);
        si.setStatus(ShippingInstructionStatus.Draft);

        when(repository.findById(id)).thenReturn(Optional.of(si));

        CarrierBooking booking = new CarrierBooking();
        booking.setId(100L);
        booking.setStatus(CarrierBookingStatus.Draft); // not allowed

        when(carrierBookingDao.findById(100L)).thenReturn(Optional.of(booking));

        assertThatThrownBy(() -> service.submitShippingInstruction(id))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Submit not allowed. Carrier Booking is not Confirmed/Conditionally Accepted.");

        // ensure save is not called
        verify(repository, never()).save(any());
        verify(kafkaHelper, never()).sendDataToKafka(anyString(), any(), any());
    }

    @Test
    void amendShippingInstruction_shouldThrow_whenShippingInstructionMissing() {
        Long id = 999L;
        when(repository.findById(id)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> service.amendShippingInstruction(id))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid shipping instruction id");
    }

    @Test
    void populateFreightDetails_doesNothing_whenConsolNullOrPaymentNull() {
        ShippingInstruction si = new ShippingInstruction();
        si.setId(1L);
        si.setFreightDetailList(null);

        // case 1: consol null
        service.populateFreightDetails(si, null);
        assertNull(si.getFreightDetailList());

        // case 2: consol present but payment null
        ConsolidationDetails consol = new ConsolidationDetails();
        consol.setPayment(null);
        service.populateFreightDetails(si, consol);
        assertNull(si.getFreightDetailList());
    }

    @Test
    void populateFreightDetails_collect_setsPayerLocationFromCarrierDetails() {
        ShippingInstruction si = new ShippingInstruction();
        si.setId(2L);
        si.setFreightDetailList(null);

        ConsolidationDetails consol = new ConsolidationDetails();
        consol.setPayment("CCX");
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setDestinationPortCountry("COUNTRY_X");
        consol.setCarrierDetails(carrierDetails);

        TenantModel tenantModel = new TenantModel();
        V1RetrieveResponse mockTenantResponse = new V1RetrieveResponse();
        mockTenantResponse.setEntity(tenantModel);
        when(v1Service.retrieveTenant()).thenReturn(mockTenantResponse);
        // --- Act ---
        service.populateFreightDetails(si, consol);

        // --- Assert ---
        assertNotNull(si.getFreightDetailList());
        assertThat(si.getFreightDetailList()).hasSize(1);

        FreightDetail fd = si.getFreightDetailList().get(0);
        assertThat(fd.getPaymentTerms()).isEqualTo("Collect");
        assertThat(fd.getPayerType()).isEqualTo(PayerType.CONSIGNEE);
        // In Collect branch → payerLocation should come from carrierDetails
        assertThat(fd.getPayerLocation()).isEqualTo("COUNTRY_X");
    }


    @Test
    void populateReadOnlyFields_shouldThrow_whenCarrierBookingNotFound() {
        ShippingInstructionRequest req = buildSimpleRequest();
        ShippingInstruction si = buildSimpleEntity();

        ShippingInstructionResponseMapper mapper = new ShippingInstructionResponseMapper();
        si.setEntityType(EntityType.CARRIER_BOOKING);
        si.setEntityId(123L);
        mapper.setShippingInstruction(si);

        when(carrierBookingDao.findById(123L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> {
            // call private method via public wrapper: use the public update path which calls populateReadOnlyFields
            ShippingInstructionRequest updateReq = new ShippingInstructionRequest();
            updateReq.setId(1L);
            // need repository to return existing SI for update flow
            when(repository.findById(1L)).thenReturn(Optional.of(si));
            when(jsonHelper.convertValue(eq(updateReq), eq(ShippingInstruction.class))).thenReturn(si);
            // now call updateShippingInstructions which invokes populateReadOnlyFields internally
            service.updateShippingInstructions(updateReq);
        }).isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid entity id");
    }


    @Test
    void compareContainerDetails_detectsPackChange() {
        CommonContainers oldC = buildContainer("C1", 10, "BOX", new BigDecimal("100"), "KG");
        CommonContainers newC = buildContainer("C1", 15, "BOX", new BigDecimal("100"), "KG");

        List<ShippingInstructionContainerWarningResponse> warnings =
                shipmentInstructionUtil.compareContainerDetails(List.of(oldC), List.of(newC));

        assertThat(warnings).hasSize(1);
        ShippingInstructionContainerWarningResponse resp = warnings.get(0);
        assertThat(resp.getContainerNumber()).isEqualTo("C1");
        assertThat(resp.getPackagePrev()).contains("10 BOX");
        assertThat(resp.getPackagePost()).contains("15 BOX");
    }

    @Test
    void compareContainerDetails_detectsWeightChange() {
        CommonContainers oldC = buildContainer("C1", 10, "BOX", new BigDecimal("100"), "KG");
        CommonContainers newC = buildContainer("C1", 10, "BOX", new BigDecimal("120"), "KG");

        List<ShippingInstructionContainerWarningResponse> warnings =
                shipmentInstructionUtil.compareContainerDetails(List.of(oldC), List.of(newC));

        assertThat(warnings).hasSize(1);
        assertThat(warnings.get(0).getWeightPrevious()).contains("100 KG");
        assertThat(warnings.get(0).getWeightPost()).contains("120 KG");
    }

    @Test
    void compareContainerDetails_noChange_returnsEmpty() {
        CommonContainers oldC = buildContainer("C1", 10, "BOX", new BigDecimal("100"), "KG");
        CommonContainers newC = buildContainer("C1", 10, "BOX", new BigDecimal("100"), "KG");

        List<ShippingInstructionContainerWarningResponse> warnings =
                shipmentInstructionUtil.compareContainerDetails(List.of(oldC), List.of(newC));

        assertThat(warnings).isEmpty();
    }

    @Test
    void comparePackageDetails_detectsPackChange() {
        CommonPackages oldP = buildPackage("P1", 5, "CRATE", new BigDecimal("50"), "KG");
        CommonPackages newP = buildPackage("P1", 7, "CRATE", new BigDecimal("50"), "KG");

        List<ShippingInstructionContainerWarningResponse> warnings =
                shipmentInstructionUtil.comparePackageDetails(List.of(oldP), List.of(newP));

        assertThat(warnings).hasSize(1);
        ShippingInstructionContainerWarningResponse resp = warnings.get(0);
        assertThat(resp.getContainerNumber()).isEqualTo("P1");
        assertThat(resp.getPackagePrev()).contains("5 CRATE");
        assertThat(resp.getPackagePost()).contains("7 CRATE");
    }

    @Test
    void comparePackageDetails_detectsWeightChange() {
        CommonPackages oldP = buildPackage("P1", 5, "CRATE", new BigDecimal("50"), "KG");
        CommonPackages newP = buildPackage("P1", 5, "CRATE", new BigDecimal("55"), "KG");

        List<ShippingInstructionContainerWarningResponse> warnings =
                shipmentInstructionUtil.comparePackageDetails(List.of(oldP), List.of(newP));

        assertThat(warnings).hasSize(1);
        assertThat(warnings.get(0).getWeightPrevious()).contains("50 KG");
        assertThat(warnings.get(0).getWeightPost()).contains("55 KG");
    }

    @Test
    void comparePackageDetails_noChange_returnsEmpty() {
        CommonPackages oldP = buildPackage("P1", 5, "CRATE", new BigDecimal("50"), "KG");
        CommonPackages newP = buildPackage("P1", 5, "CRATE", new BigDecimal("50"), "KG");

        List<ShippingInstructionContainerWarningResponse> warnings =
                shipmentInstructionUtil.comparePackageDetails(List.of(oldP), List.of(newP));

        assertThat(warnings).isEmpty();
    }

    @Test
    void testSubmitShippingInstruction_ForCarrierBooking_ShouldSendEmail() throws JsonProcessingException {
        // Arrange
        ShippingInstruction si = new ShippingInstruction();
        si.setId(100L);
        si.setEntityType(EntityType.CARRIER_BOOKING);
        si.setEntityId(200L);
        si.setInternalEmails("ops@company.com");
        si.setCreateByUserEmail("creator@test.com");
        si.setSubmitByUserEmail("submitter@test.com");

        CarrierBooking booking = new CarrierBooking();
        booking.setId(200L);
        booking.setStatus(CarrierBookingStatus.ConfirmedByCarrier);

        when(repository.findById(100L)).thenReturn(Optional.of(si));
        when(repository.save(any())).thenAnswer(inv -> inv.getArgument(0));
        when(carrierBookingDao.findById(200L)).thenReturn(Optional.of(booking));

        EmailTemplatesRequest template = new EmailTemplatesRequest();
        template.setType(Constants.SHIPPING_INSTRUCTION_EMAIL_TEMPLATE);
        template.setName("SI_Template");
        template.setSubject("SI Notification");
        template.setBody("<html>Test</html>");

        ShippingInstructionsServiceImpl spyService = Mockito.spy(service);
        doReturn(List.of(template)).when(spyService).getShippingInstructionTemplate(any());

        // Act
        spyService.submitShippingInstruction(100L);

        // Assert
        verify(notificationService, times(1)).sendEmail(any(SendEmailBaseRequest.class));
    }

    @Test
    void testSendNotification_OnAmend_ShouldSendEmail() throws JsonProcessingException {
        // Arrange
        ShippingInstruction si = new ShippingInstruction();
        si.setId(101L);
        si.setEntityType(EntityType.CARRIER_BOOKING);
        si.setEntityId(200L);
        si.setStatus(ShippingInstructionStatus.SISubmitted);
        si.setInternalEmails("docs@company.com");
        si.setCreateByUserEmail("creator@test.com");
        si.setSubmitByUserEmail("creator@test.com");

        // Mock only essential dependencies
        when(repository.findById(101L)).thenReturn(Optional.of(si));
        when(repository.save(any())).thenAnswer(inv -> inv.getArgument(0));

        // Mock the V1 service response - this is CRITICAL
        EmailTemplatesRequest template = new EmailTemplatesRequest();
        template.setType(Constants.SHIPPING_INSTRUCTION_EMAIL_TEMPLATE);
        template.setName("SI_Template");
        template.setSubject("Amend Notification");
        template.setBody("<html>Amend Test</html>");

        V1DataResponse v1Response = new V1DataResponse();
        v1Response.entities = List.of(template);

        when(v1Service.getEmailTemplates(any(CommonV1ListRequest.class))).thenReturn(v1Response);

        // Mock JSON conversions
        when(jsonHelper.convertValueToList(any(), eq(EmailTemplatesRequest.class)))
                .thenReturn(List.of(template));
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(new ShippingInstructionResponse());
        when(jsonHelper.convertToJson(any(ContainerPackageSiPayload.class)))
                .thenReturn("{}");
        when(jsonHelper.convertToJson(any(ShippingInstruction.class)))
                .thenReturn("{}");

        // Act
        service.amendShippingInstruction(101L);

        // Assert
        verify(notificationService, times(1)).sendEmail(any(SendEmailBaseRequest.class));
    }

}
