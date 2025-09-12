package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.controller.ShippingInstructionsController;
import com.dpw.runner.shipment.services.dao.impl.ShippingInstructionDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SailingInformationRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.ShippingInstructionRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ShippingInstructionMasterDataHelper;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.IntraCommonKafkaHelper;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.Assert.assertNotNull;
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
    private ShippingInstructionMasterDataHelper shippingInstructionMasterDataHelper;
    @InjectMocks
    private ShippingInstructionsController controller;
    private static ShippingInstruction testSI;
    private static ObjectMapper objectMapper;
    @Mock
    private IPackingV3Service packingV3Service;

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
}
