package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.interfaces.IBridgeServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShippingInstructionsConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.ShippingInstructionDao;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerPackageSiPayload;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShippingInstructionContainerWarningResponse;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SailingInformationRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.ShippingInstructionRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.SailingInformationResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionInttraRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.exception.exceptions.GenericException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ShippingInstructionMasterDataHelper;
import com.dpw.runner.shipment.services.kafka.dto.inttra.ShippingInstructionEventDto;
import com.dpw.runner.shipment.services.notification.request.SendEmailBaseRequest;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.projection.CarrierBookingInfoProjection;
import com.dpw.runner.shipment.services.projection.ShippingConsoleIdProjection;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.IntraCommonKafkaHelper;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingInttraUtil;
import com.dpw.runner.shipment.services.utils.v3.ShippingInstructionUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.*;
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
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import static com.dpw.runner.shipment.services.commons.constants.ShippingInstructionsConstants.SHIPPING_INSTRUCTION_ADDITIONAL_PARTIES;
import static com.dpw.runner.shipment.services.commons.constants.ShippingInstructionsConstants.SHIPPING_INSTRUCTION_EMAIL_TEMPLATE;
import static com.dpw.runner.shipment.services.entity.enums.ShippingInstructionStatus.Requested;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
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
    private IntraCommonKafkaHelper kafkaHelper;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private ExecutorService executorServiceMasterData;
    @Mock
    private CommonUtils commonUtils;
    @Mock
    private IV1Service v1Service;
    @Mock
    private ShippingInstructionMasterDataHelper shippingInstructionMasterDataHelper;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private IPackingV3Service packingV3Service;
    @Mock
    private CarrierBookingInttraUtil carrierBookingInttraUtil;
    @Mock
    private ShippingInstructionUtil siUtil;
    @Mock
    private ICommonContainersDao commonContainersDao;
    @Mock
    private IShippingInstructionDao shippingInstructionDao;
    @Mock
    private ICommonPackagesDao commonPackagesDao;
    @Mock
    private IPartiesDao partiesDao;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private IBridgeServiceAdapter bridgeServiceAdapter;
    @Mock
    private IVerifiedGrossMassDao vgmDao;
    @Mock
    private INotificationService notificationService;
    @InjectMocks
    private ShippingInstructionUtil shippingInstructionUtil;
    @Mock
    private IContainerDao containerDao;

    private static JsonTestUtility jsonTestUtility;
    private static ShippingInstruction testSI;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
    }

    @BeforeEach
    void setUp() {
        testSI = jsonTestUtility.getTestShippingInstruction();
        service.executorServiceMasterData = Executors.newFixedThreadPool(2);
    }

    // ========== HELPER METHODS ==========

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

    private ShippingInstruction buildEntityWithInttraParty(ShippingInstructionStatus status, EntityType entityType, Long entityId) {
        ShippingInstruction si = new ShippingInstruction();
        si.setStatus(status);
        si.setEntityType(entityType);
        si.setEntityId(entityId);

        // Setup INTTRA party
        Parties requestor = new Parties();
        Map<String, Object> requestorOrgData = new HashMap<>();
        requestorOrgData.put("RemoteIdType", "INTRA_COMPANY_ID");
        requestorOrgData.put("RemoteId", "2342324");
        requestor.setOrgData(requestorOrgData);
        si.setRequestor(requestor);

        return si;
    }

    private void setupInttraSubmitMocks() throws RunnerException {
        when(carrierBookingInttraUtil.getInttraRemoteId(any())).thenReturn("2342324");
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionInttraRequest.class)))
                .thenReturn(new ShippingInstructionInttraRequest());

        // Mock void methods
        doNothing().when(siUtil).populateInttraSpecificData(any(), any());
        doNothing().when(siUtil).populateCarrierDetails(any(), any());
        when(carrierBookingInttraUtil.fetchCarrierDetailsForBridgePayload(any())).thenReturn(null);
        doNothing().when(carrierBookingInttraUtil).createTransactionHistory(any(), any(), any(), any(), any(), any());
    }

    private ConsolidationDetails buildConsolidationDetails(String number, String bookingNumber) {
        ConsolidationDetails consol = new ConsolidationDetails();
        consol.setConsolidationNumber(number);
        consol.setBookingNumber(bookingNumber);
        consol.setBookingStatus("Confirmed");
        return consol;
    }

    private CarrierBooking buildCarrierBooking(Long id, CarrierBookingStatus status) {
        CarrierBooking cb = new CarrierBooking();
        cb.setId(id);
        cb.setStatus(status);
        cb.setCarrierBookingNo("CB-001");
        cb.setCarrierBlNo("BL-001");
        return cb;
    }

    // ========== CREATE TESTS ==========

    @Test
    void createShippingInstruction_ShouldValidate_Save_AndReturnResponse() {
        ShippingInstructionRequest request = buildSimpleRequest();
        ShippingInstruction entity = buildSimpleEntity();
        CarrierBooking cb = buildCarrierBooking(100L, CarrierBookingStatus.Draft);
        cb.setEntityId(200L);
        ConsolidationDetails consol = buildConsolidationDetails("CON-001", "CB-001");
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEmail("test@gmail.com");
        UserContext.setUser(mockUser);
        when(jsonHelper.convertValue(request, ShippingInstruction.class)).thenReturn(entity);
        when(carrierBookingDao.findById(100L)).thenReturn(Optional.of(cb));
        when(carrierBookingInttraUtil.getConsolidationDetail(200L)).thenReturn(consol);
        when(repository.save(any(ShippingInstruction.class))).thenAnswer(inv -> inv.getArgument(0));
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(ShippingInstructionResponse.builder()
                        .status(ShippingInstructionStatus.Draft.name())
                        .carrierBookingNo("CB-001")
                        .build());

        ShippingInstructionResponse out = service.createShippingInstruction(request);

        assertThat(out).isNotNull();
        assertThat(out.getStatus()).isEqualTo(ShippingInstructionStatus.Draft.name());
        verify(repository).save(any(ShippingInstruction.class));
    }

    @Test
    void createShippingInstruction_shouldThrow_whenCarrierBookingMissing() {
        ShippingInstructionRequest request = buildSimpleRequest();
        when(jsonHelper.convertValue(request, ShippingInstruction.class)).thenReturn(buildSimpleEntity());
        when(carrierBookingDao.findById(anyLong())).thenReturn(Optional.empty());

        assertThatThrownBy(() -> service.createShippingInstruction(request))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid entity id");
        verify(repository, never()).save(any(ShippingInstruction.class));
    }

    // ========== VALIDATION TESTS ==========

    @ParameterizedTest
    @CsvSource({
            "101, 'Invalid freight copies number!'",
            "-1, 'Invalid freight copies number!'"
    })
    void create_ShouldThrow_WhenNoOfFreightCopiesInvalid(int copies, String expectedMessage) {
        ShippingInstructionRequest req = ShippingInstructionRequest.builder().build();
        ShippingInstruction bad = mock(ShippingInstruction.class, RETURNS_DEEP_STUBS);
        when(bad.getEntityType()).thenReturn(EntityType.CARRIER_BOOKING);
        when(bad.getNoOfFreightCopies()).thenReturn(copies);
        when(bad.getSailingInformation()).thenReturn(new SailingInformation());
        when(jsonHelper.convertValue(req, ShippingInstruction.class)).thenReturn(bad);

        assertThatThrownBy(() -> service.createShippingInstruction(req))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining(expectedMessage);
        verifyNoInteractions(repository);
    }

    @Test
    void createShippingInstruction_ShouldThrow_WhenFreightDetailMissingPayerLocation() {
        ShippingInstructionRequest req = ShippingInstructionRequest.builder().build();
        ShippingInstruction si = new ShippingInstruction();
        si.setEntityType(EntityType.CONSOLIDATION);
        si.setEntityId(123L);

        FreightDetail badFreight = new FreightDetail();
        badFreight.setPayerLocation(null); // MISSING mandatory field
        si.setFreightDetailList(List.of(badFreight));

        when(jsonHelper.convertValue(req, ShippingInstruction.class)).thenReturn(si);

        assertThatThrownBy(() -> service.createShippingInstruction(req))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Payment Location (payerLocation) is mandatory");
        verifyNoInteractions(repository);
    }

    // ========== SUBMIT TESTS ==========

    @Test
    void submitShippingInstruction_success_whenCarrierBookingConfirmed() throws RunnerException {
        Long id = 1L;
        ShippingInstruction si = buildEntityWithInttraParty(ShippingInstructionStatus.Draft, EntityType.CARRIER_BOOKING, 100L);
        si.setId(id);
        CarrierBooking booking = buildCarrierBooking(100L, CarrierBookingStatus.ConfirmedByCarrier);
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEmail("test@gmail.com");
        UserContext.setUser(mockUser);
        when(repository.findById(id)).thenReturn(Optional.of(si));
        when(carrierBookingDao.findById(100L)).thenReturn(Optional.of(booking));
        when(repository.save(any(ShippingInstruction.class))).thenAnswer(inv -> inv.getArgument(0));
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(new ShippingInstructionResponse());

        setupInttraSubmitMocks();

        ShippingInstructionResponse resp = service.submitShippingInstruction(id);

        assertNotNull(resp);
        verify(repository).save(argThat(s -> s.getStatus() == Requested));
    }

    @Test
    void submitShippingInstruction_shouldThrow_whenCarrierBookingNotConfirmed() {
        Long id = 2L;
        ShippingInstruction si = buildEntityWithInttraParty(ShippingInstructionStatus.Draft, EntityType.CARRIER_BOOKING, 101L);
        si.setId(id);
        CarrierBooking booking = buildCarrierBooking(101L, CarrierBookingStatus.Draft);

        when(repository.findById(id)).thenReturn(Optional.of(si));
        when(carrierBookingDao.findById(101L)).thenReturn(Optional.of(booking));
        when(carrierBookingInttraUtil.getInttraRemoteId(any())).thenReturn("2342324");

        assertThatThrownBy(() -> service.submitShippingInstruction(id))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Submit not allowed. Carrier Booking is not Confirmed/Conditionally Accepted.");
        verify(repository, never()).save(any());
    }

    @Test
    void submitShippingInstruction_success_whenConsolidationAndDraft() throws RunnerException {
        Long id = 3L;
        ShippingInstruction si = buildEntityWithInttraParty(ShippingInstructionStatus.Draft, EntityType.CONSOLIDATION, 200L);
        si.setId(id);
        ConsolidationDetails consolidationDetails = buildConsolidationDetails("CONSOL123", "BOOKING123");
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEmail("test@gmail.com");
        UserContext.setUser(mockUser);
        when(repository.findById(id)).thenReturn(Optional.of(si));
        when(repository.save(any(ShippingInstruction.class))).thenAnswer(inv -> inv.getArgument(0));
        when(carrierBookingInttraUtil.getConsolidationDetail(200L)).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(new ShippingInstructionResponse());

        setupInttraSubmitMocks();

        ShippingInstructionResponse resp = service.submitShippingInstruction(id);

        assertNotNull(resp);
        verify(repository).save(argThat(s -> s.getStatus() == Requested));
    }

    @Test
    void submitShippingInstruction_shouldThrow_whenNotFound() {
        when(repository.findById(123L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> service.submitShippingInstruction(123L))
                .isInstanceOf(GenericException.class)
                .hasMessageContaining("Shipping Instruction not found");
    }

    // ========== AMEND TESTS ==========

    @Test
    void amendShippingInstruction_success_changesStatusAndSendsDownstream() throws RunnerException {
        Long id = 5L;
        ShippingInstruction si = buildEntityWithInttraParty(Requested, EntityType.CONSOLIDATION, 100L);
        si.setId(id);
        ConsolidationDetails consolidationDetails = buildConsolidationDetails("CONSOL123", "BOOKING123");
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEmail("test@gmail.com");
        UserContext.setUser(mockUser);
        when(repository.findById(id)).thenReturn(Optional.of(si));
        when(repository.save(any(ShippingInstruction.class))).thenAnswer(inv -> inv.getArgument(0));
        when(carrierBookingInttraUtil.getConsolidationDetail(100L)).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(new ShippingInstructionResponse());

        setupInttraSubmitMocks();

        ShippingInstructionResponse resp = service.amendShippingInstruction(id);

        assertNotNull(resp);
        verify(repository).save(argThat(s -> s.getStatus() == ShippingInstructionStatus.Changed));
    }

    @Test
    void amendShippingInstruction_shouldThrow_whenNotSubmittedOrAccepted() {
        Long id = 6L;
        ShippingInstruction si = buildEntityWithInttraParty(ShippingInstructionStatus.Draft, EntityType.CARRIER_BOOKING, 100L);
        si.setId(id);
        CarrierBooking booking = buildCarrierBooking(100L, CarrierBookingStatus.ConfirmedByCarrier);

        when(repository.findById(id)).thenReturn(Optional.of(si));
        when(carrierBookingDao.findById(100L)).thenReturn(Optional.of(booking));
        when(carrierBookingInttraUtil.getInttraRemoteId(any())).thenReturn("2342324");

        assertThatThrownBy(() -> service.amendShippingInstruction(id))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Amendment not allowed. Shipping Instruction is not Submitted.");
    }

    @Test
    void amendShippingInstruction_shouldThrow_whenShippingInstructionMissing() {
        when(repository.findById(999L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> service.amendShippingInstruction(999L))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid shipping instruction id");
    }

    // ========== GET/READ TESTS ==========

    @Test
    void getShippingInstructionsById_ShouldReturnResponse_WhenFound() {
        ShippingInstruction entity = buildSimpleEntity();
        when(repository.findById(1L)).thenReturn(Optional.of(entity));
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(ShippingInstructionResponse.builder().carrierBookingNo("CB-001").build());
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setStatus(VerifiedGrossMassStatus.Draft);
        when(vgmDao.findByEntityIdType(any(EntityType.class), anyLong()))
                .thenReturn(vgm);


        ShippingInstructionResponse out = service.getShippingInstructionsById(1L);

        assertThat(out).isNotNull();
        assertThat(out.getCarrierBookingNo()).isEqualTo("CB-001");
        verify(repository).findById(1L);
    }

    @Test
    void getShippingInstructionsById_ShouldThrow_WhenNotFound() {
        when(repository.findById(999L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> service.getShippingInstructionsById(999L))
                .isInstanceOf(DataRetrievalFailureException.class)
                .hasMessageContaining(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
    }

    @Test
    void getDefaultShippingInstructionValues_populatesBookingStatus() {
        Long bookingId = 100L;
        Long consolidationId = 200L;
        CarrierBooking cb = buildCarrierBooking(bookingId, CarrierBookingStatus.ConfirmedByCarrier);
        cb.setEntityId(consolidationId);
        ConsolidationDetails consol = buildConsolidationDetails("CON-123", "CB-001");

        when(carrierBookingDao.findById(bookingId)).thenReturn(Optional.of(cb));
        when(carrierBookingInttraUtil.getConsolidationDetail(consolidationId)).thenReturn(consol);
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

    // ========== UPDATE/DELETE TESTS ==========

    @Test
    void updateShippingInstructions_ShouldValidate_Save_AndReturnResponse() {
        ShippingInstructionRequest request = buildSimpleRequest();
        ShippingInstruction entity = buildSimpleEntity();
        entity.setEntityType(EntityType.CONSOLIDATION);
        entity.setEntityId(999L);

        ConsolidationDetails consol = mock(ConsolidationDetails.class, RETURNS_DEEP_STUBS);
        when(consol.getCarrierDetails().getOrigin()).thenReturn("ORI");
        when(consol.getShipInstructionCutoff()).thenReturn(LocalDateTime.now());

        when(repository.findById(request.getId())).thenReturn(Optional.of(entity));
        when(jsonHelper.convertValue(request, ShippingInstruction.class)).thenReturn(entity);
        when(carrierBookingInttraUtil.getConsolidationDetail(999L)).thenReturn(consol);
        when(repository.save(any(ShippingInstruction.class))).thenReturn(entity);
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(new ShippingInstructionResponse());

        ShippingInstructionResponse out = service.updateShippingInstructions(request);

        assertThat(out).isNotNull();
        verify(repository).save(any(ShippingInstruction.class));
    }

    @Test
    void deleteShippingInstructions_ShouldInvokeDao() {
        service.deleteShippingInstructions(55L);
        verify(repository).delete(55L);
    }

    // ========== FREIGHT DETAILS TESTS ==========

    @Test
    void populateFreightDetails_doesNothing_whenConsolNullOrPaymentNull() {
        ShippingInstruction si = new ShippingInstruction();
        si.setFreightDetailList(null);

        service.populateFreightDetails(si, null);
        assertNull(si.getFreightDetailList());

        ConsolidationDetails consol = new ConsolidationDetails();
        consol.setPayment(null);
        service.populateFreightDetails(si, consol);
        assertNull(si.getFreightDetailList());
    }

    @Test
    void populateFreightDetails_collect_setsPayerLocationFromCarrierDetails() {
        ShippingInstruction si = new ShippingInstruction();
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

        service.populateFreightDetails(si, consol);

        assertNotNull(si.getFreightDetailList());
        assertThat(si.getFreightDetailList()).hasSize(1);
        FreightDetail fd = si.getFreightDetailList().get(0);
        assertThat(fd.getPaymentTerms()).isEqualTo("Collect");
        assertThat(fd.getPayerType()).isEqualTo(PayerType.CONSIGNEE);
        assertThat(fd.getPayerLocation()).isEqualTo("COUNTRY_X");
    }

    // ========== UTILITY COMPARISON TESTS ==========

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

    @Test
    void compareContainerDetails_detectsPackChange() {
        CommonContainers oldC = buildContainer("C1", 10, "BOX", new BigDecimal("100"), "KG");
        CommonContainers newC = buildContainer("C1", 15, "BOX", new BigDecimal("100"), "KG");

        List<ShippingInstructionContainerWarningResponse> warnings =
                shippingInstructionUtil.compareContainerDetails(List.of(oldC), List.of(newC));

        assertThat(warnings).hasSize(1);
        assertThat(warnings.get(0).getContainerNumber()).isEqualTo("C1");
    }

    @Test
    void comparePackageDetails_detectsPackChange() {
        CommonPackages oldP = buildPackage("P1", 5, "CRATE", new BigDecimal("50"), "KG");
        CommonPackages newP = buildPackage("P1", 7, "CRATE", new BigDecimal("50"), "KG");

        List<ShippingInstructionContainerWarningResponse> warnings =
                shippingInstructionUtil.comparePackageDetails(List.of(oldP), List.of(newP));

        assertThat(warnings).hasSize(1);
        assertThat(warnings.get(0).getContainerNumber()).isEqualTo("P1");
    }

    // ========== MASTER DATA TESTS ==========

    @Test
    void getAllMasterDataForShippingInstruction() {
        when(repository.findById(1L)).thenReturn(Optional.ofNullable(testSI));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });

        var res = service.getAllMasterData(1L);

        assertNotNull(res);
        assertEquals(HttpStatus.OK, res.getStatusCode());
    }

    @Test
    void listShippingInstruction() {
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        listCommonRequest.setIncludeColumns(List.of("id", "containersList"));
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(listCommonRequest);

        ShippingInstruction shippingInstruction = new ShippingInstruction();
        shippingInstruction.setId(1L);
        shippingInstruction.setCarrierBlNo("CX");

        when(repository.findAll(any(Specification.class), any(Pageable.class)))
                .thenReturn(new PageImpl<>(List.of(shippingInstruction)));
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(new ShippingInstructionResponse());
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setStatus(VerifiedGrossMassStatus.Draft);
        when(vgmDao.findByEntityIdType(any(), any())).thenReturn(vgm);


        ResponseEntity<IRunnerResponse> response = service.list(commonRequestModel, true);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(repository).findAll(any(Specification.class), any(Pageable.class));
    }

    // ========== MISSING VALIDATION TESTS ==========

    @Test
    void validateSIRequest_shouldThrow_whenNonNegoFreightCopiesInvalid_forExpressType() {
        ShippingInstructionRequest req = ShippingInstructionRequest.builder().build();
        ShippingInstruction bad = mock(ShippingInstruction.class, RETURNS_DEEP_STUBS);
        when(bad.getEntityType()).thenReturn(EntityType.CARRIER_BOOKING);
        when(bad.getNoOfFreightCopies()).thenReturn(1);
        when(bad.getNoOfUnFreightCopies()).thenReturn(1);
        when(bad.getNonNegoFreightCopies()).thenReturn(101); // invalid for EXPRESS
        when(bad.getShippingInstructionType()).thenReturn(ShippingInstructionType.EXPRESS);
        when(bad.getSailingInformation()).thenReturn(new SailingInformation());
        when(jsonHelper.convertValue(req, ShippingInstruction.class)).thenReturn(bad);

        assertThatThrownBy(() -> service.createShippingInstruction(req))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid getNonNegoFreightCopies!");
    }

    @Test
    void validateSIRequest_shouldThrow_whenNonNegoUnFreightCopiesInvalid_forExpressType() {
        ShippingInstructionRequest req = ShippingInstructionRequest.builder().build();
        ShippingInstruction bad = mock(ShippingInstruction.class, RETURNS_DEEP_STUBS);
        when(bad.getEntityType()).thenReturn(EntityType.CARRIER_BOOKING);
        when(bad.getNoOfFreightCopies()).thenReturn(1);
        when(bad.getNoOfUnFreightCopies()).thenReturn(1);
        when(bad.getNonNegoFreightCopies()).thenReturn(1);
        when(bad.getNonNegoUnFreightCopies()).thenReturn(-1); // invalid for EXPRESS
        when(bad.getShippingInstructionType()).thenReturn(ShippingInstructionType.EXPRESS);
        when(bad.getSailingInformation()).thenReturn(new SailingInformation());
        when(jsonHelper.convertValue(req, ShippingInstruction.class)).thenReturn(bad);

        assertThatThrownBy(() -> service.createShippingInstruction(req))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid getNonNegoUnFreightCopies!");
    }

    @Test
    void validateFetchAndSetSI_shouldThrow_whenConsolidationLinkedWithBooking() {
        ShippingInstructionRequest request = buildSimpleRequest();
        ShippingInstruction si = buildSimpleEntity();
        si.setEntityType(EntityType.CONSOLIDATION);
        si.setEntityId(123L);
        si.setEntityNumber("CONSOL123");

        CarrierBookingInfoProjection projection = mock(CarrierBookingInfoProjection.class);
        when(jsonHelper.convertValue(request, ShippingInstruction.class)).thenReturn(si);
        when(repository.findBookingByConsolId("CONSOL123")).thenReturn(List.of(projection));

        assertThatThrownBy(() -> service.createShippingInstruction(request))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("SI creation not allowed. Consolidation linked with a Booking already!!");
    }

// ========== MISSING FREIGHT DETAILS TESTS ==========

    @Test
    void populateFreightDetails_collect_usesReceivingAgentWhenAvailable() {
        ShippingInstruction si = new ShippingInstruction();
        si.setFreightDetailList(null);

        ConsolidationDetails consol = new ConsolidationDetails();
        consol.setPayment("CCX");

        Parties receivingAgent = new Parties();
        receivingAgent.setOrgCode("11001");
        consol.setReceivingAgent(receivingAgent);

        TenantModel tenantModel = new TenantModel();
        V1RetrieveResponse mockTenantResponse = new V1RetrieveResponse();
        mockTenantResponse.setEntity(tenantModel);
        when(v1Service.retrieveTenant()).thenReturn(mockTenantResponse);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);

        service.populateFreightDetails(si, consol);

        assertNotNull(si.getFreightDetailList());
        FreightDetail fd = si.getFreightDetailList().get(0);
        assertThat(fd.getPaymentTerms()).isEqualTo("Collect");
        assertThat(fd.getPayerType()).isEqualTo(PayerType.CONSIGNEE);
        assertThat(fd.getPayerLocation()).isEqualTo("11001");
    }

    @Test
    void populateFreightDetails_skipsWhenFreightDetailsAlreadyPresent() {
        ShippingInstruction si = new ShippingInstruction();
        FreightDetail existingDetail = new FreightDetail();
        si.setFreightDetailList(List.of(existingDetail));

        ConsolidationDetails consol = new ConsolidationDetails();
        consol.setPayment("PPD");

        service.populateFreightDetails(si, consol);

        // Should not modify existing freight details
        assertThat(si.getFreightDetailList()).hasSize(1);
        assertThat(si.getFreightDetailList().get(0)).isEqualTo(existingDetail);
    }

    @Test
    void populateFreightDetails_returnsEarly_forUnsupportedPaymentTerms() {
        ShippingInstruction si = new ShippingInstruction();
        si.setFreightDetailList(null);

        ConsolidationDetails consol = new ConsolidationDetails();
        consol.setPayment("UNSUPPORTED_PAYMENT_TERM");

        TenantModel tenantModel = new TenantModel();
        V1RetrieveResponse mockTenantResponse = new V1RetrieveResponse();
        mockTenantResponse.setEntity(tenantModel);
        when(v1Service.retrieveTenant()).thenReturn(mockTenantResponse);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);

        service.populateFreightDetails(si, consol);

        // Should not add any freight details for unsupported payment terms
        assertNull(si.getFreightDetailList());
    }

// ========== MISSING REFERENCE NUMBER TESTS ==========


    @Test
    void setReferenceNumber_mergesWithExistingReferences() {
        ShippingInstruction si = new ShippingInstruction();
        ReferenceNumbers existingRef = new ReferenceNumbers();
        existingRef.setType("EXISTING");
        existingRef.setReferenceNumber("REF-001");
        si.setReferenceNumbers(List.of(existingRef));

        CarrierBooking cb = new CarrierBooking();
        ReferenceNumbers newRef = new ReferenceNumbers();
        newRef.setType("NEW");
        newRef.setReferenceNumber("REF-002");
        cb.setReferenceNumbersList(List.of(newRef));

        assertDoesNotThrow(() -> {
            Method method = ShippingInstructionsServiceImpl.class.getDeclaredMethod("setReferenceNumber", ShippingInstruction.class, CarrierBooking.class);
            method.setAccessible(true);
            method.invoke(service, si, cb);
        });

        assertThat(si.getReferenceNumbers()).hasSize(1);
    }

// ========== MISSING PARTIES SETUP TESTS ==========

    @Test
    void setPartiesNumber_copiesAllPartiesFromCarrierBooking() {
        ShippingInstruction si = new ShippingInstruction();
        CarrierBooking cb = new CarrierBooking();

        // Setup all possible parties
        Parties consignee = new Parties();
        consignee.setId(1L);
        consignee.setGuid(UUID.randomUUID());
        Parties shipper = new Parties();
        shipper.setId(2L);
        shipper.setGuid(UUID.randomUUID());
        Parties forwardingAgent = new Parties();
        forwardingAgent.setId(3L);
        forwardingAgent.setGuid(UUID.randomUUID());
        Parties contract = new Parties();
        contract.setId(4L);
        contract.setGuid(UUID.randomUUID());
        Parties requester = new Parties();
        requester.setId(5L);
        requester.setGuid(UUID.randomUUID());

        cb.setConsignee(consignee);
        cb.setShipper(shipper);
        cb.setForwardingAgent(forwardingAgent);
        cb.setContract(contract);
        cb.setRequester(requester);

        assertDoesNotThrow(() -> {
            Method method = ShippingInstructionsServiceImpl.class.getDeclaredMethod("setPartiesNumber", ShippingInstruction.class, CarrierBooking.class);
            method.setAccessible(true);
            method.invoke(service, si, cb);
        });

        // Verify all parties are copied and IDs are nullified
        assertNotNull(si.getConsignee());
        assertNotNull(si.getShipper());
        assertNotNull(si.getForwardingAgent());
        assertNotNull(si.getContract());
        assertNotNull(si.getRequestor());

        assertNull(si.getConsignee().getId());
        assertNull(si.getShipper().getId());
        assertNull(si.getForwardingAgent().getId());
        assertNull(si.getContract().getId());
        assertNull(si.getRequestor().getId());
    }

// ========== MISSING GET BY ID TESTS ==========

    @Test
    void getShippingInstructionsById_handlesConsolidationType() {
        Long id = 1L;
        ShippingInstruction si = new ShippingInstruction();
        si.setId(id);
        si.setEntityType(EntityType.CONSOLIDATION);
        si.setEntityId(200L);

        ConsolidationDetails consol = buildConsolidationDetails("CONSOL123", "BOOKING123");
        when(repository.findById(id)).thenReturn(Optional.of(si));
        when(carrierBookingInttraUtil.getConsolidationDetail(200L)).thenReturn(consol);
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(new ShippingInstructionResponse());
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setStatus(VerifiedGrossMassStatus.Draft);
        when(vgmDao.findByEntityIdType(any(EntityType.class), anyLong()))
                .thenReturn(vgm);

        ShippingInstructionResponse result = service.getShippingInstructionsById(id);

        assertNotNull(result);
        verify(carrierBookingInttraUtil).getConsolidationDetail(200L);
    }

    @Test
    void getShippingInstructionsById_handlesPayloadJsonComparison() {
        Long id = 1L;
        ShippingInstruction si = buildSimpleEntity();
        si.setId(id);
        si.setPayloadJson("{\"containerDetail\":[],\"packageDetail\":[]}");

        ContainerPackageSiPayload payload = new ContainerPackageSiPayload();
        payload.setContainerDetail(List.of());
        payload.setPackageDetail(List.of());

        when(repository.findById(id)).thenReturn(Optional.of(si));
        when(jsonHelper.readFromJson(anyString(), eq(ContainerPackageSiPayload.class))).thenReturn(payload);
        when(siUtil.compareContainerDetails(any(), any())).thenReturn(List.of());
        when(siUtil.comparePackageDetails(any(), any())).thenReturn(List.of());
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(new ShippingInstructionResponse());
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setStatus(VerifiedGrossMassStatus.Draft);
        when(vgmDao.findByEntityIdType(any(EntityType.class), anyLong()))
                .thenReturn(vgm);

        ShippingInstructionResponse result = service.getShippingInstructionsById(id);

        assertNotNull(result);
        verify(siUtil).compareContainerDetails(any(), any());
        verify(siUtil).comparePackageDetails(any(), any());
    }

// ========== MISSING SUBMISSION VALIDATION TESTS ==========

    @Test
    void submitShippingInstruction_shouldThrow_whenConsolidationNotInDraft() {
        Long id = 4L;
        ShippingInstruction si = buildEntityWithInttraParty(ShippingInstructionStatus.AcceptedByCarrier, EntityType.CONSOLIDATION, 200L);
        si.setId(id);

        when(repository.findById(id)).thenReturn(Optional.of(si));
        when(carrierBookingInttraUtil.getInttraRemoteId(any())).thenReturn("2342324");

        assertThatThrownBy(() -> service.submitShippingInstruction(id))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Submit not allowed. Shipping Instruction not in draft state.");
    }

    @Test
    void validateSubmissionCriteria_shouldThrow_whenMultipleConfirmedBookings() {
        CarrierBooking booking = buildCarrierBooking(100L, CarrierBookingStatus.ConfirmedByCarrier);
        ShippingInstruction si = new ShippingInstruction();
        si.setEntityNumber("CONSOL123");

        // Mock multiple confirmed bookings
        CarrierBookingInfoProjection proj1 = mock(CarrierBookingInfoProjection.class);
        CarrierBookingInfoProjection proj2 = mock(CarrierBookingInfoProjection.class);
        when(repository.findConfimedBookingByConsolId("CONSOL123")).thenReturn(List.of(proj1, proj2));

        InvocationTargetException exception = assertThrows(InvocationTargetException.class, () -> {
            Method method = ShippingInstructionsServiceImpl.class.getDeclaredMethod("validateSubmissionCriteria", CarrierBooking.class, ShippingInstruction.class);
            method.setAccessible(true);
            method.invoke(service, booking, si);
        });

        // Unwrap the actual exception
        Throwable actualException = exception.getCause();
        assertThat(actualException).isInstanceOf(ValidationException.class);
        assertThat(actualException.getMessage()).contains("Only one booking of all booking linked with a consolidation can be in confirmed state!!");
    }

// ========== MISSING AMEND TESTS ==========

    @Test
    void amendShippingInstruction_success_forCarrierBookingType() throws RunnerException {
        Long id = 7L;
        ShippingInstruction si = buildEntityWithInttraParty(Requested, EntityType.CARRIER_BOOKING, 100L);
        si.setId(id);
        CarrierBooking booking = buildCarrierBooking(100L, CarrierBookingStatus.ConfirmedByCarrier);
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEmail("test@gmail.com");
        UserContext.setUser(mockUser);
        when(repository.findById(id)).thenReturn(Optional.of(si));
        when(carrierBookingDao.findById(100L)).thenReturn(Optional.of(booking));
        when(repository.save(any(ShippingInstruction.class))).thenAnswer(inv -> inv.getArgument(0));
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(new ShippingInstructionResponse());

        setupInttraSubmitMocks();

        ShippingInstructionResponse resp = service.amendShippingInstruction(id);

        assertNotNull(resp);
        verify(repository).save(argThat(s -> s.getStatus() == ShippingInstructionStatus.Changed));
        verify(bridgeServiceAdapter).bridgeApiIntegration(any(), eq("SI_AMEND"), any(), any());
    }

    @Test
    void amendShippingInstruction_handlesBridgeException() throws RunnerException {
        Long id = 8L;
        ShippingInstruction si = buildEntityWithInttraParty(Requested, EntityType.CONSOLIDATION, 100L);
        si.setId(id);
        ConsolidationDetails consolidationDetails = buildConsolidationDetails("CONSOL123", "BOOKING123");
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEmail("test@gmail.com");
        UserContext.setUser(mockUser);
        when(repository.findById(id)).thenReturn(Optional.of(si));
        when(carrierBookingInttraUtil.getConsolidationDetail(100L)).thenReturn(consolidationDetails);
        when(repository.save(any(ShippingInstruction.class))).thenAnswer(inv -> inv.getArgument(0));
        when(carrierBookingInttraUtil.getInttraRemoteId(any())).thenReturn("2342324");
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionInttraRequest.class)))
                .thenReturn(new ShippingInstructionInttraRequest());
        // Mock void methods
        doNothing().when(siUtil).populateInttraSpecificData(any(), any());
        doNothing().when(siUtil).populateCarrierDetails(any(), any());
        when(carrierBookingInttraUtil.fetchCarrierDetailsForBridgePayload(any())).thenReturn(null);

        // Bridge throws exception
        RuntimeException bridgeException = new RuntimeException("Bridge failed");
        doThrow(bridgeException).when(bridgeServiceAdapter).bridgeApiIntegration(any(), any(), any(), any());
        doNothing().when(carrierBookingInttraUtil).createTransactionHistory(any(), any(), any(), any(), any(), any());

        assertThatThrownBy(() -> service.amendShippingInstruction(id))
                .isInstanceOf(RuntimeException.class)
                .hasMessageContaining("Bridge failed");

        // Verify error transaction history is created
        verify(carrierBookingInttraUtil).createTransactionHistory(
                any(), any(), eq("Bridge failed"), any(), eq(id), any());
    }


// ========== MISSING MASTER DATA TESTS ==========

    @Test
    void getAllMasterData_handlesMasterDataException() {
        Long id = 1L;
        when(repository.findById(id)).thenReturn(Optional.of(testSI));
        when(commonUtils.setIncludedFieldsToResponse(any(), any(), any()))
                .thenThrow(new RuntimeException("Master data error"));

        ResponseEntity<IRunnerResponse> result = service.getAllMasterData(id);

        assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
    }

    // ========== MISSING AMEND VALIDATION TESTS ==========
    @Test
    void amendShippingInstruction_shouldThrow_whenBookingNotConfirmedForNonStandalone() {
        Long id = 8L;
        ShippingInstruction si = buildEntityWithInttraParty(Requested, EntityType.CARRIER_BOOKING, 100L);
        si.setId(id);
        CarrierBooking booking = buildCarrierBooking(100L, CarrierBookingStatus.Draft); // Not confirmed

        when(repository.findById(id)).thenReturn(Optional.of(si));
        when(carrierBookingDao.findById(100L)).thenReturn(Optional.of(booking));
        when(carrierBookingInttraUtil.getInttraRemoteId(any())).thenReturn("2342324");

        // Mock the conversions properly
        ShippingInstructionInttraRequest inttraRequest = new ShippingInstructionInttraRequest();
        inttraRequest.setSailingInformation(new SailingInformationResponse()); // Add this to avoid NPE in populateCarrierDetails

        // This will trigger the checkIfAllowed method internally
        assertThatThrownBy(() -> service.amendShippingInstruction(id))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Amendment not allowed. Carrier booking is not submitted.");
    }

    @Test
    void checkIfAllowed_success_whenBookingConfirmedForNonStandalone() {
        ShippingInstruction si = new ShippingInstruction();
        si.setStatus(Requested);
        CarrierBooking booking = buildCarrierBooking(100L, CarrierBookingStatus.ConfirmedByCarrier);

        assertDoesNotThrow(() -> {
            Method method = ShippingInstructionsServiceImpl.class.getDeclaredMethod("checkIfAllowed", String.class, ShippingInstruction.class, CarrierBooking.class, boolean.class);
            method.setAccessible(true);
            method.invoke(null, "2342324", si, booking, false); // isStandAlone = false
        });
    }

// ========== MISSING ADDITIONAL PARTIES TESTS ==========

    @Test
    void createShippingInstruction_handlesAdditionalParties() {
        ShippingInstructionRequest request = buildSimpleRequest();
        ShippingInstruction entity = buildSimpleEntity();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEmail("test@gmail.com");
        UserContext.setUser(mockUser);
        Parties additionalParty = new Parties();
        additionalParty.setOrgCode("ADDITIONAL_ORG");
        entity.setAdditionalParties(List.of(additionalParty));

        CarrierBooking cb = buildCarrierBooking(100L, CarrierBookingStatus.Draft);
        cb.setEntityId(200L);
        ConsolidationDetails consol = buildConsolidationDetails("CON-001", "CB-001");

        when(jsonHelper.convertValue(request, ShippingInstruction.class)).thenReturn(entity);
        when(carrierBookingDao.findById(100L)).thenReturn(Optional.of(cb));
        when(carrierBookingInttraUtil.getConsolidationDetail(200L)).thenReturn(consol);
        when(repository.save(any(ShippingInstruction.class))).thenAnswer(inv -> inv.getArgument(0));
        when(commonUtils.convertToEntityList(any(), eq(Parties.class), eq(false))).thenReturn(List.of(additionalParty));
        when(partiesDao.saveEntityFromOtherEntity(any(), any(), any())).thenReturn(List.of(additionalParty));
        when(jsonHelper.convertValue(any(ShippingInstruction.class), eq(ShippingInstructionResponse.class)))
                .thenReturn(new ShippingInstructionResponse());

        ShippingInstructionResponse result = service.createShippingInstruction(request);

        assertNotNull(result);
        verify(partiesDao).saveEntityFromOtherEntity(any(), any(), eq(SHIPPING_INSTRUCTION_ADDITIONAL_PARTIES));
    }

// ========== MISSING SAILING INFO TESTS ==========

    @Test
    void populateSailingInformationFromCarrierBooking_handlesNullSailingInfo() {
        ShippingInstruction si = new ShippingInstruction();
        si.setSailingInformation(null);
        CarrierBooking cb = new CarrierBooking();
        cb.setSailingInformation(new SailingInformation());

        // Should not throw exception
        assertDoesNotThrow(() -> {
            Method method = ShippingInstructionsServiceImpl.class.getDeclaredMethod("populateSailingInformationFromCarrierBooking", ShippingInstruction.class, CarrierBooking.class);
            method.setAccessible(true);
            method.invoke(service, si, cb);
        });
    }

    @Test
    void setSailingInfoAndCutoff_handlesNullShippingInstruction() {
        ConsolidationDetails consol = new ConsolidationDetails();

        // Should not throw exception and return early
        assertDoesNotThrow(() -> {
            Method method = ShippingInstructionsServiceImpl.class.getDeclaredMethod("setSailingInfoAndCutoff", ShippingInstruction.class, ConsolidationDetails.class);
            method.setAccessible(true);
            method.invoke(service, null, consol);
        });
    }

    @Test
    void setSailingInfoAndCutoff_createsNewSailingInfoWhenNull() {
        ShippingInstruction si = new ShippingInstruction();
        si.setSailingInformation(null);
        ConsolidationDetails consol = new ConsolidationDetails();

        assertDoesNotThrow(() -> {
            Method method = ShippingInstructionsServiceImpl.class.getDeclaredMethod("setSailingInfoAndCutoff", ShippingInstruction.class, ConsolidationDetails.class);
            method.setAccessible(true);
            method.invoke(service, si, consol);
        });

        assertNotNull(si.getSailingInformation());
    }


// ========== MISSING ENTITY TYPE VALIDATION ==========

    @Test
    void getDefaultShippingInstructionValues_shouldThrow_whenUnsupportedEntityType() {
        assertThatThrownBy(() -> service.getDefaultShippingInstructionValues(null, 100L))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid value of Shipping Instruction Type");
    }

    @Test
    void updateShippingInstructions_shouldThrow_whenUnsupportedEntityTypeInReadOnlyFields() {
        ShippingInstructionRequest request = buildSimpleRequest();
        ShippingInstruction entity = buildSimpleEntity();
        entity.setEntityType(null); // unsupported

        when(repository.findById(request.getId())).thenReturn(Optional.of(entity));
        when(jsonHelper.convertValue(request, ShippingInstruction.class)).thenReturn(entity);

        assertThatThrownBy(() -> service.updateShippingInstructions(request))
                .isInstanceOf(ValidationException.class)
                .hasMessageContaining("Invalid value of Shipping Instruction Type");
    }

    @Test
    void setReferenceNumber_handlesNullReferenceList() {
        ShippingInstruction si = new ShippingInstruction();
        CarrierBooking cb = new CarrierBooking();
        cb.setId(100L);
        cb.setReferenceNumbersList(null);

        // This should not throw exception and should handle gracefully
        assertDoesNotThrow(() -> {
            // Use reflection to call private method for testing
            Method method = ShippingInstructionsServiceImpl.class.getDeclaredMethod("setReferenceNumber", ShippingInstruction.class, CarrierBooking.class);
            method.setAccessible(true);
            method.invoke(service, si, cb);
        });

        // Reference numbers should remain null/empty
        assertTrue(si.getReferenceNumbers() == null || si.getReferenceNumbers().isEmpty());
    }

    @Test
    void setCommonContainers_convertsAllContainerFields() {
        Containers container = new Containers();
        container.setContainerCode("20GP");
        container.setContainerNumber("CONT123");
        container.setPacks("5");
        container.setPacksType("BOX");
        container.setHsCode("HS123");
        container.setCommodityCode("COMM123");
        container.setGuid(UUID.randomUUID());

        try {
            Method method = ShippingInstructionsServiceImpl.class.getDeclaredMethod("setCommonContainers", List.class);
            method.setAccessible(true);

            @SuppressWarnings("unchecked")
            List<CommonContainers> result = (List<CommonContainers>) method.invoke(service, List.of(container));

            assertThat(result).hasSize(1);
            CommonContainers common = result.get(0);
            assertEquals("20GP", common.getContainerCode());
            assertEquals("CONT123", common.getContainerNo());
            assertEquals(5, common.getPacks());
            assertEquals("BOX", common.getPacksUnit());
            assertEquals("HS123", common.getHsCode());
        } catch (Exception e) {
            fail("Reflection failed: " + e.getMessage());
        }
    }

    @Test
    void testCancelShippingInstruction_success() {
        Long siId = 123L;
        ShippingInstruction si = new ShippingInstruction();
        si.setId(siId);
        si.setStatus(ShippingInstructionStatus.Draft);

        // Mock repo behavior
        when(repository.findById(siId)).thenReturn(Optional.of(si));
        when(repository.save(any(ShippingInstruction.class))).thenAnswer(invocation -> invocation.getArgument(0));

        // Call method
        service.cancelShippingInstruction(siId);

        // Verify SI status is updated
        assertEquals(ShippingInstructionStatus.Cancelled, si.getStatus());

        // Verify repository.save was called
        verify(repository, times(1)).save(si);

        // Verify transaction history creation
        verify(carrierBookingInttraUtil, times(1)).createTransactionHistory(
                (Requested.getDescription()),
                (FlowType.Inbound),
                ("SI Cancelled"),
                (SourceSystem.CargoRunner),
                (siId),
                (EntityTypeTransactionHistory.SI)
        );
    }


    @Test
    void testCancelShippingInstruction_notFound() {
        Long siId = 999L;
        when(repository.findById(siId)).thenReturn(Optional.empty());

        ValidationException ex = assertThrows(ValidationException.class, () -> service.cancelShippingInstruction(siId));
        assertEquals("INVALID_SI:" + siId, ex.getMessage());

        verify(repository, never()).save(any());
        verify(carrierBookingInttraUtil, never()).createTransactionHistory(any(), any(), any(), any(), any(), any());
    }
    @Test
    void testUpdateStatus_AperakFile_ValidInstruction() {
        ShippingInstructionEventDto eventDto = new ShippingInstructionEventDto();
        eventDto.setSiId("123");
        eventDto.setStatus("Accepted");
        eventDto.setComments("Sample comment");

        ShippingInstruction shippingInstruction = new ShippingInstruction();
        shippingInstruction.setId(123L);
        SailingInformation sailingInfo = new SailingInformation();
        sailingInfo.setCarrier("MAERSK");
        shippingInstruction.setSailingInformation(sailingInfo);
        // Given
        String fileName = ShippingInstructionsConstants.APERAK_PREFIX + "123" + ShippingInstructionsConstants.XML_SUFFIX;
        when(repository.findById(123L)).thenReturn(Optional.of(shippingInstruction));

        // When
        service.updateShippingInstructionsStatus(eventDto, fileName);

        // Then
        verify(repository, times(1)).save(any(ShippingInstruction.class));
        verify(carrierBookingInttraUtil, times(1))
                .createTransactionHistory(
                        eq(ShippingInstructionStatus.ConfirmedByCarrier.name()),
                        eq(FlowType.Outbound),
                        contains("MAERSK"),
                        eq(SourceSystem.Carrier),
                        eq(123L),
                        eq(EntityTypeTransactionHistory.SI)
                );
    }

    @Test
    void testUpdateStatus_AperakFile_InvalidInstruction() {
        ShippingInstructionEventDto eventDto = new ShippingInstructionEventDto();
        eventDto.setSiId("123");
        eventDto.setStatus("Accepted");
        eventDto.setComments("Sample comment");

        ShippingInstruction shippingInstruction = new ShippingInstruction();
        shippingInstruction.setId(123L);
        SailingInformation sailingInfo = new SailingInformation();
        sailingInfo.setCarrier("MAERSK");
        shippingInstruction.setSailingInformation(sailingInfo);
        // Given
        String fileName = ShippingInstructionsConstants.APERAK_PREFIX + "123" + ShippingInstructionsConstants.XML_SUFFIX;
        when(repository.findById(123L)).thenReturn(Optional.empty());

        // When
        service.updateShippingInstructionsStatus(eventDto, fileName);

        // Then
        verify(repository, never()).save(any());
        verify(carrierBookingInttraUtil, never()).createTransactionHistory(any(), any(), any(), any(), any(), any());
    }

    @Test
    void testUpdateStatus_ContrlxFile_ValidInstruction() {
        ShippingInstructionEventDto eventDto = new ShippingInstructionEventDto();
        eventDto.setSiId("123");
        eventDto.setStatus("Accepted");
        eventDto.setComments("Sample comment");

        ShippingInstruction shippingInstruction = new ShippingInstruction();
        shippingInstruction.setId(123L);
        SailingInformation sailingInfo = new SailingInformation();
        sailingInfo.setCarrier("MAERSK");
        shippingInstruction.setSailingInformation(sailingInfo);
        // Given
        String fileName = ShippingInstructionsConstants.CONTRLX_PREFIX + "123" + ShippingInstructionsConstants.XML_SUFFIX;
        when(repository.findById(123L)).thenReturn(Optional.of(shippingInstruction));

        // When
        service.updateShippingInstructionsStatus(eventDto, fileName);

        // Then
        verify(repository, times(1)).save(any(ShippingInstruction.class));
        verify(carrierBookingInttraUtil, times(1))
                .createTransactionHistory(
                        eq(ShippingInstructionStatus.AcceptedByCarrier.name()),
                        eq(FlowType.Outbound),
                        anyString(),
                        eq(SourceSystem.INTTRA),
                        eq(123L),
                        eq(EntityTypeTransactionHistory.SI)
                );
    }

    @Test
    void testUpdateStatus_FileNameNull_NoAction() {
        ShippingInstructionEventDto eventDto = new ShippingInstructionEventDto();
        eventDto.setSiId("123");
        eventDto.setStatus("ConfirmedByCarrier");
        eventDto.setComments("Sample comment");

        ShippingInstruction shippingInstruction = new ShippingInstruction();
        shippingInstruction.setId(123L);
        SailingInformation sailingInfo = new SailingInformation();
        sailingInfo.setCarrier("MAERSK");
        shippingInstruction.setSailingInformation(sailingInfo);
        // When
        service.updateShippingInstructionsStatus(eventDto, null);

        // Then
        verify(repository, never()).findById(any());
        verify(repository, never()).save(any());
        verify(carrierBookingInttraUtil, never()).createTransactionHistory(any(), any(), any(), any(), any(), any());
    }

    @Test
    void testUpdateStatus_SiIdEmpty_NoAction() {
        ShippingInstructionEventDto eventDto = new ShippingInstructionEventDto();
        eventDto.setSiId("123");
        eventDto.setStatus("ConfirmedByCarrier");
        eventDto.setComments("Sample comment");

        ShippingInstruction shippingInstruction = new ShippingInstruction();
        shippingInstruction.setId(123L);
        SailingInformation sailingInfo = new SailingInformation();
        sailingInfo.setCarrier("MAERSK");
        shippingInstruction.setSailingInformation(sailingInfo);
        // Given
        eventDto.setSiId(""); // empty

        String fileName = ShippingInstructionsConstants.APERAK_PREFIX + "file.xml";

        // When
        service.updateShippingInstructionsStatus(eventDto, fileName);

        // Then
        verify(repository, never()).findById(any());
        verify(repository, never()).save(any());
    }
    @Test
    void testSendNotification_Success() {
        ShippingInstruction shippingInstruction = new ShippingInstruction();

        // Mock email template response
        EmailTemplatesRequest emailTemplate = new EmailTemplatesRequest();
        emailTemplate.setType(SHIPPING_INSTRUCTION_EMAIL_TEMPLATE);
        emailTemplate.setSubject("Test Subject");
        emailTemplate.setBody("Test Body");

        // Mocking fetchEmailTemplate to return the email template
        when(carrierBookingInttraUtil.fetchEmailTemplate(anyList())).thenReturn(List.of(emailTemplate));

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            UsersDto user = Mockito.mock(UsersDto.class);
            userContext.when(UserContext::getUser).thenReturn(user);

            // Call the sendNotification method
            service.sendNotification(shippingInstruction);

            assertNotNull(emailTemplate);
        }
    }

    @Test
    void testSendNotification_NoEmailTemplateFound() throws JsonProcessingException {
        ShippingInstruction shippingInstruction = new ShippingInstruction();


        // Mock fetchEmailTemplate to return an empty list (no templates found)
        when(carrierBookingInttraUtil.fetchEmailTemplate(anyList())).thenReturn(Collections.emptyList());

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            UsersDto user = Mockito.mock(UsersDto.class);
            userContext.when(UserContext::getUser).thenReturn(user);

            // Call the sendNotification method
            service.sendNotification(shippingInstruction);

            // Verify that email sending did not occur
            verify(notificationService, times(0)).sendEmail(any(SendEmailBaseRequest.class));
        }
    }

    @Test
    void testSendNotification_ExceptionDuringEmailSending()  {
        ShippingInstruction shippingInstruction = new ShippingInstruction();
        shippingInstruction.setId(1L);
        shippingInstruction.setCarrierBookingNo("CB123");
        shippingInstruction.setStatus(ShippingInstructionStatus.Draft);

        // Mock email template response
        EmailTemplatesRequest emailTemplate = new EmailTemplatesRequest();
        emailTemplate.setType(SHIPPING_INSTRUCTION_EMAIL_TEMPLATE);
        emailTemplate.setSubject("Test Subject");
        emailTemplate.setBody("Test Body");

        // Mock fetchEmailTemplate to return the email template
        when(carrierBookingInttraUtil.fetchEmailTemplate(anyList())).thenReturn(List.of(emailTemplate));

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            UsersDto user = Mockito.mock(UsersDto.class);
            userContext.when(UserContext::getUser).thenReturn(user);

            // Call the sendNotification method
            service.sendNotification(shippingInstruction);

            // Verify that an error was logged
            assertNotNull(emailTemplate);
        }
    }

    @Test
    void testSendNotification_NullEmailTemplate() throws JsonProcessingException {
        ShippingInstruction shippingInstruction = new ShippingInstruction();


        // Mock fetchEmailTemplate to return a null template
        when(carrierBookingInttraUtil.fetchEmailTemplate(anyList())).thenReturn(Collections.singletonList(null));

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            UsersDto user = Mockito.mock(UsersDto.class);
            userContext.when(UserContext::getUser).thenReturn(user);

            // Call the sendNotification method
            service.sendNotification(shippingInstruction);

            // Verify that email sending did not occur
            verify(notificationService, times(0)).sendEmail(any(SendEmailBaseRequest.class));
        }
    }

    // ========== SYNC COMMON CONTAINERS TESTS ==========

    @Test
    void syncCommonContainersByConsolId_whenConsolIdIsNull_shouldReturnEarly() {
        // Act
        shippingInstructionUtil.syncCommonContainersByConsolId(null);

        // Assert
        verifyNoInteractions(shippingInstructionDao, containerDao, commonContainersDao);
    }

    @Test
    void syncCommonContainersByConsolId_whenNoShippingInstructionsFound_shouldReturnEarly() {
        // Arrange
        Long consolId = 100L;
        when(shippingInstructionDao.findByEntityTypeAndEntityIdIn(EntityType.CONSOLIDATION, List.of(consolId)))
                .thenReturn(Collections.emptyList());
        when(shippingInstructionDao.findByCarrierBookingConsolId(List.of(consolId)))
                .thenReturn(Collections.emptyList());

        // Act
        shippingInstructionUtil.syncCommonContainersByConsolId(consolId);

        // Assert
        verify(shippingInstructionDao).findByEntityTypeAndEntityIdIn(EntityType.CONSOLIDATION, List.of(consolId));
        verify(shippingInstructionDao).findByCarrierBookingConsolId(List.of(consolId));
        verifyNoInteractions(containerDao, commonContainersDao);
    }

    @Test
    void syncCommonContainersByConsolId_whenNoContainersFound_shouldReturnEarly() {
        // Arrange
        Long consolId = 200L;
        Long siId = 300L;

        ShippingConsoleIdProjection projection = mock(ShippingConsoleIdProjection.class);
        when(projection.getId()).thenReturn(siId);
        when(shippingInstructionDao.findByEntityTypeAndEntityIdIn(EntityType.CONSOLIDATION, List.of(consolId)))
                .thenReturn(List.of(projection));
        when(containerDao.findByConsolidationId(consolId))
                .thenReturn(Collections.emptyList());

        // Act
        shippingInstructionUtil.syncCommonContainersByConsolId(consolId);

        // Assert
        verify(containerDao).findByConsolidationId(consolId);
        verifyNoInteractions(commonContainersDao);
    }

    @Test
    void syncCommonContainersByConsolId_whenContainersFoundNull_shouldReturnEarly() {
        // Arrange
        Long consolId = 200L;
        Long siId = 300L;

        ShippingConsoleIdProjection projection = mock(ShippingConsoleIdProjection.class);
        when(projection.getId()).thenReturn(siId);
        when(shippingInstructionDao.findByEntityTypeAndEntityIdIn(EntityType.CONSOLIDATION, List.of(consolId)))
                .thenReturn(List.of(projection));
        when(containerDao.findByConsolidationId(consolId))
                .thenReturn(null);

        // Act
        shippingInstructionUtil.syncCommonContainersByConsolId(consolId);

        // Assert
        verify(containerDao).findByConsolidationId(consolId);
        verifyNoInteractions(commonContainersDao);
    }

    @Test
    void syncCommonContainersByConsolId_whenDirectSIFound_shouldCreateNewCommonContainers() {
        // Arrange
        Long consolId = 100L;
        Long siId = 200L;
        UUID containerGuid = UUID.randomUUID();

        ShippingConsoleIdProjection projection = mock(ShippingConsoleIdProjection.class);
        when(projection.getId()).thenReturn(siId);
        when(shippingInstructionDao.findByEntityTypeAndEntityIdIn(EntityType.CONSOLIDATION, List.of(consolId)))
                .thenReturn(List.of(projection));

        Containers container = new Containers();
        container.setGuid(containerGuid);
        container.setContainerNumber("CONT001");
        when(containerDao.findByConsolidationId(consolId))
                .thenReturn(List.of(container));

        when(commonContainersDao.getAll(List.of(containerGuid)))
                .thenReturn(Collections.emptyList());

        // Act
        shippingInstructionUtil.syncCommonContainersByConsolId(consolId);

        // Assert
        ArgumentCaptor<List<CommonContainers>> captor = ArgumentCaptor.forClass(List.class);
        verify(commonContainersDao).saveAll(captor.capture());

        List<CommonContainers> saved = captor.getValue();
        assertThat(saved).hasSize(1);
        assertThat(saved.get(0).getContainerRefGuid()).isEqualTo(containerGuid);
        assertThat(saved.get(0).getShippingInstructionId()).isEqualTo(siId);
    }

    @Test
    void syncCommonContainersByConsolId_whenSIFoundViaCarrier_shouldUseCarrierRoute() {
        // Arrange
        Long consolId = 100L;
        Long siId = 200L;
        UUID containerGuid = UUID.randomUUID();

        when(shippingInstructionDao.findByEntityTypeAndEntityIdIn(EntityType.CONSOLIDATION, List.of(consolId)))
                .thenReturn(Collections.emptyList());

        ShippingConsoleIdProjection projection = mock(ShippingConsoleIdProjection.class);
        when(projection.getId()).thenReturn(siId);
        when(shippingInstructionDao.findByCarrierBookingConsolId(List.of(consolId)))
                .thenReturn(List.of(projection));

        Containers container = new Containers();
        container.setGuid(containerGuid);
        container.setContainerNumber("CONT001");
        when(containerDao.findByConsolidationId(consolId))
                .thenReturn(List.of(container));

        when(commonContainersDao.getAll(List.of(containerGuid)))
                .thenReturn(Collections.emptyList());

        // Act
        shippingInstructionUtil.syncCommonContainersByConsolId(consolId);

        // Assert
        verify(shippingInstructionDao).findByCarrierBookingConsolId(List.of(consolId));
        ArgumentCaptor<List<CommonContainers>> captor = ArgumentCaptor.forClass(List.class);
        verify(commonContainersDao).saveAll(captor.capture());

        List<CommonContainers> saved = captor.getValue();
        assertThat(saved).hasSize(1);
        assertThat(saved.get(0).getShippingInstructionId()).isEqualTo(siId);
    }
}