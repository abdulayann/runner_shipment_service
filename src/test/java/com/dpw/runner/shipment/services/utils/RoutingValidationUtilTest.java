package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dto.request.BulkUpdateRoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.function.Executable;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;

import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_SEA;
import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class RoutingValidationUtilTest {
    @Mock
    private IShipmentServiceV3 shipmentService;
    @Mock
    private IConsolidationService consolidationService;
    @Mock
    private ICustomerBookingService customerBookingService;
    @Mock
    private IRoutingsDao routingsV3Dao;

    @InjectMocks
    private RoutingValidationUtil routingValidationUtil;

    private List<RoutingsResponse> routingsResponses;

    @BeforeEach
    void setUp() {
        routingsResponses = new ArrayList<>();
    }

    @Test
    void testValidateUpdateRequest() {
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateUpdateRequest(null));
    }

    @Test
    void testValidateUpdateRequest1() {
        RoutingsRequest routingsRequest = new RoutingsRequest();
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateUpdateRequest(routingsRequest));
    }

    @Test
    void testValidateUpdateRequest2() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().id(1L).build();
        assertDoesNotThrow(() -> routingValidationUtil.validateUpdateRequest(routingsRequest));
    }

    @Test
    void testValidateDeleteRequest() {
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateDeleteRequest(null));
    }

    @Test
    void testValidateDeleteRequest1() {
        CommonGetRequest request = CommonGetRequest.builder().build();
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateDeleteRequest(request));
    }

    @Test
    void testValidateDeleteRequest2() {
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        assertDoesNotThrow(() -> routingValidationUtil.validateDeleteRequest(request));
    }

    @Test
    void testValidateUpdateBulkRequest() {
        List<RoutingsRequest> routingListRequest = new ArrayList<>();
        routingListRequest.add(RoutingsRequest.builder().id(1L).build());
        routingListRequest.add(RoutingsRequest.builder().id(2L).build());
        List<Routings> existingRoutings = new ArrayList<>();
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateUpdateBulkRequest(routingListRequest, existingRoutings));
    }

    @Test
    void testValidateUpdateBulkRequest1() {
        List<RoutingsRequest> routingListRequest = new ArrayList<>();
        routingListRequest.add(RoutingsRequest.builder().id(1L).build());
        List<Routings> existingRoutings = new ArrayList<>();
        Routings routings = new Routings();
        routings.setId(1L);
        existingRoutings.add(routings);
        assertDoesNotThrow(() -> routingValidationUtil.validateUpdateBulkRequest(routingListRequest, existingRoutings));
    }

    @Test
    void testValidateDeleteBulkRequest() {
        List<RoutingsRequest> requests = new ArrayList<>();
        requests.add(RoutingsRequest.builder().build());
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateDeleteBulkRequest(requests));
    }

    @Test
    void testValidateDeleteBulkRequest1() {
        List<RoutingsRequest> requests = new ArrayList<>();
        requests.add(RoutingsRequest.builder().id(1L).build());
        assertDoesNotThrow(() -> routingValidationUtil.validateDeleteBulkRequest(requests));
    }

    @Test
    void testValidateModule_Shipment() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().build();
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateModule(routingsRequest, Constants.SHIPMENT));
    }

    @Test
    void testValidateModule_Shipment1() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().shipmentId(1L).build();
        when(shipmentService.findById(routingsRequest.getShipmentId())).thenReturn(Optional.empty());
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateModule(routingsRequest, Constants.SHIPMENT));
    }

    @Test
    void testValidateModule_Shipment2() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().shipmentId(0L).build();
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateModule(routingsRequest, Constants.SHIPMENT));
    }

    @Test
    void testValidateModule_Shipment3() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().shipmentId(1L).build();
        when(shipmentService.findById(routingsRequest.getShipmentId())).thenReturn(Optional.of(ShipmentDetails.builder().build()));
        assertDoesNotThrow(() -> routingValidationUtil.validateModule(routingsRequest, Constants.SHIPMENT));
    }

    /* No console attached  : Adding should allow  */
    @Test
    void testValidateModule_Shipment4() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().carriage(RoutingCarriage.MAIN_CARRIAGE).shipmentId(1L).build();
        lenient().when(shipmentService.findById(routingsRequest.getShipmentId())).thenReturn(Optional.of(ShipmentDetails.builder().build()));
        lenient().when(routingsV3Dao.findByShipmentId(routingsRequest.getShipmentId())).thenReturn(List.of(Routings.builder().carriage(RoutingCarriage.MAIN_CARRIAGE).inheritedFromConsolidation(true).build()));
        assertDoesNotThrow(() -> routingValidationUtil.checkIfMainCarriageAllowed(routingsRequest));
    }

    /* console attached with no inherit  : Adding should not allow  */
    @Test
    void testValidateModule_Shipment5() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().carriage(RoutingCarriage.MAIN_CARRIAGE).shipmentId(1L).build();
        lenient().when(shipmentService.findById(routingsRequest.getShipmentId())).thenReturn(Optional.of(ShipmentDetails.builder().consolRef("123").build()));
        lenient().when(routingsV3Dao.findByShipmentId(routingsRequest.getShipmentId())).thenReturn(List.of(Routings.builder().carriage(RoutingCarriage.MAIN_CARRIAGE).inheritedFromConsolidation(false).build()));
        assertThrows(ValidationException.class, () -> routingValidationUtil.checkIfMainCarriageAllowed(routingsRequest));
    }

    /* console attached with no CARRIAGE : Adding should not allow  */
    @Test
    void testValidateModule_Shipment7() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().carriage(RoutingCarriage.MAIN_CARRIAGE).shipmentId(1L).build();
        lenient().when(shipmentService.findById(routingsRequest.getShipmentId())).thenReturn(Optional.of(ShipmentDetails.builder().consolRef("1234").build()));
        lenient().when(routingsV3Dao.findByShipmentId(routingsRequest.getShipmentId())).thenReturn(List.of());
        assertThrows(ValidationException.class, () -> routingValidationUtil.checkIfMainCarriageAllowed(routingsRequest));
    }

    /* console attached with  inherit  : Adding should allow  */
    @Test
    void testValidateModule_Shipment6() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().carriage(RoutingCarriage.MAIN_CARRIAGE).shipmentId(1L).build();
        lenient().when(shipmentService.findById(routingsRequest.getShipmentId())).thenReturn(Optional.of(ShipmentDetails.builder().consolRef("12345").build()));
        lenient().when(routingsV3Dao.findByShipmentId(routingsRequest.getShipmentId())).thenReturn(List.of(Routings.builder().carriage(RoutingCarriage.MAIN_CARRIAGE).inheritedFromConsolidation(true).build()));
        assertDoesNotThrow(() -> routingValidationUtil.checkIfMainCarriageAllowed(routingsRequest));
    }

    @Test
    void testValidateModule_Consolidation() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().build();
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateModule(routingsRequest, Constants.CONSOLIDATION));
    }

    @Test
    void testValidateModule_Consolidation1() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().consolidationId(1L).build();
        when(consolidationService.findById(routingsRequest.getConsolidationId())).thenReturn(Optional.empty());
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateModule(routingsRequest, Constants.CONSOLIDATION));
    }

    @Test
    void testValidateModule_Consolidation2() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().consolidationId(0L).build();
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateModule(routingsRequest, Constants.CONSOLIDATION));
    }

    @Test
    void testValidateModule_Consolidation3() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().consolidationId(1L).build();
        when(consolidationService.findById(routingsRequest.getConsolidationId())).thenReturn(Optional.of(ConsolidationDetails.builder().build()));
        assertDoesNotThrow(() -> routingValidationUtil.validateModule(routingsRequest, Constants.CONSOLIDATION));
    }

    @Test
    void testValidateModule_Booking() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().build();
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateModule(routingsRequest, Constants.BOOKING));
    }

    @Test
    void testValidateModule_Booking1() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().bookingId(1L).build();
        when(customerBookingService.findById(routingsRequest.getBookingId())).thenReturn(Optional.empty());
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateModule(routingsRequest, Constants.BOOKING));
    }

    @Test
    void testValidateModule_Booking2() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().bookingId(0L).build();
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateModule(routingsRequest, Constants.BOOKING));
    }

    @Test
    void testValidateModule_Booking3() {
        RoutingsRequest routingsRequest = RoutingsRequest.builder().bookingId(1L).build();
        when(customerBookingService.findById(routingsRequest.getBookingId())).thenReturn(Optional.of(CustomerBooking.builder().build()));
        assertDoesNotThrow(() -> routingValidationUtil.validateModule(routingsRequest, Constants.BOOKING));
    }

    @Test
    void testValidateRoutingsRequest() {
        List<RoutingsRequest> requests = new ArrayList<>();
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateRoutingsRequest(requests, Constants.SHIPMENT));
    }

    @Test
    void testValidateRoutingsRequest_Shipment() {
        List<RoutingsRequest> requests = new ArrayList<>();
        requests.add(RoutingsRequest.builder().build());
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateRoutingsRequest(requests, Constants.SHIPMENT));
    }

    @Test
    void testValidateRoutingsRequest_Shipment1() {
        List<RoutingsRequest> requests = new ArrayList<>();
        requests.add(RoutingsRequest.builder().shipmentId(1L).consolidationId(2L).build());
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateRoutingsRequest(requests, Constants.SHIPMENT));
    }

    @Test
    void testValidateRoutingsRequest_Shipment2() {
        List<RoutingsRequest> requests = new ArrayList<>();
        requests.add(RoutingsRequest.builder().shipmentId(1L).build());
        requests.add(RoutingsRequest.builder().shipmentId(2L).build());
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateRoutingsRequest(requests, Constants.SHIPMENT));
    }

    @Test
    void testValidateRoutingsRequest_Shipment4() {
        List<RoutingsRequest> requests = new ArrayList<>();
        requests.add(RoutingsRequest.builder().consolidationId(1L).build());
        requests.add(RoutingsRequest.builder().consolidationId(2L).build());
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateRoutingsRequest(requests, Constants.CONSOLIDATION));
    }

    @Test
    void testValidateRoutingsRequest_Shipment5() {
        List<RoutingsRequest> requests = new ArrayList<>();
        requests.add(RoutingsRequest.builder().shipmentId(1L).build());
        requests.add(RoutingsRequest.builder().consolidationId(2L).build());
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateRoutingsRequest(requests, Constants.CONSOLIDATION));
    }

    @Test
    void testValidateMainCarriageAdjacencyInIncoming() {
        List<RoutingsRequest> incomingRoutings = new ArrayList<>();
        incomingRoutings.add(RoutingsRequest.builder().carriage(RoutingCarriage.MAIN_CARRIAGE).inheritedFromConsolidation(true).build());
        incomingRoutings.add(RoutingsRequest.builder().carriage(RoutingCarriage.MAIN_CARRIAGE).inheritedFromConsolidation(false).build());
        incomingRoutings.add(RoutingsRequest.builder().carriage(RoutingCarriage.MAIN_CARRIAGE).inheritedFromConsolidation(true).build());
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateMainCarriageAdjacencyInIncoming(incomingRoutings));
    }

    @Test
    void testValidateMainCarriageAdjacencyInIncoming1() {
        List<RoutingsRequest> incomingRoutings = new ArrayList<>();
        incomingRoutings.add(RoutingsRequest.builder().carriage(RoutingCarriage.MAIN_CARRIAGE).inheritedFromConsolidation(true).build());
        incomingRoutings.add(RoutingsRequest.builder().carriage(RoutingCarriage.MAIN_CARRIAGE).inheritedFromConsolidation(true).build());
        incomingRoutings.add(RoutingsRequest.builder().carriage(RoutingCarriage.MAIN_CARRIAGE).inheritedFromConsolidation(true).build());
        assertDoesNotThrow(() -> routingValidationUtil.validateMainCarriageAdjacencyInIncoming(incomingRoutings));
    }

    @Test
    void testValidateBulkUpdateRoutingRequest() {
        BulkUpdateRoutingsRequest request = new BulkUpdateRoutingsRequest();
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateBulkUpdateRoutingRequest(request, Constants.SHIPMENT));
    }

    @Test
    void testValidateBulkUpdateRoutingRequest1() {
        BulkUpdateRoutingsRequest request = new BulkUpdateRoutingsRequest();
        request.setRoutings(List.of(RoutingsRequest.builder().build()));
        assertThrows(ValidationException.class, () -> routingValidationUtil.validateBulkUpdateRoutingRequest(request, Constants.SHIPMENT));
    }

    @Test
    void testValidateMainCarriageRoutingLegs_Success() {

        RoutingsRequest firstRoutingLegRequest = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .etd(LocalDateTime.now())
                .atd(LocalDateTime.now())
                .build();

        RoutingsRequest lastRoutingLegRequest = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .eta(LocalDateTime.now().plusHours(10))
                .ata(LocalDateTime.now().plusHours(12))
                .build();

        assertDoesNotThrow(() -> routingValidationUtil.validateMainCarriageRoutingLegs(List.of(firstRoutingLegRequest, lastRoutingLegRequest)));
    }

    @Test
    void testValidateMainCarriageRoutingLegs_ETDMoreThanETA() {

        LocalDateTime now = LocalDateTime.now();
        RoutingsRequest firstRoutingLegRequest = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .etd(now.plusDays(2))
                .atd(now)
                .build();

        RoutingsRequest lastRoutingLegRequest = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .eta(now)
                .ata(now)
                .build();

        Executable executable = () -> routingValidationUtil.validateMainCarriageRoutingLegs(List.of(firstRoutingLegRequest, lastRoutingLegRequest));
        ValidationException exception = assertThrows(ValidationException.class, executable);
        assertEquals("ETA (Last main-carriage) cannot be less than ETD (First Main-carriage)", exception.getMessage());
    }

    @Test
    void testValidateMainCarriageRoutingLegs_ATALessThanATD() {

        RoutingsRequest firstRoutingLegRequest = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .etd(LocalDateTime.now())
                .atd(LocalDateTime.now())
                .build();

        RoutingsRequest lastRoutingLegRequest = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .eta(LocalDateTime.now().plusHours(10))
                .ata(LocalDateTime.now().minusHours(30))
                .build();

        Executable executable = () -> routingValidationUtil.validateMainCarriageRoutingLegs(List.of(firstRoutingLegRequest, lastRoutingLegRequest));
        ValidationException exception = assertThrows(ValidationException.class, executable);
        assertEquals("ATA (Last Main-carriage) cannot be less than ATD (First Main-carriage)", exception.getMessage());

    }

    @Test
    void testValidateMainCarriageRoutingLegs_ATASetInFuture() {

        RoutingsRequest firstRoutingLegRequest = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .etd(LocalDateTime.now())
                .atd(LocalDateTime.now())
                .build();

        RoutingsRequest lastRoutingLegRequest = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .eta(LocalDateTime.now().plusHours(10))
                .ata(LocalDateTime.now().plusHours(30))
                .build();

        Executable executable = () -> routingValidationUtil.validateMainCarriageRoutingLegs(List.of(firstRoutingLegRequest, lastRoutingLegRequest));
        ValidationException exception = assertThrows(ValidationException.class, executable);
        assertEquals("ATA (Last Main-carriage) cannot be more than Current Date", exception.getMessage());
    }

    @Test
    void testValidateMainCarriageRoutingLegs_ATDSetInFuture() {
        LocalDateTime now = LocalDateTime.now();
        RoutingsRequest firstRoutingLegRequest = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .atd(now.plusHours(30))
                .build();

        RoutingsRequest lastRoutingLegRequest = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .eta(now.plusHours(10))
                .ata(now.plusHours(20))
                .build();

        Executable executable = () -> routingValidationUtil.validateMainCarriageRoutingLegs(List.of(firstRoutingLegRequest, lastRoutingLegRequest));
        ValidationException exception = assertThrows(ValidationException.class, executable);
        assertEquals("ATD (First Main-carriage) cannot be more than Current Date", exception.getMessage());
    }

    @Test
    void testValidateMainCarriageRoutingLegs_ATAIsNull() {
        LocalDateTime now = LocalDateTime.now();
        RoutingsRequest firstRoutingLegRequest = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .etd(now)
                .atd(now)
                .build();

        RoutingsRequest lastRoutingLegRequest = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .eta(now.plusHours(10))
                .ata(null)
                .build();

        assertDoesNotThrow(() -> routingValidationUtil.validateMainCarriageRoutingLegs(List.of(firstRoutingLegRequest, lastRoutingLegRequest)));
    }

    @Test
    void testValidateMainCarriageRoutingLegs_ATDIsNull() {
        RoutingsRequest firstRoutingLegRequest = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .etd(LocalDateTime.now())
                .atd(null)
                .build();

        RoutingsRequest lastRoutingLegRequest = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .eta(LocalDateTime.now().plusHours(10))
                .ata(LocalDateTime.now().plusHours(12))
                .build();

        assertDoesNotThrow(() -> routingValidationUtil.validateMainCarriageRoutingLegs(List.of(firstRoutingLegRequest, lastRoutingLegRequest)));
    }

    @Test
    void testFindMainCarriageLeg_returnsFirstAndLastSuccess() {
        RoutingsRequest preCarriageRoutingLeg = RoutingsRequest.builder()
                .carriage(RoutingCarriage.PRE_CARRIAGE)
                .build();

        RoutingsRequest mainCarriageRoutingLeg1 = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .etd(LocalDateTime.now().minusDays(1))
                .build();

        RoutingsRequest mainCarriageRoutingLeg2 = RoutingsRequest.builder()
                .carriage(RoutingCarriage.MAIN_CARRIAGE)
                .eta(LocalDateTime.now().plusDays(1))
                .build();

        List<RoutingsRequest> list = List.of(preCarriageRoutingLeg, mainCarriageRoutingLeg1, mainCarriageRoutingLeg2);
        assertDoesNotThrow(() -> routingValidationUtil.validateMainCarriageRoutingLegs(list));
    }

    @Test
    void testValidateMainCarriageRoutingLegs_NoMainCarriageLegs() {

        RoutingsRequest preCarriage = RoutingsRequest.builder()
                .carriage(RoutingCarriage.PRE_CARRIAGE)
                .build();
        RoutingsRequest onCarriage = RoutingsRequest.builder()
                .carriage(RoutingCarriage.ON_CARRIAGE)
                .build();

        List<RoutingsRequest> list = List.of(preCarriage, onCarriage);
        assertDoesNotThrow(() -> routingValidationUtil.validateMainCarriageRoutingLegs(list));
    }

    @Test
    @DisplayName("Should return empty list when routing responses is null")
    void testValidateRoutingLegs_NullInput() {
        List<String> result = routingValidationUtil.validateRoutingLegs(null, new HashMap<>());

        assertTrue(result.isEmpty());
    }

    @Test
    @DisplayName("Should return empty list when routing responses is empty")
    void testValidateRoutingLegs_EmptyList() {
        List<String> result = routingValidationUtil.validateRoutingLegs(routingsResponses, new HashMap<>());

        assertTrue(result.isEmpty());
    }

    @Test
    @DisplayName("Should return empty list when only one leg exists")
    void testValidateRoutingLegs_SingleLeg() {
        RoutingsResponse leg1 = createRoutingResponse(1L,
                LocalDateTime.of(2024, 1, 15, 10, 0),
                LocalDateTime.of(2024, 1, 15, 12, 0));
        routingsResponses.add(leg1);

        List<String> result = routingValidationUtil.validateRoutingLegs(routingsResponses, new HashMap<>());

        assertTrue(result.isEmpty());
    }

    @Test
    @DisplayName("Should return empty list when ETD is after previous ETA (valid scenario)")
    void testValidateRoutingLegs_ValidTiming() {
        RoutingsResponse leg1 = createRoutingResponse(1L,
                LocalDateTime.of(2024, 1, 15, 10, 0), // ETA
                LocalDateTime.of(2024, 1, 15, 12, 0)); // ETD
        RoutingsResponse leg2 = createRoutingResponse(2L,
                LocalDateTime.of(2024, 1, 16, 8, 0),   // ETA
                LocalDateTime.of(2024, 1, 15, 13, 0)); // ETD (after leg1 ETA)

        routingsResponses.add(leg1);
        routingsResponses.add(leg2);

        List<String> result = routingValidationUtil.validateRoutingLegs(routingsResponses, new HashMap<>());

        assertTrue(result.isEmpty());
    }

    @Test
    @DisplayName("Should return validation error when ETD is before previous ETA")
    void testValidateRoutingLegs_InvalidTiming_ETDBeforeETA() {
        RoutingsResponse leg1 = createRoutingResponse(1L,
                LocalDateTime.of(2024, 1, 15, 10, 0), // ETA
                LocalDateTime.of(2024, 1, 15, 12, 0)); // ETD
        RoutingsResponse leg2 = createRoutingResponse(2L,
                LocalDateTime.of(2024, 1, 16, 8, 0),   // ETA
                LocalDateTime.of(2024, 1, 15, 9, 0));  // ETD (before leg1 ETA)

        routingsResponses.add(leg1);
        routingsResponses.add(leg2);

        List<String> result = routingValidationUtil.validateRoutingLegs(routingsResponses, new HashMap<>());

        assertEquals(1, result.size());
        assertEquals("ETD (of Leg No. 2) should be greater than ETA (of Leg No. 1)", result.get(0));
    }

    @Test
    @DisplayName("Should return multiple validation errors for multiple invalid legs")
    void testValidateRoutingLegs_MultipleInvalidLegs() {
        RoutingsResponse leg1 = createRoutingResponse(1L,
                LocalDateTime.of(2024, 1, 15, 10, 0), // ETA
                LocalDateTime.of(2024, 1, 15, 12, 0)); // ETD
        RoutingsResponse leg2 = createRoutingResponse(2L,
                LocalDateTime.of(2024, 1, 16, 8, 0),   // ETA
                LocalDateTime.of(2024, 1, 15, 9, 0));  // ETD (invalid - before leg1 ETA)
        RoutingsResponse leg3 = createRoutingResponse(3L,
                LocalDateTime.of(2024, 1, 17, 10, 0),  // ETA
                LocalDateTime.of(2024, 1, 16, 7, 0));  // ETD (invalid - before leg2 ETA)

        routingsResponses.add(leg1);
        routingsResponses.add(leg2);
        routingsResponses.add(leg3);

        List<String> result = routingValidationUtil.validateRoutingLegs(routingsResponses, new HashMap<>());

        assertEquals(2, result.size());
        assertEquals("ETD (of Leg No. 2) should be greater than ETA (of Leg No. 1)", result.get(0));
        assertEquals("ETD (of Leg No. 3) should be greater than ETA (of Leg No. 2)", result.get(1));
    }

    @Test
    @DisplayName("Should skip validation when current leg ETD is null")
    void testValidateRoutingLegs_NullETD() {
        RoutingsResponse leg1 = createRoutingResponse(1L,
                LocalDateTime.of(2024, 1, 15, 10, 0), // ETA
                LocalDateTime.of(2024, 1, 15, 12, 0)); // ETD
        RoutingsResponse leg2 = createRoutingResponse(2L,
                LocalDateTime.of(2024, 1, 16, 8, 0),   // ETA
                null);                                 // ETD is null

        routingsResponses.add(leg1);
        routingsResponses.add(leg2);

        List<String> result = routingValidationUtil.validateRoutingLegs(routingsResponses, new HashMap<>());

        assertTrue(result.isEmpty());
    }

    @Test
    @DisplayName("Should skip validation when previous leg ETA is null")
    void testValidateRoutingLegs_NullPreviousETA() {
        RoutingsResponse leg1 = createRoutingResponse(1L,
                null,                                 // ETA is null
                LocalDateTime.of(2024, 1, 15, 12, 0)); // ETD
        RoutingsResponse leg2 = createRoutingResponse(2L,
                LocalDateTime.of(2024, 1, 16, 8, 0),   // ETA
                LocalDateTime.of(2024, 1, 15, 9, 0));  // ETD

        routingsResponses.add(leg1);
        routingsResponses.add(leg2);

        List<String> result = routingValidationUtil.validateRoutingLegs(routingsResponses, new HashMap<>());

        assertTrue(result.isEmpty());
    }

    @Test
    @DisplayName("Should return null when no validation errors exist")
    void testGetWarningMessage_NoErrors() {
        RoutingsResponse leg1 = createRoutingResponse(1L,
                LocalDateTime.of(2024, 1, 15, 10, 0), // ETA
                LocalDateTime.of(2024, 1, 15, 12, 0)); // ETD
        RoutingsResponse leg2 = createRoutingResponse(2L,
                LocalDateTime.of(2024, 1, 16, 8, 0),   // ETA
                LocalDateTime.of(2024, 1, 15, 13, 0)); // ETD (valid)

        routingsResponses.add(leg1);
        routingsResponses.add(leg2);

        String result = routingValidationUtil.getWarningMessage(routingsResponses, new HashMap<>());

        assertNull(result);
    }

    @Test
    @DisplayName("Should return single line warning message for single validation error")
    void testGetWarningMessage_SingleError() {
        RoutingsResponse leg1 = createRoutingResponse(1L,
                LocalDateTime.of(2024, 1, 15, 10, 0), // ETA
                LocalDateTime.of(2024, 1, 15, 12, 0)); // ETD
        RoutingsResponse leg2 = createRoutingResponse(2L,
                LocalDateTime.of(2024, 1, 16, 8, 0),   // ETA
                LocalDateTime.of(2024, 1, 15, 9, 0));  // ETD (invalid)

        routingsResponses.add(leg1);
        routingsResponses.add(leg2);

        String result = routingValidationUtil.getWarningMessage(routingsResponses, new HashMap<>());

        assertNotNull(result);
        assertEquals("ETD (of Leg No. 2) should be greater than ETA (of Leg No. 1)", result);
    }

    @Test
    @DisplayName("Should return multi-line warning message for multiple validation errors")
    void testGetWarningMessage_MultipleErrors() {
        RoutingsResponse leg1 = createRoutingResponse(1L,
                LocalDateTime.of(2024, 1, 15, 10, 0), // ETA
                LocalDateTime.of(2024, 1, 15, 12, 0)); // ETD
        RoutingsResponse leg2 = createRoutingResponse(2L,
                LocalDateTime.of(2024, 1, 16, 8, 0),   // ETA
                LocalDateTime.of(2024, 1, 15, 9, 0));  // ETD (invalid)
        RoutingsResponse leg3 = createRoutingResponse(3L,
                LocalDateTime.of(2024, 1, 17, 10, 0),  // ETA
                LocalDateTime.of(2024, 1, 16, 7, 0));  // ETD (invalid)

        routingsResponses.add(leg1);
        routingsResponses.add(leg2);
        routingsResponses.add(leg3);

        String result = routingValidationUtil.getWarningMessage(routingsResponses, new HashMap<>());

        assertNotNull(result);
        String[] lines = result.split("###");
        assertEquals(2, lines.length);
        assertEquals("ETD (of Leg No. 2) should be greater than ETA (of Leg No. 1)", lines[0]);
        assertEquals("ETD (of Leg No. 3) should be greater than ETA (of Leg No. 2)", lines[1]);
    }

    @Test
    @DisplayName("Should return null for getWarningMessage when input is null")
    void testGetWarningMessage_NullInput() {
        String result = routingValidationUtil.getWarningMessage(null, new HashMap<>());

        assertNull(result);
    }

    @Test
    @DisplayName("Should return null for getWarningMessage when input is empty")
    void testGetWarningMessage_EmptyInput() {
        String result = routingValidationUtil.getWarningMessage(routingsResponses, new HashMap<>());

        assertNull(result);
    }

    // Helper method to create RoutingsResponse objects for testing
    private RoutingsResponse createRoutingResponse(Long leg, LocalDateTime eta, LocalDateTime etd) {
        RoutingsResponse response = new RoutingsResponse();
        response.setLeg(leg);
        response.setEta(eta);
        response.setEtd(etd);
        return response;
    }

    // Additional edge case tests
    @Test
    @DisplayName("Should handle mixed valid and invalid legs correctly")
    void testValidateRoutingLegs_MixedValidInvalid() {
        RoutingsResponse leg1 = createRoutingResponse(1L,
                LocalDateTime.of(2024, 1, 15, 10, 0), // ETA
                LocalDateTime.of(2024, 1, 15, 12, 0)); // ETD
        RoutingsResponse leg2 = createRoutingResponse(2L,
                LocalDateTime.of(2024, 1, 16, 8, 0),   // ETA
                LocalDateTime.of(2024, 1, 15, 13, 0)); // ETD (valid)
        RoutingsResponse leg3 = createRoutingResponse(3L,
                LocalDateTime.of(2024, 1, 17, 10, 0),  // ETA
                LocalDateTime.of(2024, 1, 16, 7, 0));  // ETD (invalid - before leg2 ETA)
        RoutingsResponse leg4 = createRoutingResponse(4L,
                LocalDateTime.of(2024, 1, 18, 15, 0),  // ETA
                LocalDateTime.of(2024, 1, 17, 11, 0)); // ETD (valid)

        routingsResponses.add(leg1);
        routingsResponses.add(leg2);
        routingsResponses.add(leg3);
        routingsResponses.add(leg4);

        List<String> result = routingValidationUtil.validateRoutingLegs(routingsResponses, new HashMap<>());

        assertEquals(1, result.size());
        assertEquals("ETD (of Leg No. 3) should be greater than ETA (of Leg No. 2)", result.get(0));
    }

    @Test
    @DisplayName("Should handle same date different time correctly")
    void testValidateRoutingLegs_SameDateDifferentTime() {
        RoutingsResponse leg1 = createRoutingResponse(1L,
                LocalDateTime.of(2024, 1, 15, 10, 30), // ETA: 10:30 AM
                LocalDateTime.of(2024, 1, 15, 12, 0));  // ETD: 12:00 PM
        RoutingsResponse leg2 = createRoutingResponse(2L,
                LocalDateTime.of(2024, 1, 16, 8, 0),    // ETA
                LocalDateTime.of(2024, 1, 15, 10, 31)); // ETD: 10:31 AM (1 minute after leg1 ETA - valid)

        routingsResponses.add(leg1);
        routingsResponses.add(leg2);

        List<String> result = routingValidationUtil.validateRoutingLegs(routingsResponses, new HashMap<>());

        assertTrue(result.isEmpty());
    }

    @Test
    void shouldThrowExceptionWhenVoyageLengthExceeds20() {

        RoutingsRequest routing = new RoutingsRequest();
        routing.setVoyage("123456789012345678901");
        routing.setMode(TRANSPORT_MODE_SEA);// 21 chars
        BulkUpdateRoutingsRequest request = new BulkUpdateRoutingsRequest();
        request.setRoutings(List.of(routing));
        ValidationException ex = assertThrows(
                ValidationException.class,
                () -> routingValidationUtil.validateVoyageLengthRequest(request)
        );
        assertThat(ex.getMessage()).isEqualTo("max size is 20 for voyage");
    }

    @Test
    void shouldNotThrowWhenVoyageLengthIsExactly20() {

        RoutingsRequest routing = new RoutingsRequest();
        routing.setMode(TRANSPORT_MODE_SEA);// 21 chars
        routing.setVoyage("12345678901234567890"); // exactly 20 chars
        BulkUpdateRoutingsRequest request = new BulkUpdateRoutingsRequest();
        request.setRoutings(List.of(routing));
        assertDoesNotThrow(() -> routingValidationUtil.validateVoyageLengthRequest(request));
    }

    @Test
    void shouldNotThrowWhenVoyageIsNull() {
        RoutingsRequest routing = new RoutingsRequest();
        routing.setMode(TRANSPORT_MODE_SEA);// 21 chars
        routing.setVoyage(null);
        BulkUpdateRoutingsRequest request = new BulkUpdateRoutingsRequest();
        request.setRoutings(List.of(routing));
        assertDoesNotThrow(() -> routingValidationUtil.validateVoyageLengthRequest(request));
    }

    @Test
    void shouldNotThrowWhenModeIsAir() {
        RoutingsRequest routing = new RoutingsRequest();
        routing.setMode(TRANSPORT_MODE_AIR);// 21 chars
        routing.setVoyage("43243243232423424324");
        BulkUpdateRoutingsRequest request = new BulkUpdateRoutingsRequest();
        request.setRoutings(List.of(routing));
        assertDoesNotThrow(() -> routingValidationUtil.validateVoyageLengthRequest(request));
    }
}
