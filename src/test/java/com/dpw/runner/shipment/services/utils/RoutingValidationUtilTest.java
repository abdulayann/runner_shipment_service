package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.request.BulkUpdateRoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.Mockito.*;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class RoutingValidationUtilTest {
    @Mock
    private IShipmentServiceV3 shipmentService;
    @Mock
    private IConsolidationService consolidationService;
    @Mock
    private ICustomerBookingService customerBookingService;
    @InjectMocks
    private RoutingValidationUtil routingValidationUtil;

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


}