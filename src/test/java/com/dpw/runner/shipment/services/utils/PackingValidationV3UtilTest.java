package com.dpw.runner.shipment.services.utils;


import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.utils.v3.PackingValidationV3Util;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.Mockito.when;

@SuppressWarnings("java:S5778")
@ExtendWith({MockitoExtension.class})
@Execution(CONCURRENT)
class PackingValidationV3UtilTest {
    @Mock
    private IShipmentServiceV3 shipmentService;

    @Mock
    private IConsolidationService consolidationService;

    @Mock
    private ICustomerBookingService customerBookingService;

    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @InjectMocks
    private PackingValidationV3Util packingValidationV3Util;

    private PackingV3Request request;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        request = new PackingV3Request();
    }

    @Test
    void validateUpdateBulkRequest_shouldThrowWhenIdNotFound() {
        PackingV3Request req1 = new PackingV3Request();
        req1.setId(1L);
        PackingV3Request req2 = new PackingV3Request();
        req2.setId(3L); // 3L is missing in DB

        Packing packing1 = new Packing();
        packing1.setId(1L);
        Packing packing2 = new Packing();
        packing2.setId(2L);

        List<PackingV3Request> requests = List.of(req1, req2);
        List<Packing> existing = List.of(packing1, packing2);

        DataRetrievalFailureException ex = assertThrows(DataRetrievalFailureException.class,
                () -> packingValidationV3Util.validateUpdateBulkRequest(requests, existing));

        assertTrue(ex.getMessage().contains("No packing found for the ids: [3]"));
    }

    @Test
    void validateUpdateRequest_shouldPassWhenIdPresent() {
        PackingV3Request request1 = new PackingV3Request();
        request1.setId(100L);

        assertDoesNotThrow(() -> packingValidationV3Util.validateUpdateRequest(request1));
    }

    @Test
    void validateUpdateRequest_shouldThrowWhenRequestIsNull() {
        RunnerException ex = assertThrows(RunnerException.class, () -> packingValidationV3Util.validateUpdateRequest(null));
        assertEquals("Packing Id cannot be null or empty.", ex.getMessage());
    }

    @Test
    void validateUpdateRequest_shouldThrowWhenIdIsNull() {
        RunnerException ex = assertThrows(RunnerException.class, () -> packingValidationV3Util.validateUpdateRequest(request));
        assertEquals("Packing Id cannot be null or empty.", ex.getMessage());
    }

    @Test
    void validateUpdateBulkRequest_shouldNotThrowWhenAllIdsExistInDB() {
        // Request with ID 1, DB also has ID 1
        PackingV3Request request1 = new PackingV3Request();
        request1.setId(1L);

        Packing dbPacking = new Packing();
        dbPacking.setId(1L);

        List<PackingV3Request> requestList = List.of(request1);
        List<Packing> dbList = List.of(dbPacking);

        assertDoesNotThrow(() -> packingValidationV3Util.validateUpdateBulkRequest(requestList, dbList));
    }

    @Test
    void validateDeleteBulkRequest_shouldPassWhenAllHaveIds() {
        PackingV3Request req1 = new PackingV3Request();
        req1.setId(5L);
        PackingV3Request req2 = new PackingV3Request();
        req2.setId(10L);

        List<PackingV3Request> requests = List.of(req1, req2);

        assertDoesNotThrow(() -> packingValidationV3Util.validateDeleteBulkRequest(requests));
    }

    @Test
    void validateDeleteBulkRequest_shouldThrowWhenAnyIdIsNull() {
        PackingV3Request req1 = new PackingV3Request();
        req1.setId(null);
        PackingV3Request req2 = new PackingV3Request();
        req2.setId(10L);

        List<PackingV3Request> requests = List.of(req1, req2);

        DataRetrievalFailureException ex = assertThrows(DataRetrievalFailureException.class,
                () -> packingValidationV3Util.validateDeleteBulkRequest(requests));
        assertEquals("All packing delete requests must have a id.", ex.getMessage());
    }

    @Test
    void testValidateModule_validShipmentId() {
        request.setShipmentId(100L);
        when(shipmentService.findById(100L)).thenReturn(Optional.of(new ShipmentDetails()));

        assertDoesNotThrow(() -> packingValidationV3Util.validateModule(request, Constants.SHIPMENT));
    }

    @Test
    void testValidateModule_nullShipmentId() {
        request.setShipmentId(null);

        ValidationException ex = assertThrows(ValidationException.class, () ->
                packingValidationV3Util.validateModule(request, Constants.SHIPMENT)
        );
        assertEquals("Shipment id is empty", ex.getMessage());
    }

    @Test
    void testValidateModule_invalidShipmentId() {
        request.setShipmentId(999L);
        when(shipmentService.findById(999L)).thenReturn(Optional.empty());

        ValidationException ex = assertThrows(ValidationException.class, () ->
                packingValidationV3Util.validateModule(request, Constants.SHIPMENT)
        );
        assertEquals("Please provide the valid shipment id", ex.getMessage());
    }

    @Test
    void testValidateModule_validConsolidationId() {
        request.setConsolidationId(200L);
        when(consolidationService.findById(200L)).thenReturn(Optional.of(new ConsolidationDetails()));

        assertDoesNotThrow(() -> packingValidationV3Util.validateModule(request, Constants.CONSOLIDATION));
    }

    @Test
    void testValidateModule_nullConsolidationId() {
        request.setConsolidationId(null);

        ValidationException ex = assertThrows(ValidationException.class, () ->
                packingValidationV3Util.validateModule(request, Constants.CONSOLIDATION)
        );
        assertEquals("Consolidation id is empty", ex.getMessage());
    }

    @Test
    void testValidateModule_invalidConsolidationId() {
        request.setConsolidationId(888L);
        when(consolidationService.findById(888L)).thenReturn(Optional.empty());

        ValidationException ex = assertThrows(ValidationException.class, () ->
                packingValidationV3Util.validateModule(request, Constants.CONSOLIDATION)
        );
        assertEquals("Please provide the valid consolidation id", ex.getMessage());
    }

    @Test
    void testValidateModule_validBookingId() {
        request.setBookingId(300L);
        when(customerBookingService.findById(300L)).thenReturn(Optional.of(new CustomerBooking()));

        assertDoesNotThrow(() -> packingValidationV3Util.validateModule(request, Constants.BOOKING));
    }

    @Test
    void testValidateModule_nullBookingId() {
        request.setBookingId(null);

        ValidationException ex = assertThrows(ValidationException.class, () ->
                packingValidationV3Util.validateModule(request, Constants.BOOKING)
        );
        assertEquals("Booking id is empty", ex.getMessage());
    }

    @Test
    void testValidateModule_invalidBookingId() {
        request.setBookingId(404L);
        when(customerBookingService.findById(404L)).thenReturn(Optional.empty());

        ValidationException ex = assertThrows(ValidationException.class, () ->
                packingValidationV3Util.validateModule(request, Constants.BOOKING)
        );
        assertEquals("Please provide the valid booking id", ex.getMessage());
    }

    @Test
    void testValidateModule_invalidEntity() {
        assertDoesNotThrow(() -> packingValidationV3Util.validateModule(request, null));
    }

    @Test
    void testValidateSameParentId_withNullList_shouldPass() {
        packingValidationV3Util.validateSameParentId(null, "SHIPMENT");
    }

    @Test
    void testValidateSameParentId_withEmptyList_shouldPass() {
        packingValidationV3Util.validateSameParentId(List.of(), "SHIPMENT");
    }

    @Test
    void testValidateSameParentId_withSameShipmentId_shouldPass() {
        PackingV3Request req1 = new PackingV3Request();
        req1.setShipmentId(100L);

        PackingV3Request req2 = new PackingV3Request();
        req2.setShipmentId(100L);

        packingValidationV3Util.validateSameParentId(List.of(req1, req2), "SHIPMENT");
    }

    @Test
    void testValidateSameParentId_withDifferentShipmentIds_shouldFail() {
        PackingV3Request req1 = new PackingV3Request();
        req1.setShipmentId(100L);

        PackingV3Request req2 = new PackingV3Request();
        req2.setShipmentId(101L);

        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () ->
                packingValidationV3Util.validateSameParentId(List.of(req1, req2), "SHIPMENT")
        );

        assertTrue(ex.getMessage().contains("same shipmentId"));
    }

    @Test
    void testValidateSameParentId_withDifferentModuleType_shouldFail() {
        PackingV3Request req = new PackingV3Request();
        req.setShipmentId(100L);

        assertThatThrownBy(() -> packingValidationV3Util.validateSameParentId(List.of(req), "UNKNOWN"))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("Unsupported module type");
    }

    @Test
    void testValidateSameParentId_withSameBookingId_shouldPass() {
        PackingV3Request req1 = new PackingV3Request();
        req1.setBookingId(200L);

        PackingV3Request req2 = new PackingV3Request();
        req2.setBookingId(200L);

        packingValidationV3Util.validateSameParentId(List.of(req1, req2), "BOOKING");
    }

    @Test
    void testValidateSameParentId_withDifferentConsolidationIds_shouldFail() {
        PackingV3Request req1 = new PackingV3Request();
        req1.setConsolidationId(300L);

        PackingV3Request req2 = new PackingV3Request();
        req2.setConsolidationId(301L);

        assertThatThrownBy(() -> packingValidationV3Util.validateSameParentId(List.of(req1, req2), "CONSOLIDATION"))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("same consolidationId");
    }

    @Test
    void testValidateSameParentId_withNullIds_shouldPass() {
        PackingV3Request req1 = new PackingV3Request();
        PackingV3Request req2 = new PackingV3Request();

        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () ->
                packingValidationV3Util.validateSameParentId(List.of(req1, req2), "SHIPMENT")
        );

        assertTrue(ex.getMessage().contains("have the shipmentId"));
    }

    @Test
    void testShipmentGateInDateIsNull_shouldNotThrow() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setShipmentGateInDate(null);

        assertDoesNotThrow(() -> packingValidationV3Util.validateShipmentGateInDate(shipmentDetails));
    }

    @Test
    void testShipmentGateInAfterCfsCutOff_shouldThrow() {
        LocalDateTime now = LocalDateTime.now();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentGateInDate(now.plusDays(5));

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setEtd(now.plusDays(10));
        shipmentDetails.setCarrierDetails(carrierDetails);

        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setConsolidationId(100L);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCfsCutOffDate(now);

        when(consoleShipmentMappingDao.findByShipmentId(1L)).thenReturn(List.of(mapping));
        when(consolidationService.findById(100L)).thenReturn(Optional.of(consolidationDetails));

        RunnerException ex = assertThrows(RunnerException.class, () ->
                packingValidationV3Util.validateShipmentGateInDate(shipmentDetails)
        );

        assertEquals("Shipment Gate In date should not be greater than the CFS Cut Off Date entered at the consolidation level.", ex.getMessage());
    }

    @Test
    void testShipmentGateInAfterEtd_shouldThrow() {
        LocalDateTime now = LocalDateTime.now();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentGateInDate(now.plusDays(7));

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setEtd(now.plusDays(5));
        shipmentDetails.setCarrierDetails(carrierDetails);

        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setConsolidationId(200L);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCfsCutOffDate(null); // null to skip this check

        when(consoleShipmentMappingDao.findByShipmentId(1L)).thenReturn(List.of(mapping));
        when(consolidationService.findById(200L)).thenReturn(Optional.of(consolidationDetails));

        RunnerException ex = assertThrows(RunnerException.class, () ->
                packingValidationV3Util.validateShipmentGateInDate(shipmentDetails)
        );

        assertEquals("Shipment Gate In Date cannot be greater than ETD.", ex.getMessage());
    }

    @Test
    void testValidShipmentGateInDate_shouldNotThrow() {
        LocalDateTime now = LocalDateTime.now();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentGateInDate(now);

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setEtd(now.plusDays(5));
        shipmentDetails.setCarrierDetails(carrierDetails);

        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setConsolidationId(300L);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCfsCutOffDate(now.plusDays(1));

        when(consoleShipmentMappingDao.findByShipmentId(1L)).thenReturn(List.of(mapping));
        when(consolidationService.findById(300L)).thenReturn(Optional.of(consolidationDetails));

        assertDoesNotThrow(() -> packingValidationV3Util.validateShipmentGateInDate(shipmentDetails));
    }

    @Test
    void testConsoleShipmentMappingEmpty_shouldNotThrow() {
        LocalDateTime now = LocalDateTime.now();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentGateInDate(now);

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setEtd(now.plusDays(5));
        shipmentDetails.setCarrierDetails(carrierDetails);

        when(consoleShipmentMappingDao.findByShipmentId(1L)).thenReturn(Collections.emptyList());

        assertDoesNotThrow(() -> packingValidationV3Util.validateShipmentGateInDate(shipmentDetails));
    }

    @Test
    void testConsolidationOptionalEmpty_shouldNotThrow() {
        LocalDateTime now = LocalDateTime.now();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentGateInDate(now);

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setEtd(now.plusDays(5));
        shipmentDetails.setCarrierDetails(carrierDetails);

        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setConsolidationId(123L);

        when(consoleShipmentMappingDao.findByShipmentId(1L)).thenReturn(List.of(mapping));
        when(consolidationService.findById(123L)).thenReturn(Optional.empty());

        assertDoesNotThrow(() -> packingValidationV3Util.validateShipmentGateInDate(shipmentDetails));
    }


}
