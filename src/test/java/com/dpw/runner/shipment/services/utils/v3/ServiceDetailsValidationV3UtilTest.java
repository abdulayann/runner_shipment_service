package com.dpw.runner.shipment.services.utils.v3;


import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.request.ServiceDetailsRequest;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;

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
class ServiceDetailsValidationV3UtilTest {
    @Mock
    private IShipmentServiceV3 shipmentService;

    @Mock
    private IConsolidationService consolidationService;

    @InjectMocks
    private ServiceDetailsValidationV3Util serviceDetailsValidationV3Util;

    private ServiceDetailsRequest request;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        request = new ServiceDetailsRequest();
    }

    @Test
    void validateUpdateBulkRequest_shouldThrowWhenIdNotFound() {
        ServiceDetailsRequest req1 = new ServiceDetailsRequest();
        req1.setId(1L);
        ServiceDetailsRequest req2 = new ServiceDetailsRequest();
        req2.setId(3L); // 3L is missing in DB

        ServiceDetails serviceDetails1 = new ServiceDetails();
        serviceDetails1.setId(1L);
        ServiceDetails serviceDetails2 = new ServiceDetails();
        serviceDetails2.setId(2L);

        List<ServiceDetailsRequest> requests = List.of(req1, req2);
        List<ServiceDetails> existing = List.of(serviceDetails1, serviceDetails2);

        DataRetrievalFailureException ex = assertThrows(DataRetrievalFailureException.class,
                () -> serviceDetailsValidationV3Util.validateUpdateBulkRequest(requests, existing));

        assertTrue(ex.getMessage().contains("No service details found for the ids: [3]"));
    }

    @Test
    void validateUpdateBulkRequest_shouldThrowWhenServiceTypeNotPresent() {
        ServiceDetailsRequest req1 = new ServiceDetailsRequest();
        req1.setId(1L);
        ServiceDetailsRequest req2 = new ServiceDetailsRequest();
        req2.setId(2L);

        ServiceDetails serviceDetails1 = new ServiceDetails();
        serviceDetails1.setId(1L);
        ServiceDetails serviceDetails2 = new ServiceDetails();
        serviceDetails2.setId(2L);

        List<ServiceDetailsRequest> requests = List.of(req1, req2);
        List<ServiceDetails> existing = List.of(serviceDetails1, serviceDetails2);

        ValidationException ex = assertThrows(ValidationException.class,
                () -> serviceDetailsValidationV3Util.validateUpdateBulkRequest(requests, existing));

        assertTrue(ex.getMessage().contains("All service details requests must have service type."));
    }

    @Test
    void validateUpdateBulkRequest_shouldNotThrowWhenAllIdsExistInDB() {
        // Request with ID 1, DB also has ID 1
        ServiceDetailsRequest request1 = new ServiceDetailsRequest();
        request1.setId(1L);
        request1.setServiceType("CLN");

        ServiceDetails dbServiceDetails = new ServiceDetails();
        dbServiceDetails.setId(1L);
        dbServiceDetails.setServiceType("CLN");

        List<ServiceDetailsRequest> requestList = List.of(request1);
        List<ServiceDetails> dbList = List.of(dbServiceDetails);

        assertDoesNotThrow(() -> serviceDetailsValidationV3Util.validateUpdateBulkRequest(requestList, dbList));
    }

    @Test
    void validateDeleteBulkRequest_shouldPassWhenAllHaveIds() {
        ServiceDetailsRequest req1 = new ServiceDetailsRequest();
        req1.setId(5L);
        ServiceDetailsRequest req2 = new ServiceDetailsRequest();
        req2.setId(10L);

        List<ServiceDetailsRequest> requests = List.of(req1, req2);

        assertDoesNotThrow(() -> serviceDetailsValidationV3Util.validateDeleteBulkRequest(requests));
    }

    @Test
    void validateDeleteBulkRequest_shouldThrowWhenAnyIdIsNull() {
        ServiceDetailsRequest req1 = new ServiceDetailsRequest();
        req1.setId(null);
        ServiceDetailsRequest req2 = new ServiceDetailsRequest();
        req2.setId(10L);

        List<ServiceDetailsRequest> requests = List.of(req1, req2);

        ValidationException ex = assertThrows(ValidationException.class,
                () -> serviceDetailsValidationV3Util.validateDeleteBulkRequest(requests));
        assertEquals("All service details delete requests must have a id.", ex.getMessage());
    }

    @Test
    void testValidateModule_validShipmentId() {
        request.setShipmentId(100L);
        when(shipmentService.findById(100L)).thenReturn(Optional.of(new ShipmentDetails()));

        assertDoesNotThrow(() -> serviceDetailsValidationV3Util.validateModule(request, Constants.SHIPMENT));
    }

    @Test
    void testValidateModule_nullShipmentId() {
        request.setShipmentId(null);

        ValidationException ex = assertThrows(ValidationException.class, () ->
                serviceDetailsValidationV3Util.validateModule(request, Constants.SHIPMENT)
        );
        assertEquals("Shipment id is empty", ex.getMessage());
    }

    @Test
    void testValidateModule_invalidShipmentId() {
        request.setShipmentId(999L);
        when(shipmentService.findById(999L)).thenReturn(Optional.empty());

        ValidationException ex = assertThrows(ValidationException.class, () ->
                serviceDetailsValidationV3Util.validateModule(request, Constants.SHIPMENT)
        );
        assertEquals("Please provide the valid shipment id", ex.getMessage());
    }

    @Test
    void testValidateModule_validConsolidationId() {
        request.setConsolidationId(200L);
        when(consolidationService.findById(200L)).thenReturn(Optional.of(new ConsolidationDetails()));

        assertDoesNotThrow(() -> serviceDetailsValidationV3Util.validateModule(request, Constants.CONSOLIDATION));
    }

    @Test
    void testValidateModule_nullConsolidationId() {
        request.setConsolidationId(null);

        ValidationException ex = assertThrows(ValidationException.class, () ->
                serviceDetailsValidationV3Util.validateModule(request, Constants.CONSOLIDATION)
        );
        assertEquals("Consolidation id is empty", ex.getMessage());
    }

    @Test
    void testValidateModule_invalidConsolidationId() {
        request.setConsolidationId(888L);
        when(consolidationService.findById(888L)).thenReturn(Optional.empty());

        ValidationException ex = assertThrows(ValidationException.class, () ->
                serviceDetailsValidationV3Util.validateModule(request, Constants.CONSOLIDATION)
        );
        assertEquals("Please provide the valid consolidation id", ex.getMessage());
    }

    @Test
    void testValidateModule_invalidEntity() {
        assertDoesNotThrow(() -> serviceDetailsValidationV3Util.validateModule(request, null));
    }

    @Test
    void testValidateSameParentId_withNullList_shouldPass() {
        assertThrows(RunnerException.class, () -> serviceDetailsValidationV3Util.validateSameParentId(null, "SHIPMENT"));
    }

    @Test
    void testValidateSameParentId_withEmptyList_shouldPass() {
        assertThrows(RunnerException.class, () -> serviceDetailsValidationV3Util.validateSameParentId(List.of(), "SHIPMENT"));
    }

    @Test
    void testValidateSameParentId_withSameShipmentId_shouldPass() throws RunnerException {
        ServiceDetailsRequest req1 = new ServiceDetailsRequest();
        req1.setShipmentId(100L);

        ServiceDetailsRequest req2 = new ServiceDetailsRequest();
        req2.setShipmentId(100L);

        serviceDetailsValidationV3Util.validateSameParentId(List.of(req1, req2), "SHIPMENT");
    }

    @Test
    void testValidateSameParentId_withDifferentShipmentIds_shouldFail() {
        ServiceDetailsRequest req1 = new ServiceDetailsRequest();
        req1.setShipmentId(100L);

        ServiceDetailsRequest req2 = new ServiceDetailsRequest();
        req2.setShipmentId(101L);

        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () ->
                serviceDetailsValidationV3Util.validateSameParentId(List.of(req1, req2), "SHIPMENT")
        );

        assertTrue(ex.getMessage().contains("same shipmentId"));
    }

    @Test
    void testValidateSameParentId_withDifferentModuleType_shouldFail() {
        ServiceDetailsRequest req = new ServiceDetailsRequest();
        req.setShipmentId(100L);

        assertThatThrownBy(() -> serviceDetailsValidationV3Util.validateSameParentId(List.of(req), "UNKNOWN"))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("Unsupported module type");
    }

    @Test
    void testValidateSameParentId_withDifferentConsolidationIds_shouldFail() {
        ServiceDetailsRequest req1 = new ServiceDetailsRequest();
        req1.setConsolidationId(300L);

        ServiceDetailsRequest req2 = new ServiceDetailsRequest();
        req2.setConsolidationId(301L);

        assertThatThrownBy(() -> serviceDetailsValidationV3Util.validateSameParentId(List.of(req1, req2), "CONSOLIDATION"))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("same consolidationId");
    }

    @Test
    void testValidateSameParentId_withNullIds_shouldPass() {
        ServiceDetailsRequest req1 = new ServiceDetailsRequest();
        ServiceDetailsRequest req2 = new ServiceDetailsRequest();

        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () ->
                serviceDetailsValidationV3Util.validateSameParentId(List.of(req1, req2), "SHIPMENT")
        );

        assertTrue(ex.getMessage().contains("have the shipmentId"));
    }

}
