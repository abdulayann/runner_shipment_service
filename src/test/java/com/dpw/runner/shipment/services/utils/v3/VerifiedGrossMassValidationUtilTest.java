package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.VerifiedGrossMassRequest;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierBookingService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class VerifiedGrossMassValidationUtilTest extends CommonMocks {
    @Mock
    private IConsolidationV3Service consolidationV3Service;

    @Mock
    private ICarrierBookingService carrierBookingService;

    @Mock
    private VerifiedGrossMassRequest verifiedGrossMassRequest;

    @InjectMocks
    private VerifiedGrossMassValidationUtil validationUtil;

    private static final Long VALID_ENTITY_ID = 123L;
    private static final Long INVALID_ENTITY_ID = 999L;


    @Test
    void validateRequest_WithConsolidationEntityType_WhenEntityExists_ShouldReturnEntity() {
        // Arrange
        ConsolidationDetails expectedConsolidation = new ConsolidationDetails();
        when(consolidationV3Service.findById(VALID_ENTITY_ID))
                .thenReturn(Optional.of(expectedConsolidation));

        // Act
        Object result = validationUtil.validateRequest(EntityType.CONSOLIDATION, VALID_ENTITY_ID);

        // Assert
        assertNotNull(result);
        assertEquals(expectedConsolidation, result);
        verify(consolidationV3Service).findById(VALID_ENTITY_ID);
        verifyNoInteractions(carrierBookingService);
    }

    @Test
    void validateRequest_WithConsolidationEntityType_WhenEntityNotExists_ShouldThrowValidationException() {
        // Arrange
        when(consolidationV3Service.findById(INVALID_ENTITY_ID))
                .thenReturn(Optional.empty());

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class,
                () -> validationUtil.validateRequest(EntityType.CONSOLIDATION, INVALID_ENTITY_ID));

        assertEquals("Invalid Consolidation id", exception.getMessage());
        verify(consolidationV3Service).findById(INVALID_ENTITY_ID);
        verifyNoInteractions(carrierBookingService);
    }

    @Test
    void validateRequest_WithCarrierBookingEntityType_WhenEntityExists_ShouldReturnEntity() {
        // Arrange
        CarrierBooking expectedCarrierBooking = new CarrierBooking();
        when(carrierBookingService.findById(VALID_ENTITY_ID))
                .thenReturn(Optional.of(expectedCarrierBooking));

        // Act
        Object result = validationUtil.validateRequest(EntityType.CARRIER_BOOKING, VALID_ENTITY_ID);

        // Assert
        assertNotNull(result);
        assertEquals(expectedCarrierBooking, result);
        verify(carrierBookingService).findById(VALID_ENTITY_ID);
        verifyNoInteractions(consolidationV3Service);
    }

    @Test
    void validateRequest_WithCarrierBookingEntityType_WhenEntityNotExists_ShouldThrowValidationException() {
        // Arrange
        when(carrierBookingService.findById(INVALID_ENTITY_ID))
                .thenReturn(Optional.empty());

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class,
                () -> validationUtil.validateRequest(EntityType.CARRIER_BOOKING, INVALID_ENTITY_ID));

        assertEquals("Invalid Carrier booking id", exception.getMessage());
        verify(carrierBookingService).findById(INVALID_ENTITY_ID);
        verifyNoInteractions(consolidationV3Service);
    }

    @Test
    void validateRequest_WithInvalidEntityType_ShouldThrowValidationException() {
        // Arrange - assuming there's another entity type or null
        EntityType invalidEntityType = null; // or any other EntityType not handled

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class,
                () -> validationUtil.validateRequest(invalidEntityType, VALID_ENTITY_ID));

        assertEquals("Invalid entity Type", exception.getMessage());
        verifyNoInteractions(consolidationV3Service);
        verifyNoInteractions(carrierBookingService);
    }

    @ParameterizedTest
    @ValueSource(strings = {"P2P", "D2D", "D2P", "P2D"})
    void validateServiceType_WithValidServiceType_P2P_ShouldNotThrowException(String serviceType) {
        // Arrange
        when(verifiedGrossMassRequest.getServiceType()).thenReturn(serviceType);

        // Act & Assert
        assertDoesNotThrow(() -> validationUtil.validateServiceType(verifiedGrossMassRequest));
    }

    @ParameterizedTest
    @ValueSource(strings = {"INVALID_TYPE", "", "p2p"})
    void validateServiceType_WithInvalidServiceType_ShouldThrowValidationException(String serviceType) {
        // Arrange
        when(verifiedGrossMassRequest.getServiceType()).thenReturn(serviceType);

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class,
                () -> validationUtil.validateServiceType(verifiedGrossMassRequest));

        assertEquals("Unsupported service type. Please select one of: P2P,D2D,D2P,P2D", exception.getMessage());
    }


    @Test
    void validateServiceType_WithEmptyServiceType_ShouldThrowValidationException() {
        // Arrange
        when(verifiedGrossMassRequest.getServiceType()).thenReturn(null);

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class,
                () -> validationUtil.validateServiceType(verifiedGrossMassRequest));

        assertEquals("Unsupported service type. Please select one of: P2P,D2D,D2P,P2D", exception.getMessage());
    }
}
