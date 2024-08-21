package com.dpw.runner.shipment.services.validator.custom.validations;

import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.junit.jupiter.MockitoExtension;

import javax.validation.ConstraintValidatorContext;

import java.time.LocalDateTime;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CargoDeliveryDateValidatorTest {

    private CargoDeliveryDateValidator validator;
    private ConstraintValidatorContext context;
    private ShipmentRequest shipment;

    @BeforeEach
    void setUp() {
        validator = new CargoDeliveryDateValidator();
        context = mock(ConstraintValidatorContext.class);
        shipment = new ShipmentRequest();
    }

    @Test
    void isValid_whenCargoDeliveryDateIsBeforeCargoReadyDate_shouldReturnFalse() {
        // Arrange
        LocalDateTime cargoReadyDate = LocalDateTime.now().plusDays(1);
        LocalDateTime cargoDeliveryDate = LocalDateTime.now();
        shipment.setCargoReadyDate(cargoReadyDate);
        shipment.setCargoDeliveryDate(cargoDeliveryDate);

        ConstraintValidatorContext.ConstraintViolationBuilder builder = mock(ConstraintValidatorContext.ConstraintViolationBuilder.class);
        ConstraintValidatorContext.ConstraintViolationBuilder.NodeBuilderCustomizableContext nodeBuilder = mock(ConstraintValidatorContext.ConstraintViolationBuilder.NodeBuilderCustomizableContext.class);

        when(context.buildConstraintViolationWithTemplate(anyString())).thenReturn(builder);
        when(builder.addPropertyNode(anyString())).thenReturn(nodeBuilder);
        when(nodeBuilder.addConstraintViolation()).thenReturn(context);

        // Act
        boolean result = validator.isValid(shipment, context);

        // Assert
        assertFalse(result);
        verify(context).disableDefaultConstraintViolation();
        verify(context).buildConstraintViolationWithTemplate("Cargo Delivery Date should not be lesser than Cargo Ready Date.");
        verify(builder).addPropertyNode("cargoDeliveryDate");
        verify(nodeBuilder).addConstraintViolation();
    }

    @Test
    void isValid_whenCargoDeliveryDateIsAfterCargoReadyDate_shouldReturnTrue() {
        // Arrange
        LocalDateTime cargoReadyDate = LocalDateTime.now();
        LocalDateTime cargoDeliveryDate = LocalDateTime.now().plusDays(1);
        shipment.setCargoReadyDate(cargoReadyDate);
        shipment.setCargoDeliveryDate(cargoDeliveryDate);

        // Act
        boolean result = validator.isValid(shipment, context);

        // Assert
        assertTrue(result);
        verify(context, never()).disableDefaultConstraintViolation();
        verify(context, never()).buildConstraintViolationWithTemplate(anyString());
    }

    @Test
    void isValid_whenBothDatesAreNull_shouldReturnTrue() {
        // Arrange
        shipment.setCargoReadyDate(null);
        shipment.setCargoDeliveryDate(null);

        // Act
        boolean result = validator.isValid(shipment, context);

        // Assert
        assertTrue(result);
        verify(context, never()).disableDefaultConstraintViolation();
        verify(context, never()).buildConstraintViolationWithTemplate(anyString());
    }

    @Test
    void isValid_whenCargoReadyDateIsNullAndCargoDeliveryDateIsNotNull_shouldReturnTrue() {
        // Arrange
        shipment.setCargoReadyDate(null);
        shipment.setCargoDeliveryDate(LocalDateTime.now());

        // Act
        boolean result = validator.isValid(shipment, context);

        // Assert
        assertTrue(result);
        verify(context, never()).disableDefaultConstraintViolation();
        verify(context, never()).buildConstraintViolationWithTemplate(anyString());
    }

    @Test
    void isValid_whenCargoReadyDateIsNotNullAndCargoDeliveryDateIsNull_shouldReturnTrue() {
        // Arrange
        shipment.setCargoReadyDate(LocalDateTime.now());
        shipment.setCargoDeliveryDate(null);

        // Act
        boolean result = validator.isValid(shipment, context);

        // Assert
        assertTrue(result);
        verify(context, never()).disableDefaultConstraintViolation();
        verify(context, never()).buildConstraintViolationWithTemplate(anyString());
    }
}