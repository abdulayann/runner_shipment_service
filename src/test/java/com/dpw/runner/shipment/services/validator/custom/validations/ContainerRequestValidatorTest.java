package com.dpw.runner.shipment.services.validator.custom.validations;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsContainersRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import jakarta.validation.ConstraintValidatorContext;
import jakarta.validation.ConstraintValidatorContext.ConstraintViolationBuilder;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ContainerRequestValidatorTest {

    @Mock
    private ConstraintValidatorContext context;

    @Mock
    private ConstraintViolationBuilder violationBuilder;

    @Mock
    private ConstraintViolationBuilder.NodeBuilderCustomizableContext nodeBuilder;

    private ContainerRequestValidator validator;

    @BeforeEach
    void setUp() {
        validator = new ContainerRequestValidator();
    }

    @Test
    void testIsValid_WithNullRequest_ShouldReturnTrue() {
        // Null validation should be handled by @NotNull annotation
        boolean result = validator.isValid(null, context);

        assertTrue(result);
        verifyNoInteractions(context);
    }

    @Test
    void testIsValid_SeaMode_DangerousTrue_AllFieldsPresent_ShouldReturnTrue() {
        // Given
        TransportInstructionLegsContainersRequest request = createContainerRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setDangerous(true);
        request.setDgClass("Class 3");
        request.setUnNumber("UN1234");
        request.setProperShippingName("Flammable Liquid");

        // When
        boolean result = validator.isValid(request, context);

        // Then
        assertTrue(result);
        verify(context).disableDefaultConstraintViolation();
        verifyNoMoreInteractions(context);
    }

    @Test
    void testIsValid_SeaMode_DangerousTrue_MissingDgClass_ShouldReturnFalse() {
        // Setup mock chain
        when(context.buildConstraintViolationWithTemplate(anyString())).thenReturn(violationBuilder);
        when(violationBuilder.addPropertyNode(anyString())).thenReturn(nodeBuilder);
        when(nodeBuilder.addConstraintViolation()).thenReturn(context);

        // Given
        TransportInstructionLegsContainersRequest request = createContainerRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setDangerous(true);
        request.setDgClass(null);
        request.setUnNumber("UN1234");
        request.setProperShippingName("Flammable Liquid");

        // When
        boolean result = validator.isValid(request, context);

        // Then
        assertFalse(result);
        verify(context).disableDefaultConstraintViolation();
        verify(context).buildConstraintViolationWithTemplate("DG Class is mandatory when DG is enabled");
        verify(violationBuilder).addPropertyNode("dgClass");
        verify(nodeBuilder).addConstraintViolation();
    }

    @Test
    void testIsValid_SeaMode_DangerousTrue_EmptyDgClass_ShouldReturnFalse() {
        // Setup mock chain
        when(context.buildConstraintViolationWithTemplate(anyString())).thenReturn(violationBuilder);
        when(violationBuilder.addPropertyNode(anyString())).thenReturn(nodeBuilder);
        when(nodeBuilder.addConstraintViolation()).thenReturn(context);
        // Given
        TransportInstructionLegsContainersRequest request = createContainerRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setDangerous(true);
        request.setDgClass("");
        request.setUnNumber("UN1234");
        request.setProperShippingName("Flammable Liquid");

        // When
        boolean result = validator.isValid(request, context);

        // Then
        assertFalse(result);
        verify(context).buildConstraintViolationWithTemplate("DG Class is mandatory when DG is enabled");
    }

    @Test
    void testIsValid_SeaMode_DangerousTrue_WhitespaceDgClass_ShouldReturnFalse() {
        // Setup mock chain
        when(context.buildConstraintViolationWithTemplate(anyString())).thenReturn(violationBuilder);
        when(violationBuilder.addPropertyNode(anyString())).thenReturn(nodeBuilder);
        when(nodeBuilder.addConstraintViolation()).thenReturn(context);
        // Given
        TransportInstructionLegsContainersRequest request = createContainerRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setDangerous(true);
        request.setDgClass("   ");
        request.setUnNumber("UN1234");
        request.setProperShippingName("Flammable Liquid");

        // When
        boolean result = validator.isValid(request, context);

        // Then
        assertFalse(result);
        verify(context).buildConstraintViolationWithTemplate("DG Class is mandatory when DG is enabled");
    }

    @Test
    void testIsValid_SeaMode_DangerousTrue_MissingUnNumber_ShouldReturnFalse() {
        // Setup mock chain
        when(context.buildConstraintViolationWithTemplate(anyString())).thenReturn(violationBuilder);
        when(violationBuilder.addPropertyNode(anyString())).thenReturn(nodeBuilder);
        when(nodeBuilder.addConstraintViolation()).thenReturn(context);
        // Given
        TransportInstructionLegsContainersRequest request = createContainerRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setDangerous(true);
        request.setDgClass("Class 3");
        request.setUnNumber(null);
        request.setProperShippingName("Flammable Liquid");

        // When
        boolean result = validator.isValid(request, context);

        // Then
        assertFalse(result);
        verify(context).buildConstraintViolationWithTemplate("UN Number is mandatory when DG is enabled");
        verify(violationBuilder).addPropertyNode("unNumber");
    }

    @Test
    void testIsValid_SeaMode_DangerousTrue_MissingProperShippingName_ShouldReturnFalse() {
        // Setup mock chain
        when(context.buildConstraintViolationWithTemplate(anyString())).thenReturn(violationBuilder);
        when(violationBuilder.addPropertyNode(anyString())).thenReturn(nodeBuilder);
        when(nodeBuilder.addConstraintViolation()).thenReturn(context);
        // Given
        TransportInstructionLegsContainersRequest request = createContainerRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setDangerous(true);
        request.setDgClass("Class 3");
        request.setUnNumber("UN1234");
        request.setProperShippingName(null);

        // When
        boolean result = validator.isValid(request, context);

        // Then
        assertFalse(result);
        verify(context).buildConstraintViolationWithTemplate("Proper Shipping Name is mandatory when DG is enabled");
        verify(violationBuilder).addPropertyNode("properShippingName");
    }

    @Test
    void testIsValid_SeaMode_DangerousTrue_AllFieldsMissing_ShouldReturnFalse() {
        // Setup mock chain
        when(context.buildConstraintViolationWithTemplate(anyString())).thenReturn(violationBuilder);
        when(violationBuilder.addPropertyNode(anyString())).thenReturn(nodeBuilder);
        when(nodeBuilder.addConstraintViolation()).thenReturn(context);
        // Setup mock chain
        when(context.buildConstraintViolationWithTemplate(anyString())).thenReturn(violationBuilder);
        when(violationBuilder.addPropertyNode(anyString())).thenReturn(nodeBuilder);
        when(nodeBuilder.addConstraintViolation()).thenReturn(context);
        // Given
        TransportInstructionLegsContainersRequest request = createContainerRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setDangerous(true);
        request.setDgClass(null);
        request.setUnNumber(null);
        request.setProperShippingName(null);

        // When
        boolean result = validator.isValid(request, context);

        // Then
        assertFalse(result);
        verify(context, times(3)).buildConstraintViolationWithTemplate(anyString());
    }

    @Test
    void testIsValid_SeaMode_DangerousFalse_ShouldReturnTrue() {
        // Given
        TransportInstructionLegsContainersRequest request = createContainerRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setDangerous(false);

        // When
        boolean result = validator.isValid(request, context);

        // Then
        assertTrue(result);
        verify(context).disableDefaultConstraintViolation();
        verifyNoMoreInteractions(context);
    }

    @Test
    void testIsValid_SeaMode_DangerousNull_ShouldReturnTrue() {
        // Given
        TransportInstructionLegsContainersRequest request = createContainerRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setDangerous(null);

        // When
        boolean result = validator.isValid(request, context);

        // Then
        assertTrue(result);
        verify(context).disableDefaultConstraintViolation();
        verifyNoMoreInteractions(context);
    }

    @Test
    void testIsValid_AirMode_DangerousTrue_AllFieldsPresent_ShouldReturnTrue() {
        // Given
        TransportInstructionLegsContainersRequest request = createContainerRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        request.setDangerous(true);
        request.setDgClass("Class 9");
        request.setUnNumber("UN3480");

        // When
        boolean result = validator.isValid(request, context);

        // Then
        assertTrue(result);
        verify(context).disableDefaultConstraintViolation();
        verifyNoMoreInteractions(context);
    }

    @Test
    void testIsValid_AirMode_DangerousTrue_MissingDgClass_ShouldReturnFalse() {
        // Setup mock chain
        when(context.buildConstraintViolationWithTemplate(anyString())).thenReturn(violationBuilder);
        when(violationBuilder.addPropertyNode(anyString())).thenReturn(nodeBuilder);
        when(nodeBuilder.addConstraintViolation()).thenReturn(context);
        // Given
        TransportInstructionLegsContainersRequest request = createContainerRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        request.setDangerous(true);
        request.setDgClass(null);
        request.setUnNumber("UN3480");

        // When
        boolean result = validator.isValid(request, context);

        // Then
        assertFalse(result);
        verify(context).buildConstraintViolationWithTemplate("DG Class is mandatory when DG is enabled");
        verify(violationBuilder).addPropertyNode("dgClass");
    }

    @Test
    void testIsValid_AirMode_DangerousTrue_MissingUnNumber_ShouldReturnFalse() {
        // Setup mock chain
        when(context.buildConstraintViolationWithTemplate(anyString())).thenReturn(violationBuilder);
        when(violationBuilder.addPropertyNode(anyString())).thenReturn(nodeBuilder);
        when(nodeBuilder.addConstraintViolation()).thenReturn(context);
        // Given
        TransportInstructionLegsContainersRequest request = createContainerRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        request.setDangerous(true);
        request.setDgClass("Class 9");
        request.setUnNumber(null);

        // When
        boolean result = validator.isValid(request, context);

        // Then
        assertFalse(result);
        verify(context).buildConstraintViolationWithTemplate("UN Number is mandatory when DG is enabled");
        verify(violationBuilder).addPropertyNode("unNumber");
    }

    @Test
    void testIsValid_AirMode_DangerousTrue_AllFieldsMissing_ShouldReturnFalse() {
        // Setup mock chain
        when(context.buildConstraintViolationWithTemplate(anyString())).thenReturn(violationBuilder);
        when(violationBuilder.addPropertyNode(anyString())).thenReturn(nodeBuilder);
        when(nodeBuilder.addConstraintViolation()).thenReturn(context);
        // Given
        TransportInstructionLegsContainersRequest request = createContainerRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        request.setDangerous(true);
        request.setDgClass(null);
        request.setUnNumber(null);

        // When
        boolean result = validator.isValid(request, context);

        // Then
        assertFalse(result);
        verify(context, times(2)).buildConstraintViolationWithTemplate(anyString());
    }

    @Test
    void testIsValid_AirMode_DangerousFalse_ShouldReturnTrue() {
        // Given
        TransportInstructionLegsContainersRequest request = createContainerRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        request.setDangerous(false);

        // When
        boolean result = validator.isValid(request, context);

        // Then
        assertTrue(result);
        verify(context).disableDefaultConstraintViolation();
        verifyNoMoreInteractions(context);
    }

    @Test
    void testIsValid_UnsupportedMode_ShouldReturnTrue() {
        // Given
        TransportInstructionLegsContainersRequest request = createContainerRequest();
        request.setTransportMode("RAIL");
        request.setDangerous(true);

        // When
        boolean result = validator.isValid(request, context);

        // Then
        assertTrue(result);
        verify(context).disableDefaultConstraintViolation();
        verifyNoMoreInteractions(context);
    }

    @Test
    void testIsValid_CaseInsensitiveMode_ShouldWork() {
        // Given
        TransportInstructionLegsContainersRequest request = createContainerRequest();
        request.setTransportMode("AIR"); // uppercase
        request.setDangerous(true);
        request.setDgClass("Class 9");
        request.setUnNumber("UN3480");

        // When
        boolean result = validator.isValid(request, context);

        // Then
        assertTrue(result);
    }

    private TransportInstructionLegsContainersRequest createContainerRequest() {
        return new TransportInstructionLegsContainersRequest();
    }
}
