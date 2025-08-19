package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class ContainerV3FacadeServiceTest {
    @Mock
    private ContainerV3Service containerV3Service;

    @InjectMocks
    private ContainerV3FacadeService containerV3FacadeService; // Replace with your actual class name


    @Test
    void testValidContainerNumber_AppendsLastDigit() {
        // Arrange
        ContainerV3Request request = new ContainerV3Request();
        request.setContainerNumber("ABC123");

        ContainerNumberCheckResponse response = new ContainerNumberCheckResponse();
        response.setSuccess(true);
        response.setLastDigit(4);

        when(containerV3Service.validateContainerNumber("ABC123")).thenReturn(response);

        List<ContainerV3Request> requestList = new ArrayList<>();
        requestList.add(request);

        // Act
        containerV3FacadeService.validateContainerNumberFormat(requestList);

        // Assert
        assertEquals("ABC1234", request.getContainerNumber());
        verify(containerV3Service, times(1)).validateContainerNumber("ABC123");
    }

    @Test
    void testInvalidContainerNumber_ThrowsException() {
        // Arrange
        ContainerV3Request request = new ContainerV3Request();
        request.setContainerNumber("INVALID");

        ContainerNumberCheckResponse response = new ContainerNumberCheckResponse();
        response.setSuccess(false);

        when(containerV3Service.validateContainerNumber("INVALID")).thenReturn(response);

        List<ContainerV3Request> requestList = Arrays.asList(request);

        // Act & Assert
        ValidationException ex = assertThrows(ValidationException.class,
                () -> containerV3FacadeService.validateContainerNumberFormat(requestList));
        assertEquals("Invalid container number format", ex.getMessage());

        verify(containerV3Service, times(1)).validateContainerNumber("INVALID");
    }

    @Test
    void testEmptyOrNullList_NoInteractionWithService() {
        // Null case
        containerV3FacadeService.validateContainerNumberFormat(null);
        verifyNoInteractions(containerV3Service);

        // Empty list case
        containerV3FacadeService.validateContainerNumberFormat(new ArrayList<>());
        verifyNoInteractions(containerV3Service);
    }

    @Test
    void testContainerNumberEmpty_SkipsValidation() {
        ContainerV3Request request = new ContainerV3Request();
        request.setContainerNumber(""); // Empty string

        List<ContainerV3Request> requestList = List.of(request);

        containerV3FacadeService.validateContainerNumberFormat(requestList);

        verifyNoInteractions(containerV3Service);
    }
}
