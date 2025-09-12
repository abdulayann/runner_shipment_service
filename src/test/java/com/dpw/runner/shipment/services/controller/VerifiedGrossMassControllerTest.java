package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassBulkUpdateRequest;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.impl.VerifiedGrossMassService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class VerifiedGrossMassControllerTest {

    @InjectMocks
    private VerifiedGrossMassController controller;

    @Mock
    private VerifiedGrossMassService verifiedGrossMassService;

    @Test
    void bulkUpdateContainers_AllScenarios() {
        // --- Success case ---
        VerifiedGrossMassBulkUpdateRequest successRequest = new VerifiedGrossMassBulkUpdateRequest();
        successRequest.setContainerIds(Arrays.asList(1L, 2L));

        List<CommonContainerResponse> serviceResponse = Arrays.asList(
                new CommonContainerResponse(), new CommonContainerResponse()
        );
        when(verifiedGrossMassService.bulkUpdateContainers(successRequest))
                .thenReturn(serviceResponse);
        ResponseEntity<IRunnerResponse> successResponse =
                controller.bulkUpdateContainers(successRequest);
        assertEquals(HttpStatus.OK, successResponse.getStatusCode());
        assertNotNull(successResponse.getBody());
        // --- Validation exception case ---
        VerifiedGrossMassBulkUpdateRequest invalidRequest = new VerifiedGrossMassBulkUpdateRequest();
        invalidRequest.setContainerIds(Arrays.asList(1L)); // only 1 container
        when(verifiedGrossMassService.bulkUpdateContainers(invalidRequest))
                .thenThrow(new ValidationException("At least 2 containers must be selected for bulk update"));
        ResponseEntity<IRunnerResponse> validationResponse =
                controller.bulkUpdateContainers(invalidRequest);
        assertEquals(HttpStatus.BAD_REQUEST, validationResponse.getStatusCode());
        assertNotNull(validationResponse.getBody());

        // --- Unexpected exception case ---
        VerifiedGrossMassBulkUpdateRequest errorRequest = new VerifiedGrossMassBulkUpdateRequest();
        errorRequest.setContainerIds(Arrays.asList(3L, 4L));

        when(verifiedGrossMassService.bulkUpdateContainers(errorRequest))
                .thenThrow(new RuntimeException("Unexpected error"));

        ResponseEntity<IRunnerResponse> errorResponse =
                controller.bulkUpdateContainers(errorRequest);

        assertEquals(HttpStatus.BAD_REQUEST, errorResponse.getStatusCode());
        assertNotNull(errorResponse.getBody());
    }
}

