package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.VerifiedGrossMassRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassBulkUpdateRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassResponse;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class VerifiedGrossMassControllerTest {

    @InjectMocks
    private VerifiedGrossMassController controller;

    @Mock
    private VerifiedGrossMassService verifiedGrossMassService;

    @Mock
    private JsonHelper jsonHelper;

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

    @Test
    void createVgm() {
        VerifiedGrossMassResponse response = new VerifiedGrossMassResponse();
        when(verifiedGrossMassService.create(any())).thenReturn(response);
        ResponseEntity<IRunnerResponse> responseResponseEntity = controller.create(new VerifiedGrossMassRequest());
        assertEquals(responseResponseEntity.getStatusCodeValue(), HttpStatus.OK.value());
    }

    @Test
    void updateVgm() {
        VerifiedGrossMassResponse response = new VerifiedGrossMassResponse();
        when(verifiedGrossMassService.update(any())).thenReturn(response);
        ResponseEntity<IRunnerResponse> responseResponseEntity = controller.update(new VerifiedGrossMassRequest());
        assertEquals(responseResponseEntity.getStatusCodeValue(), HttpStatus.OK.value());
    }

    @Test
    void retrieveVgm() {
        VerifiedGrossMassResponse response = new VerifiedGrossMassResponse();
        when(verifiedGrossMassService.retrieveById(any())).thenReturn(response);
        ResponseEntity<IRunnerResponse> responseResponseEntity = controller.retrieveById(1L);
        assertEquals(responseResponseEntity.getStatusCodeValue(), HttpStatus.OK.value());
    }

    @Test
    void deleteVgm() {
        doNothing().when(verifiedGrossMassService).delete(anyLong());
        ResponseEntity<IRunnerResponse> responseResponseEntity = controller.delete(1L);
        assertEquals(responseResponseEntity.getStatusCodeValue(), HttpStatus.OK.value());
    }

    @Test
    void getDefaultVgm() {
        VerifiedGrossMassResponse response = new VerifiedGrossMassResponse();
        when(verifiedGrossMassService.getDefaultVerifiedGrossMassValues(any(), anyLong())).thenReturn(response);
        ResponseEntity<IRunnerResponse> responseResponseEntity = controller.getDefault(1L, EntityType.CONSOLIDATION);
        assertEquals(responseResponseEntity.getStatusCodeValue(), HttpStatus.OK.value());
    }

    @Test
    void getAllMasterDataVgm() {
        VerifiedGrossMassResponse response = new VerifiedGrossMassResponse();
        when(verifiedGrossMassService.getAllMasterData(anyLong())).thenReturn((ResponseEntity.ok(response)));
        ResponseEntity<?> responseResponseEntity = controller.getAllMasterData(1L);
        assertEquals(responseResponseEntity.getStatusCodeValue(), HttpStatus.OK.value());
    }

    @Test
    void vgmList() {
        VerifiedGrossMassResponse response = new VerifiedGrossMassResponse();
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        when(verifiedGrossMassService.list(any(), anyBoolean())).thenReturn((ResponseEntity.ok(response)));
        ResponseEntity<?> responseResponseEntity = controller.list(listCommonRequest, false);
        assertEquals(responseResponseEntity.getStatusCodeValue(), HttpStatus.OK.value());
    }
}

