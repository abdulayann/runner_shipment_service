package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.ShippingInstructionRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShippingInstructionsService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ContextConfiguration(classes = {MasterDataController.class})
@ExtendWith(MockitoExtension.class)
class ShippingInstructionControllerTest {

    @Mock
    private IShippingInstructionsService service;

    @Mock
    private JsonHelper jsonHelper; // if controller depends on it

    @InjectMocks
    private ShippingInstructionsController controller;

    @Test
    void testCreateFailure_unit() {
        ShippingInstructionRequest request = new ShippingInstructionRequest();

        when(service.createShippingInstruction(any(ShippingInstructionRequest.class)))
                .thenThrow(new RuntimeException("Create failed"));

        ResponseEntity<IRunnerResponse> result = controller.create(request);

        assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
        assertNotNull(result.getBody());

        verify(service, times(1)).createShippingInstruction(any(ShippingInstructionRequest.class));
    }

    @Test
    void testCreateSuccess() {
        ShippingInstructionRequest request = new ShippingInstructionRequest();
        ShippingInstructionResponse response = new ShippingInstructionResponse();

        when(service.createShippingInstruction(any(ShippingInstructionRequest.class)))
                .thenReturn(response);

        ResponseEntity<?> result = controller.create(request);

        assertEquals(HttpStatus.OK, result.getStatusCode());
        assertNotNull(result.getBody());
        verify(service, times(1)).createShippingInstruction(any(ShippingInstructionRequest.class));
    }

    // ---------- GET BY ID ----------
    @Test
    void testGetByIdSuccess() {
        ShippingInstructionResponse response = new ShippingInstructionResponse();
        when(service.getShippingInstructionsById(1L)).thenReturn(response);

        ResponseEntity<?> result = controller.getById(1L);

        assertEquals(HttpStatus.OK, result.getStatusCode());
        assertNotNull(result.getBody());
        verify(service).getShippingInstructionsById(1L);
    }

    @Test
    void testGetByIdFailure() {
        when(service.getShippingInstructionsById(99L))
                .thenThrow(new RuntimeException("Not found"));

        ResponseEntity<?> result = controller.getById(99L);

        assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
        verify(service).getShippingInstructionsById(99L);
    }

    // ---------- UPDATE ----------
    @Test
    void testUpdateSuccess() {
        ShippingInstructionRequest request = new ShippingInstructionRequest();
        ShippingInstructionResponse response = new ShippingInstructionResponse();

        when(service.updateShippingInstructions(any())).thenReturn(response);

        ResponseEntity<?> result = controller.update(request);

        assertEquals(HttpStatus.OK, result.getStatusCode());
        assertNotNull(result.getBody());
        verify(service).updateShippingInstructions(any());
    }

    @Test
    void testUpdateFailure() {
        ShippingInstructionRequest request = new ShippingInstructionRequest();

        when(service.updateShippingInstructions(any()))
                .thenThrow(new RuntimeException("Update failed"));

        ResponseEntity<?> result = controller.update(request);

        assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
        verify(service).updateShippingInstructions(any());
    }

    // ---------- DELETE ----------
    @Test
    void testDeleteSuccess() {
        doNothing().when(service).deleteShippingInstructions(1L);

        ResponseEntity<?> result = controller.delete(1L);

        assertEquals(HttpStatus.OK, result.getStatusCode());
        verify(service).deleteShippingInstructions(1L);
    }

    @Test
    void testDeleteFailure() {
        doThrow(new RuntimeException("Delete failed"))
                .when(service).deleteShippingInstructions(1L);

        ResponseEntity<?> result = controller.delete(1L);

        assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
        verify(service).deleteShippingInstructions(1L);
    }

    // ---------- GET ALL MASTER DATA ----------
    @Test
    void testGetAllMasterDataSuccess() {
        ResponseEntity<?> mockResponse = ResponseEntity.ok("master data");
        when(service.getAllMasterData(1L)).thenReturn((ResponseEntity<IRunnerResponse>) mockResponse);

        ResponseEntity<?> result = controller.getAllMasterData(1L);

        assertEquals(HttpStatus.OK, result.getStatusCode());
        assertEquals("master data", result.getBody());
        verify(service).getAllMasterData(1L);
    }

    @Test
    void testGetAllMasterDataFailure() {
        when(service.getAllMasterData(2L))
                .thenThrow(new RuntimeException("Master data fetch failed"));

        ResponseEntity<?> result = controller.getAllMasterData(2L);

        assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
        verify(service).getAllMasterData(2L);
    }

}
