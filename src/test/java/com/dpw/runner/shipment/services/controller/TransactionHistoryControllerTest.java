package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.TransactionHistoryResponse;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITransactionHistoryService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class TransactionHistoryControllerTest {

    @Mock
    private ITransactionHistoryService transactionHistoryService;

    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private TransactionHistoryController transactionHistoryController;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void retrieveById_ShouldReturnSuccessResponse_WhenTransactionHistoryFound() {
        Long entityId = 1L;
        EntityTypeTransactionHistory entityType = EntityTypeTransactionHistory.VGM;
        TransactionHistoryResponse mockResponse = new TransactionHistoryResponse();
        mockResponse.setId(1L);
        mockResponse.setDescription("Test Transaction History");
        List<TransactionHistoryResponse> mockResponseList = new ArrayList<>();
        mockResponseList.add(mockResponse);

        // Mock only the actual return type from the service
        when(transactionHistoryService.retrieveById(entityId, entityType)).thenReturn(mockResponseList);

        // Act
        ResponseEntity<IRunnerResponse> response = transactionHistoryController.retrieveById(entityId, entityType);

        // Assert
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());
        assertTrue(response.getBody() instanceof RunnerResponse); // or TransactionHistoryResponse
        verify(transactionHistoryService).retrieveById(entityId, entityType);
    }
}
