package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.dto.request.AttachListShipmentRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IQuoteContractsService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class QuoteContractsControllerTest {

    @Mock
    private IQuoteContractsService quoteContractsService;

    @InjectMocks
    private QuoteContractsController quoteContractsController;

    @Test
    void list() throws RunnerException {
        when(quoteContractsService.list(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = quoteContractsController.list(new AttachListShipmentRequest());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }
}
