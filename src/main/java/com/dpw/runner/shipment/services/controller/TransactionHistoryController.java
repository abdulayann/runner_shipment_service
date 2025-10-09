package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.TransactionHistoryConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.TransactionHistoryResponse;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITransactionHistoryService;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping(TransactionHistoryConstants.TRANSACTION_HISTORY_API_HANDLE)
@Slf4j
public class TransactionHistoryController {

    private final ITransactionHistoryService transactionHistoryService;
    private final JsonHelper jsonHelper;

    public TransactionHistoryController(ITransactionHistoryService transactionHistoryService, JsonHelper jsonHelper) {
        this.transactionHistoryService = transactionHistoryService;
        this.jsonHelper = jsonHelper;
    }

    // Response wrapper classes
    private static class MyResponseClass extends RunnerResponse<TransactionHistoryResponse> {
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = TransactionHistoryController.MyResponseClass.class)), description = TransactionHistoryConstants.TRANSACTION_HISTORY_RETRIEVE_BY_ID_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@RequestParam Long entityId, @RequestParam EntityTypeTransactionHistory entityType) {
        log.info("Received Transaction History GET BY ID request with RequestId: {}, id: {} and entityType: {}",
                LoggerHelper.getRequestIdFromMDC(), entityId, entityType);
        List<TransactionHistoryResponse> response = transactionHistoryService.retrieveById(entityId, entityType);
        log.info("Verified Gross Mass GET BY ID successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(response));
        return ResponseHelper.buildSuccessResponse(response);
    }
}
