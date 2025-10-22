package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.NPMConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.response.QuoteContractsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IQuoteContractsService;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(NPMConstants.QUOTE_CONTRACTS_DATA_API_HANDLE)
@Slf4j
public class QuoteContractsController {

    private final IQuoteContractsService quoteContractsService;

    @Autowired
    public QuoteContractsController(IQuoteContractsService quoteContractsService) {
        this.quoteContractsService = quoteContractsService;
    }

    private static class MyQuoteContractsListResponseClass extends RunnerListResponse<QuoteContractsResponse> {}

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = NPMConstants.QUOTE_CONTRACTS_LIST_SUCCESSFUL, content = @Content(schema = @Schema(implementation =  QuoteContractsController.MyQuoteContractsListResponseClass.class)))})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return quoteContractsService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

}
