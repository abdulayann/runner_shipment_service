package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.NPMConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.response.QuoteContractsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IQuoteContractsService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
@RequestMapping(NPMConstants.QUOTE_CONTRACTS_DATA_API_HANDLE)
@Slf4j
public class QuoteContractsController {

    private final IQuoteContractsService quoteContractsService;

    @Autowired
    public QuoteContractsController(IQuoteContractsService quoteContractsService) {
        this.quoteContractsService = quoteContractsService;
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = NPMConstants.QUOTE_CONTRACTS_LIST_SUCCESSFUL, response = QuoteContractsController.MyListResponseClass.class)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return quoteContractsService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    private static class MyListResponseClass extends RunnerListResponse<QuoteContractsResponse> {
    }

}
