package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.adapters.interfaces.INPMServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.NPMConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ListContractRequest;
import com.dpw.runner.shipment.services.dto.request.ListContractsWithFilterRequest;
import com.dpw.runner.shipment.services.dto.request.npm.GetContractsCountForPartiesRequest;
import com.dpw.runner.shipment.services.dto.request.npm.NPMAutoSellRequest;
import com.dpw.runner.shipment.services.dto.request.npm.NPMFetchOffersRequestFromUI;
import com.dpw.runner.shipment.services.dto.request.npm.NPMImportRatesRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
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

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = NPMConstants.NPM_API_HANDLE)
public class NPMController {
    private final INPMServiceAdapter npmService;

    @Autowired
    public NPMController(INPMServiceAdapter npmService) {
        this.npmService = npmService;
    }

    @PostMapping(NPMConstants.RETRIEVE_CONTRACT_SHIPMENT)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = NPMConstants.CONTRACT_LIST_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> fetchContractFromShipment(@RequestBody @Valid ListContractRequest request) {
        String responseMsg;
        try {
            return  npmService.fetchContractFromShipment(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : NPMConstants.CONTRACT_LIST_FAILED;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(NPMConstants.LIST_CONTRACT)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = NPMConstants.CONTRACT_LIST_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> fetchContracts(@RequestBody @Valid ListContractsWithFilterRequest request) {
        String responseMsg;
        try {
             return  npmService.fetchContracts(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : NPMConstants.CONTRACT_LIST_FAILED;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(NPMConstants.LIST_CONTRACTS_WITH_FILTERS)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = NPMConstants.CONTRACT_LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> fetchContractsWithFilters(@RequestBody @Valid ListContractsWithFilterRequest request) {
        String responseMsg;
        try {
            return  npmService.fetchContractsWithFilters(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : NPMConstants.CONTRACT_LIST_FAILED;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(NPMConstants.RETRIEVE_CONTRACT)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = NPMConstants.CONTRACT_LIST_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> fetchContract(@RequestBody @Valid ListContractRequest request) {
        String responseMsg;
        try {
            return  npmService.fetchContract(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : NPMConstants.CONTRACT_LIST_FAILED;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(NPMConstants.GET_OFFERS)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = NPMConstants.LIST_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> getNPMOffers(@RequestBody @Valid NPMFetchOffersRequestFromUI request) {
        String responseMsg;
        try {
            return npmService.fetchOffers(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(NPMConstants.GET_OFFERS_V8)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = NPMConstants.LIST_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> getNPMOffersV8(@RequestBody @Valid NPMFetchOffersRequestFromUI request) {
        String responseMsg;
        try {
            return npmService.fetchOffersV8(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping("/getAwbAutoSell")
    @ExcludeTimeZone
    public ResponseEntity <IRunnerResponse> getAwbAutoSell(@RequestBody NPMAutoSellRequest request) {
        String responseMsg;
        try {
            return npmService.awbAutoSell(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping("/getAwbImportRates")
    @ExcludeTimeZone
    public ResponseEntity <IRunnerResponse> getAwbImportRates(@RequestBody NPMImportRatesRequest request) {
        String responseMsg;
        try {
            return npmService.awbImportRates(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(NPMConstants.LIST_CONTRACTS_MULTI_PARTY)
    public ResponseEntity<IRunnerResponse> fetchContractsCountForParties(@RequestBody @Valid GetContractsCountForPartiesRequest request) {
        try {
            return npmService.fetchContractsCountForParties(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : NPMConstants.CONTRACT_LIST_FAILED;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }
}
