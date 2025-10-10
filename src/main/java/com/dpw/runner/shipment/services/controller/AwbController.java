package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.awb.CustomAwbRetrieveRequest;
import com.dpw.runner.shipment.services.dto.request.awb.GenerateAwbPaymentInfoRequest;
import com.dpw.runner.shipment.services.dto.response.AwbCalculationResponse;
import com.dpw.runner.shipment.services.dto.response.AwbResponse;
import com.dpw.runner.shipment.services.dto.response.FnmStatusMessageResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAwbService;
import com.dpw.runner.shipment.services.syncing.Entity.AwbRequestV2;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;
import java.util.Optional;


@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = {AwbConstants.AWB_API_HANDLE, AwbConstants.AWB_V3_API_HANDLE})
@Slf4j
public class AwbController {
    private final IAwbService awbService;

    private class MyResponseClass extends RunnerResponse<AwbResponse>{}
    private class MyListResponseClass extends RunnerListResponse<AwbResponse>{}
    private class AwbCalculationResponseClass extends RunnerResponse<AwbCalculationResponse>{}
    private class FnmStatusMessageResponseClass extends RunnerResponse<FnmStatusMessageResponse>{}


    @Autowired
    public AwbController(IAwbService awbService) {
        this.awbService = awbService;
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", content = @Content( array = @ArraySchema(schema = @Schema(implementation = MyListResponseClass.class))), description = AwbConstants.AWB_LIST_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid FetchAwbListRequest listCommonRequest) {

        try {
            return awbService.list(CommonRequestModel.buildRequest(listCommonRequest));
        } catch (Exception ex) {
            log.error(ex.toString());
        }
        return ResponseEntity.ok(null);

    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = AwbConstants.AWB_CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping("/create")
    public ResponseEntity<IRunnerResponse> createAwb(@RequestBody @Valid CreateAwbRequest request) {
        String responseMsg;
        try {
            return awbService.createAwb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = AwbConstants.AWB_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class)))
    })
    @PutMapping("/update")
    public ResponseEntity<IRunnerResponse> updateAwbDetails(@RequestBody @Valid AwbRequest request) {
        String responseMsg;
        try {
            return awbService.updateAwb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = MyResponseClass.class, description = AwbConstants.AWB_RETRIEVE_BY_ID_SUCCESSFUL)))})
    @GetMapping("/retrieve/id")
    public ResponseEntity<IRunnerResponse> retrieveById(@Parameter(description = AwbConstants.AWB_ID, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return awbService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = AwbConstants.AWB_RETRIEVE_BY_ID_SUCCESSFUL)})
    @PostMapping("/customRetrieve")
    public ResponseEntity<IRunnerResponse> retrieveByIssuingAgentName(@RequestBody CustomAwbRetrieveRequest request) {
        return awbService.customAwbRetrieve(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = AwbConstants.MAWB_CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping("/mawb/create")
    public ResponseEntity<IRunnerResponse> createMawb(@RequestBody @Valid CreateAwbRequest request) {
        String responseMsg;
        try {
            return awbService.createMawb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = AwbConstants.MAWB_GOODS_AND_PACKS_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PutMapping("/mawb/goods_and_packs/update")
    public ResponseEntity<IRunnerResponse> updateGoodsAndPacksForMawb(@RequestBody @Valid CreateAwbRequest request) {
        String responseMsg;
        try {
            return awbService.updateGoodsAndPacksForMawb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = AwbConstants.AWB_SYNC_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping("/createV1Awb")
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> createV1Awb(@RequestBody @Valid AwbRequestV2 request, @RequestParam(required = false, defaultValue = "true") boolean checkForSync) {
        String responseMsg;
        try {
            return awbService.createV1Awb(CommonRequestModel.buildRequest(request), checkForSync);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = AwbConstants.AWB_SYNC_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping("/reset")
    public ResponseEntity<IRunnerResponse> reset(@RequestBody @Valid ResetAwbRequest request) {
        String responseMsg;
        try {
            return awbService.reset(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = AwbConstants.AWB_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping("/partial-update-awb")
    public ResponseEntity<IRunnerResponse> partialAutoUpdateAwb(@RequestBody @Valid CreateAwbRequest request) {
        String responseMsg;
        try {
            return awbService.partialAutoUpdateAwb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = AwbConstants.AWB_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping("/partial-update-mawb")
    public ResponseEntity<IRunnerResponse> partialAutoUpdateMawb(@RequestBody @Valid CreateAwbRequest request) {
        String responseMsg;
        try {
            return awbService.partialAutoUpdateMawb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = AwbConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
    @GetMapping(ApiConstants.GET_ALL_MASTER_DATA)
    public ResponseEntity<IRunnerResponse> getAllMasterData(@RequestParam(required = false) Long shipmentId, @RequestParam(required = false) Long consolidationId) {
        String responseMsg = Constants.FAILURE_EXECUTING;
        try {
            if(shipmentId != null)
                return awbService.getAllMasterData(CommonRequestModel.buildRequest(shipmentId), true);
            else if(consolidationId != null)
                return awbService.getAllMasterData(CommonRequestModel.buildRequest(consolidationId), false);
            return ResponseHelper.buildFailedResponse(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error retrieving master data";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = AwbConstants.PAYMENT_INFO_RETRIEVE_SUCCESS, content = @Content(schema = @Schema(implementation = AwbCalculationResponseClass.class)))})
    @PostMapping(AwbConstants.GET_AWB_PAYMENT_INFO)
    public ResponseEntity<IRunnerResponse> generateAwbPaymentIndo(@RequestBody GenerateAwbPaymentInfoRequest req) {
        String responseMsg = Constants.FAILURE_EXECUTING;
        try {
            return awbService.generateAwbPaymentInfo(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error generating payment information";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }


    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = MyListResponseClass.class, description = AwbConstants.AWB_RETRIEVE_BY_ID_SUCCESSFUL)))})
    @GetMapping(ApiConstants.RETIEVE_BY_MAWB_ID)
    public ResponseEntity<IRunnerResponse> retrieveByAwbByMawb(@Parameter(description = AwbConstants.MAWB, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return awbService.retrieveByAwbByMawb(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = AwbConstants.DIMS_TEXT_RETERIEVE_SUCCESS)})
    @PostMapping(AwbConstants.DIMS_TEXT)
    public ResponseEntity<IRunnerResponse> dimsText(@RequestBody GenerateAwbPaymentInfoRequest req) {
        String responseMsg = Constants.FAILURE_EXECUTING;
        try {
            return awbService.generateUpdatedNatureAndQuantGoodsField(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error generating dims information";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = AwbConstants.CHARGE_TYPE_DATA_RETRIEVE_SUCCESSFUL)})
    @GetMapping(ApiConstants.POPULATE_CHARGE_TYPE_DETAILS)
    public ResponseEntity<IRunnerResponse> getChargeTypeMasterData(@Parameter(description = AwbConstants.CHARGE_TYPE_ID, required = true) @RequestParam Long id) {
        String responseMsg = "";
        try {
            CommonGetRequest request = CommonGetRequest.builder().id(id).build();
            return awbService.getChargeTypeMasterData(request);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error getting data for charge type";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = AwbConstants.CHARGE_TYPE_DATA_RETRIEVE_SUCCESSFUL)})
    @GetMapping(ApiConstants.VALIDATE_IATA_AGENT)
    public ResponseEntity<IRunnerResponse> validateIataAgent(@Parameter(description = "fromShipment") Boolean fromShipment, @Parameter(description = "Consolidation Id") @RequestParam Optional<Long> consolidationId) {
        String responseMsg = "";
        try {
            return awbService.validateIataAgent(fromShipment, consolidationId);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error getting data for Iata validations ";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = AwbConstants.CHARGE_TYPE_DATA_RETRIEVE_SUCCESSFUL)})
    @GetMapping(ApiConstants.VALIDATE_AWB_FOR_ATTACHMENT)
    public ResponseEntity<IRunnerResponse> validateAwbBeforeAttachment(@Parameter(description = "Consolidation Id", required = true) @RequestParam Optional<Long> consolidationId) {
        String responseMsg = "";
        try {
            return awbService.validateAwbBeforeAttachment(consolidationId);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error getting data for Awb validations for attachment";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(content = @Content(schema = @Schema(implementation = FnmStatusMessageResponseClass.class)), responseCode = "200", description = AwbConstants.FNM_STATUS_FETCH_SUCCESS)})
    @GetMapping(ApiConstants.FNM_STATUS_MESSAGE)
    public ResponseEntity<IRunnerResponse> getFnmStatusMessage(@Parameter(description = "Shipment Id") @RequestParam Optional<Long> shipmentId, @Parameter(description = "Consolidation Id") @RequestParam Optional<Long> consolidationId) {
        String responseMsg = "";
        try {
            return awbService.getFnmStatusMessage(shipmentId, consolidationId);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error getting air messaging logs";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(content = @Content(schema = @Schema(implementation = IataFetchRateRequest.class)), responseCode = "200", description = AwbConstants.IATA_FETCH_RATE_SUCCESS)})
    @PostMapping(ApiConstants.FETCH_IATA_RATES)
    public ResponseEntity<IRunnerResponse> getFetchIataRates(@RequestBody IataFetchRateRequest iataFetchRateRequest) {
        String responseMsg = "";
        try {
            return awbService.getFetchIataRates(CommonRequestModel.buildRequest(iataFetchRateRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error Fetching Iata Rates";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }


    @ApiResponses(value = {@ApiResponse(content = @Content(schema = @Schema(implementation = IataFetchRateRequest.class)), responseCode = "200", description = AwbConstants.IATA_FETCH_RATE_SUCCESS)})
    @GetMapping(AwbConstants.AIR_MESSAGE_STATUS_RESET)
    public ResponseEntity<IRunnerResponse> airMessageStatusReset(@Parameter(description = AwbConstants.AWB_ID, required = true) @RequestParam Long id) {
        String responseMsg = "";
        try {
            return awbService.airMessageStatusReset(CommonRequestModel.buildRequest(id));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error While Air Message Status Reset";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

}
