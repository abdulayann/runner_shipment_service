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
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;
import java.util.Optional;


@SuppressWarnings("ALL")
@RestController
@RequestMapping(AwbConstants.AWB_API_HANDLE)
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
            @ApiResponse(code = 200, response = MyListResponseClass.class, message = AwbConstants.AWB_LIST_SUCCESSFUL, responseContainer = AwbConstants.RESPONSE_CONTAINER_LIST)
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
            @ApiResponse(code = 200, message = AwbConstants.AWB_CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
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
            @ApiResponse(code = 200, message = AwbConstants.AWB_UPDATE_SUCCESSFUL, response = MyResponseClass.class)
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

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyResponseClass.class, message = AwbConstants.AWB_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping("/retrieve/id")
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = AwbConstants.AWB_ID, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return awbService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbConstants.AWB_RETRIEVE_BY_ID_SUCCESSFUL)})
    @PostMapping("/customRetrieve")
    public ResponseEntity<IRunnerResponse> retrieveByIssuingAgentName(@RequestBody CustomAwbRetrieveRequest request) {
        return awbService.customAwbRetrieve(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = AwbConstants.MAWB_CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
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
            @ApiResponse(code = 200, message = AwbConstants.MAWB_GOODS_AND_PACKS_UPDATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
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
            @ApiResponse(code = 200, message = AwbConstants.AWB_SYNC_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
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
            @ApiResponse(code = 200, message = AwbConstants.AWB_SYNC_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
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
            @ApiResponse(code = 200, message = AwbConstants.AWB_UPDATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
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
            @ApiResponse(code = 200, message = AwbConstants.AWB_UPDATE_SUCCESSFUL, response = RunnerResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
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

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
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

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbConstants.PAYMENT_INFO_RETRIEVE_SUCCESS, response = AwbCalculationResponseClass.class)})
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


    @ApiResponses(value = {@ApiResponse(code = 200, response = MyListResponseClass.class, message = AwbConstants.AWB_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.RETIEVE_BY_MAWB_ID)
    public ResponseEntity<IRunnerResponse> retrieveByAwbByMawb(@ApiParam(value = AwbConstants.MAWB, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return awbService.retrieveByAwbByMawb(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbConstants.DIMS_TEXT_RETERIEVE_SUCCESS)})
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

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbConstants.CHARGE_TYPE_DATA_RETRIEVE_SUCCESSFUL)})
    @GetMapping(ApiConstants.POPULATE_CHARGE_TYPE_DETAILS)
    public ResponseEntity<IRunnerResponse> getChargeTypeMasterData(@ApiParam(value = AwbConstants.CHARGE_TYPE_ID, required = true) @RequestParam Long id) {
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

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbConstants.CHARGE_TYPE_DATA_RETRIEVE_SUCCESSFUL)})
    @GetMapping(ApiConstants.VALIDATE_IATA_AGENT)
    public ResponseEntity<IRunnerResponse> validateIataAgent(@ApiParam(name = "fromShipment") Boolean fromShipment, @ApiParam(name = "Consolidation Id") @RequestParam Optional<Long> consolidationId) {
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

    @ApiResponses(value = {@ApiResponse(response = FnmStatusMessageResponseClass.class, code = 200, message = AwbConstants.FNM_STATUS_FETCH_SUCCESS)})
    @GetMapping(ApiConstants.FNM_STATUS_MESSAGE)
    public ResponseEntity<IRunnerResponse> getFnmStatusMessage(@ApiParam(name = "Shipment Id") @RequestParam Optional<Long> shipmentId, @ApiParam(name = "Consolidation Id") @RequestParam Optional<Long> consolidationId) {
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

    @ApiResponses(value = {@ApiResponse(response = IataFetchRateRequest.class, code = 200, message = AwbConstants.IATA_FETCH_RATE_SUCCESS)})
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

}
