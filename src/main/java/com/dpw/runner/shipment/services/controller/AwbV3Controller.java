package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.AwbRequest;
import com.dpw.runner.shipment.services.dto.request.FetchAwbListRequest;
import com.dpw.runner.shipment.services.dto.request.ResetAwbRequest;
import com.dpw.runner.shipment.services.dto.response.AwbCalculationResponse;
import com.dpw.runner.shipment.services.dto.response.AwbResponse;
import com.dpw.runner.shipment.services.dto.response.FnmStatusMessageResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAwbService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;


@SuppressWarnings("ALL")
@RestController
@RequestMapping(AwbConstants.AWB_V3_API_HANDLE)
@Slf4j
public class AwbV3Controller {
    private final IAwbService awbService;

    private class MyResponseClass extends RunnerResponse<AwbResponse>{}
    private class MyListResponseClass extends RunnerListResponse<AwbResponse>{}
    private class AwbCalculationResponseClass extends RunnerResponse<AwbCalculationResponse>{}
    private class FnmStatusMessageResponseClass extends RunnerResponse<FnmStatusMessageResponse>{}


    @Autowired
    public AwbV3Controller(IAwbService awbService) {
        this.awbService = awbService;
    }


    @ApiResponses(value = {
            @ApiResponse(code = 200, response = AwbV3Controller.MyListResponseClass.class, message = AwbConstants.AWB_LIST_SUCCESSFUL, responseContainer = AwbConstants.RESPONSE_CONTAINER_LIST)
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

}
