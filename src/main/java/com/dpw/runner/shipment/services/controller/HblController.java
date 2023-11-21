package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.HblGenerateRequest;
import com.dpw.runner.shipment.services.dto.request.HblRequest;
import com.dpw.runner.shipment.services.dto.request.HblResetRequest;
import com.dpw.runner.shipment.services.dto.response.HblResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IHblService;
import com.dpw.runner.shipment.services.syncing.Entity.HblRequestV2;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

@SuppressWarnings("ALL")
@RestController
@RequestMapping(HblConstants.HBL_API_HANDLE)
@Slf4j
public class HblController {
    @Autowired
    private IHblService hblService;

    @ApiResponses(value = {@ApiResponse(code = 200, message = HblConstants.HBL_GENERATION_SUCCESS, response = RunnerResponse.class)})
    @PostMapping(HblConstants.API_GENERATE_HBL)
    public ResponseEntity<RunnerResponse> generateHBL(@RequestBody @Valid HblGenerateRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) hblService.generateHBL(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = HblConstants.HBL_UPDATE_SUCCESS, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid HblRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) hblService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = HblConstants.HBL_DELETE_SUCCESS)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) hblService.delete(CommonRequestModel.buildRequest(request));
    }


    @ApiResponses(value = {@ApiResponse(code = 200, message = HblConstants.HBLS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<HblResponse>> retrieveById(@ApiParam(value = HblConstants.HBL_ID, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return (ResponseEntity<RunnerResponse<HblResponse>>) hblService.retrieveById(CommonRequestModel.buildRequest(request));
    }


    @ApiResponses(value = {@ApiResponse(code = 200, message = HblConstants.HBLS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(HblConstants.API_RETRIEVE_BY_SHIPMENT_ID)
    public ResponseEntity<RunnerResponse> retrieveByShipmentId(@ApiParam(value = HblConstants.HBL_SHIPMENT_ID, required = true) @RequestParam Long shipmentId) {
        String responseMsg;
        try {
            CommonGetRequest request = CommonGetRequest.builder().id(shipmentId).build();
            return (ResponseEntity<RunnerResponse>) hblService.retrieveByShipmentId(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = HblConstants.HBL_RESET_SUCCESSFULL, response = RunnerResponse.class)})
    @PostMapping(HblConstants.API_RESET_HBL)
    public ResponseEntity<RunnerResponse> resetHbl(@RequestBody @Valid HblResetRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) hblService.resetHbl(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ConsolidationConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_SAVE_FROM_V1)
    public ResponseEntity<RunnerResponse<HblResponse>> saveV1Hbl(@RequestBody @Valid HblRequestV2 request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<HblResponse>>) hblService.saveV1Hbl(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<HblResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = HblConstants.HBL_GENERATION_SUCCESS, response = RunnerResponse.class)})
    @PostMapping(HblConstants.API_PARTIAL_UPDATE)
    public ResponseEntity<RunnerResponse> partialUpdateHBL(@RequestBody @Valid HblGenerateRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) hblService.partialUpdateHBL(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

}
