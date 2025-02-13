package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.HblGenerateRequest;
import com.dpw.runner.shipment.services.dto.request.HblRequest;
import com.dpw.runner.shipment.services.dto.request.HblResetRequest;
import com.dpw.runner.shipment.services.dto.response.HblResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IHblService;
import com.dpw.runner.shipment.services.syncing.Entity.HblRequestV2;
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

@SuppressWarnings("ALL")
@RestController
@RequestMapping(HblConstants.HBL_API_HANDLE)
@Slf4j
public class HblController {
    private final IHblService hblService;

    @Autowired
    public HblController(IHblService hblService) {
        this.hblService = hblService;
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = HblConstants.HBL_GENERATION_SUCCESS, response = MyResponseClass.class)})
    @PostMapping(HblConstants.API_GENERATE_HBL)
    public ResponseEntity<IRunnerResponse> generateHBL(@RequestBody @Valid HblGenerateRequest request) {
        String responseMsg;
        try {
            return hblService.generateHBL(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = HblConstants.HBL_UPDATE_SUCCESS, response = MyResponseClass.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid HblRequest request) {
        String responseMsg;
        try {
            return hblService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = HblConstants.HBL_DELETE_SUCCESS, response = RunnerResponse.class)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return hblService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = HblConstants.HBLS_RETRIEVE_BY_ID_SUCCESSFUL, response = MyResponseClass.class)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = HblConstants.HBL_ID, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return hblService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = HblConstants.HBLS_RETRIEVE_BY_ID_SUCCESSFUL, response = MyResponseClass.class)})
    @GetMapping(HblConstants.API_RETRIEVE_BY_SHIPMENT_ID)
    public ResponseEntity<IRunnerResponse> retrieveByShipmentId(@ApiParam(value = HblConstants.HBL_SHIPMENT_ID, required = true) @RequestParam Long shipmentId) {
        String responseMsg;
        try {
            CommonGetRequest request = CommonGetRequest.builder().id(shipmentId).build();
            return hblService.retrieveByShipmentId(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = HblConstants.HBL_RESET_SUCCESSFULL, response = MyResponseClass.class)})
    @PostMapping(HblConstants.API_RESET_HBL)
    public ResponseEntity<IRunnerResponse> resetHbl(@RequestBody @Valid HblResetRequest request) {
        String responseMsg;
        try {
            return hblService.resetHbl(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ConsolidationConstants.CREATE_SUCCESSFUL, response = RunnerResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_SAVE_FROM_V1)
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> saveV1Hbl(@RequestBody @Valid HblRequestV2 request, @RequestParam(required = false, defaultValue = "true") boolean checkForSync) {
        String responseMsg;
        try {
            return hblService.saveV1Hbl(CommonRequestModel.buildRequest(request), checkForSync);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = HblConstants.HBL_GENERATION_SUCCESS, response = MyResponseClass.class)})
    @PostMapping(HblConstants.API_PARTIAL_UPDATE)
    public ResponseEntity<IRunnerResponse> partialUpdateHBL(@RequestBody @Valid HblGenerateRequest request) {
        String responseMsg;
        try {
            return hblService.partialUpdateHBL(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    private class MyResponseClass extends RunnerResponse<HblResponse> {
    }

}
