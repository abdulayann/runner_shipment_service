package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.AwbRequest;
import com.dpw.runner.shipment.services.dto.request.CreateAwbRequest;
import com.dpw.runner.shipment.services.dto.response.AwbResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAwbService;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@SuppressWarnings("ALL")
@RestController
@RequestMapping(AwbShipmentInfoConstants.AWB_API_HANDLE)
@Slf4j
public class AwbShipmentInfoController {
    @Autowired
    private IAwbService awbShipmentInfoService;
    @Autowired
    ObjectMapper objectMapper;

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbShipmentInfoConstants.AWB_LIST_SUCCESSFUL, responseContainer = AwbShipmentInfoConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<AwbResponse>> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<AwbResponse>>) awbShipmentInfoService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = AwbShipmentInfoConstants.AWB_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping("/create")
    public ResponseEntity<RunnerResponse<AwbResponse>> createAwb(@RequestBody @Valid CreateAwbRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<AwbResponse>>) awbShipmentInfoService.createAwb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<AwbResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbShipmentInfoConstants.AWB_UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping("/update")
    public ResponseEntity<RunnerResponse> updateAwbDetails(@RequestBody @Valid AwbRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) awbShipmentInfoService.updateAwb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbShipmentInfoConstants.AWB_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping("/retrieve/id")
    public ResponseEntity<RunnerResponse<AwbResponse>> retrieveById(@ApiParam(value = AwbShipmentInfoConstants.AWB_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<AwbResponse>>) awbShipmentInfoService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = AwbShipmentInfoConstants.MAWB_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping("/mawb/create")
    public ResponseEntity<RunnerResponse<AwbResponse>> createMawb(@RequestBody @Valid CreateAwbRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<AwbResponse>>) awbShipmentInfoService.createMawb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<AwbResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = AwbShipmentInfoConstants.MAWB_GOODS_AND_PACKS_UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PutMapping("/mawb/goods_and_packs/update")
    public ResponseEntity<RunnerResponse<AwbResponse>> updateGoodsAndPacksForMawb(@RequestBody @Valid CreateAwbRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<AwbResponse>>) awbShipmentInfoService.updateGoodsAndPacksForMawb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<AwbResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }
}
