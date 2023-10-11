package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.AwbRequest;
import com.dpw.runner.shipment.services.dto.request.CreateAwbRequest;
import com.dpw.runner.shipment.services.dto.response.AwbResponse;
import com.dpw.runner.shipment.services.dto.response.BookingCarriageResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAwbService;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import lombok.val;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.ArrayList;
import java.util.List;


@SuppressWarnings("ALL")
@RestController
@RequestMapping(AwbConstants.AWB_API_HANDLE)
@Slf4j
public class AwbController {
    @Autowired
    private IAwbService awbService;
    @Autowired
    ObjectMapper objectMapper;

    @Autowired
    private JsonHelper jsonHelper;

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbConstants.AWB_LIST_SUCCESSFUL, responseContainer = AwbConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<?> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {

        try {
            List<String>includeColumns = listCommonRequest.getIncludeColumns();
            ResponseEntity<RunnerListResponse<AwbResponse>>list = (ResponseEntity<RunnerListResponse<AwbResponse>>) awbService.list(CommonRequestModel.buildRequest(listCommonRequest));
            if(includeColumns==null||includeColumns.size()==0)return list;
            List<Object> filtered_list= new ArrayList<>();

            for( AwbResponse curr: list.getBody().getData()){
                //    filtered_list.add((AwbResponse)jsonHelper.readFromJson(PartialFetchUtils.fetchPartialData((ResponseEntity<RunnerResponse<AwbResponse>>)ResponseHelper.buildSuccessResponse(curr),includeColumns).toString(), AwbResponse.class));
                filtered_list.add(jsonHelper.readFromJson(PartialFetchUtils.fetchPartialData((ResponseEntity<RunnerResponse<AwbResponse>>)ResponseHelper.buildSuccessResponse(curr),includeColumns).toString(), Object.class));
            }
            return ResponseEntity.ok(filtered_list);
            // ResponseEntity<?> response = ResponseHelper.buildListSuccessResponse(filtered_list);
            // return (ResponseEntity<RunnerListResponse<AwbResponse>>)response;

        } catch (Exception ex) {
            System.out.println(ex.toString());
        }
        return ResponseEntity.ok(null);

    }



    @ApiResponses(value = {
            @ApiResponse(code = 200, message = AwbConstants.AWB_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping("/create")
    public ResponseEntity<RunnerResponse<AwbResponse>> createAwb(@RequestBody @Valid CreateAwbRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<AwbResponse>>) awbService.createAwb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<AwbResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbConstants.AWB_UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping("/update")
    public ResponseEntity<RunnerResponse> updateAwbDetails(@RequestBody @Valid AwbRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) awbService.updateAwb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbConstants.AWB_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping("/retrieve/id")
    public ResponseEntity<RunnerResponse<AwbResponse>> retrieveById(@ApiParam(value = AwbConstants.AWB_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<AwbResponse>>) awbService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbConstants.AWB_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID_PARTIAL)
    public ResponseEntity<?> retrieveByIdPartial(@RequestParam(name = "includeColumns", required = false) List<String> includeColumns, @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        try {
            ResponseEntity<RunnerResponse<AwbResponse>> awb = (ResponseEntity<RunnerResponse<AwbResponse>>) awbService.retrieveById(CommonRequestModel.buildRequest(request));
            return ResponseEntity.ok(PartialFetchUtils.fetchPartialData(awb, includeColumns));
        } catch (Exception ex) {
            System.out.println(ex.toString());
        }
        return ResponseEntity.ok(null);
    }


    @ApiResponses(value = {
            @ApiResponse(code = 200, message = AwbConstants.MAWB_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping("/mawb/create")
    public ResponseEntity<RunnerResponse<AwbResponse>> createMawb(@RequestBody @Valid CreateAwbRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<AwbResponse>>) awbService.createMawb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<AwbResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = AwbConstants.MAWB_GOODS_AND_PACKS_UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PutMapping("/mawb/goods_and_packs/update")
    public ResponseEntity<RunnerResponse<AwbResponse>> updateGoodsAndPacksForMawb(@RequestBody @Valid CreateAwbRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<AwbResponse>>) awbService.updateGoodsAndPacksForMawb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<AwbResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = AwbConstants.AWB_SYNC_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping("/createV1Awb")
    public ResponseEntity<RunnerResponse<AwbResponse>> createV1Awb(@RequestBody @Valid AwbRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<AwbResponse>>) awbService.createV1Awb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<AwbResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }
}
