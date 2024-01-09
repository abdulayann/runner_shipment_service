package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.AwbRequest;
import com.dpw.runner.shipment.services.dto.request.CreateAwbRequest;
import com.dpw.runner.shipment.services.dto.request.ResetAwbRequest;
import com.dpw.runner.shipment.services.dto.request.awb.CustomAwbRetrieveRequest;
import com.dpw.runner.shipment.services.dto.request.awb.GenerateAwbPaymentInfoRequest;
import com.dpw.runner.shipment.services.dto.response.AwbResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
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
            return (ResponseEntity<RunnerListResponse<AwbResponse>>) awbService.list(CommonRequestModel.buildRequest(listCommonRequest));
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
    public ResponseEntity<RunnerResponse<AwbResponse>> retrieveById(@ApiParam(value = AwbConstants.AWB_ID, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return (ResponseEntity<RunnerResponse<AwbResponse>>) awbService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbConstants.AWB_RETRIEVE_BY_ID_SUCCESSFUL)})
    @PostMapping("/customRetrieve")
    public ResponseEntity<RunnerListResponse<AwbResponse>> retrieveByIssuingAgentName(@RequestBody CustomAwbRetrieveRequest request) {
        return (ResponseEntity<RunnerListResponse<AwbResponse>>) awbService.customAwbRetrieve(CommonRequestModel.buildRequest(request));
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
    public ResponseEntity<RunnerResponse<AwbResponse>> createV1Awb(@RequestBody @Valid AwbRequest request, @RequestParam(required = false, defaultValue = "true") boolean checkForSync) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<AwbResponse>>) awbService.createV1Awb(CommonRequestModel.buildRequest(request), checkForSync);
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
    @PostMapping("/reset")
    public ResponseEntity<RunnerResponse<AwbResponse>> reset(@RequestBody @Valid ResetAwbRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<AwbResponse>>) awbService.reset(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<AwbResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = AwbConstants.AWB_UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping("/partial-update-awb")
    public ResponseEntity<RunnerResponse<AwbResponse>> partialAutoUpdateAwb(@RequestBody @Valid CreateAwbRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<AwbResponse>>) awbService.partialAutoUpdateAwb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<AwbResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = AwbConstants.AWB_UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping("/partial-update-mawb")
    public ResponseEntity<RunnerResponse<AwbResponse>> partialAutoUpdateMawb(@RequestBody @Valid CreateAwbRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<AwbResponse>>) awbService.partialAutoUpdateMawb(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<AwbResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
    @GetMapping(ApiConstants.GET_ALL_MASTER_DATA)
    public ResponseEntity<?> getAllMasterData(@RequestParam(required = false) Long shipmentId, @RequestParam(required = false) Long consolidationId) {
        String responseMsg = "failure executing :(";
        try {
            if(shipmentId != null)
                return (ResponseEntity<?>) awbService.getAllMasterData(CommonRequestModel.buildRequest(shipmentId), true);
            else if(consolidationId != null)
                return (ResponseEntity<?>) awbService.getAllMasterData(CommonRequestModel.buildRequest(consolidationId), false);
            return ResponseHelper.buildFailedResponse(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error retrieving master data";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbConstants.PAYMENT_INFO_RETRIEVE_SUCCESS)})
    @PostMapping(AwbConstants.GET_AWB_PAYMENT_INFO)
    public ResponseEntity<?> generateAwbPaymentIndo(@RequestBody GenerateAwbPaymentInfoRequest req) {
        String responseMsg = "failure executing :(";
        try {
            return (ResponseEntity<?>) awbService.generateAwbPaymentInfo(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error generating payment information";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }


    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbConstants.AWB_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.RETIEVE_BY_MAWB_ID)
    public ResponseEntity<RunnerListResponse<AwbResponse>> retrieveByAwbByMawb(@ApiParam(value = AwbConstants.MAWB, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return (ResponseEntity<RunnerListResponse<AwbResponse>>) awbService.retrieveByAwbByMawb(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbConstants.DIMS_TEXT_RETERIEVE_SUCCESS)})
    @PostMapping(AwbConstants.DIMS_TEXT)
    public ResponseEntity<?> dimsText(@RequestBody GenerateAwbPaymentInfoRequest req) {
        String responseMsg = "failure executing :(";
        try {
            return (ResponseEntity<?>) awbService.generateUpdatedNatureAndQuantGoodsField(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error generating dims information";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }
}
