package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ViewsRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentSettingsDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ViewsResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IViewsService;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
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
@RequestMapping(ViewsConstants.VIEW_API_HANDLE)
@Slf4j
public class ViewsController {
    @Autowired
    private IViewsService viewsService;

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ViewsConstants.VIEW_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<ViewsResponse>> createViewData(@RequestBody @Valid ViewsRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<ViewsResponse>>) viewsService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ViewsResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ViewsConstants.VIEW_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) viewsService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ViewsConstants.VIEW_LIST_SUCCESSFUL, responseContainer = ViewsConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<ViewsResponse>> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<ViewsResponse>>) viewsService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ViewsConstants.VIEW_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<ViewsResponse>> retrieveById(@ApiParam(value = ViewsConstants.VIEW_ID, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return (ResponseEntity<RunnerResponse<ViewsResponse>>) viewsService.retrieveById(CommonRequestModel.buildRequest(request));
    }

//    @ApiResponses(value = {@ApiResponse(code = 200, message = ViewsConstants.VIEW_RETRIEVE_BY_ID_SUCCESSFUL)})
//    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID_PARTIAL)
//    public ResponseEntity<?> retrieveByIdPartial(@RequestParam(name = "includeColumns", required = false) List<String> includeColumns, @RequestParam Long id) {
//        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
//        try {
//            ResponseEntity<RunnerResponse<ViewsResponse>> views = (ResponseEntity<RunnerResponse<ViewsResponse>>) viewsService.retrieveById(CommonRequestModel.buildRequest(request));
//            return ResponseEntity.ok(PartialFetchUtils.fetchPartialData(views, includeColumns));
//        } catch (Exception ex) {
//            System.out.println(ex.toString());
//        }
//        return ResponseEntity.ok(null);
//    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ViewsConstants.VIEW_UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid ViewsRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) viewsService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }
}
