package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.response.JobResponse;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.impl.RoutingsService;
import com.dpw.runner.shipment.services.syncing.Entity.ElDetailsRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.RoutingsRequestV2;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.*;
import org.springframework.beans.factory.annotation.Autowired;

import javax.validation.Valid;

import java.util.List;

import static com.dpw.runner.shipment.services.commons.constants.Constants.NO_DATA;

@RestController
@RequestMapping(RoutingsConstants.ROUTINGS_API_HANDLE)
@Slf4j
public class RoutingsController {

    @Autowired
    private RoutingsService routingsService;

    @Autowired
    JsonHelper jsonHelper;

    @PostMapping(ApiConstants.API_CREATE)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = RoutingsConstants.ROUTINGS_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<RunnerResponse<RoutingsResponse>> create(@RequestBody @Valid @NonNull RoutingsRequest request) {
        try {
            RoutingsRequest req = jsonHelper.convertValue(request, RoutingsRequest.class);
            return (ResponseEntity<RunnerResponse<RoutingsResponse>>) routingsService.create(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return (ResponseEntity<RunnerResponse<RoutingsResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = RoutingsConstants.ROUTINGS_UPDATE_SUCCESSFUL)
    })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity update(@RequestBody @Valid @NonNull RoutingsRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) routingsService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);

    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = RoutingsConstants.ROUTINGS_DELETE_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) routingsService.delete(CommonRequestModel.buildRequest(request));

    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = RoutingsConstants.ROUTINGS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity retrieve(@RequestParam @NonNull Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<RoutingsResponse>>) routingsService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = RoutingsConstants.ROUTINGS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID_PARTIAL)
    public ResponseEntity<?> retrieveByIdPartial(@RequestParam(name = "includeColumns", required = false) List<String> includeColumns, @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        try {
            ResponseEntity<RunnerResponse<RoutingsResponse>> route = (ResponseEntity<RunnerResponse<RoutingsResponse>>) routingsService.retrieveById(CommonRequestModel.buildRequest(request));
            return ResponseEntity.ok(PartialFetchUtils.fetchPartialData(route, includeColumns));
        } catch (Exception ex) {
            System.out.println(ex.toString());
        }
        return ResponseEntity.ok(null);
    }


    @ApiResponses(value = {
            @ApiResponse(code = 200, message = RoutingsConstants.ROUTINGS_LIST_SUCCESSFUL, responseContainer = RoutingsConstants.ROUTINGS_LIST_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity list(@RequestBody @NonNull @Valid ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<RoutingsResponse>>) routingsService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.SHIPMENT_SYNC_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.SYNC)
    public ResponseEntity<?> syncRoutingsToService(@RequestBody @Valid RoutingsRequestV2 request) {
        String responseMsg = "failure executing :(";
        try {
            return routingsService.V1RoutingsCreateAndUpdate(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error syncing provided Routings";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
