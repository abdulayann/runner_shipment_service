package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.RoutingConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.BulkUpdateRoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.response.RoutingListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.BulkRoutingResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsV3Service;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import java.util.List;

@RestController
@RequestMapping(RoutingConstants.ROUTING_API_HANDLE_V3)
@Slf4j
public class RoutingV3Controller {

    private IRoutingsV3Service routingService;

    @Autowired
    public RoutingV3Controller(IRoutingsV3Service routingService) {
        this.routingService = routingService;
    }


    @ApiResponses(value = {
            @ApiResponse(code = 200, message = RoutingConstants.ROUTING_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.SHIPMENT_API_CREATE)
    public ResponseEntity<IRunnerResponse> shipmentCreate(@RequestBody @Valid RoutingsRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(routingService.create(CommonRequestModel.buildRequest(request), Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = RoutingConstants.ROUTING_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.SHIPMENT_API_DELETE)
    public ResponseEntity<IRunnerResponse> shipmentDelete(@RequestParam @Valid Long id) throws RunnerException {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        routingService.delete(CommonRequestModel.buildRequest(request), Constants.SHIPMENT);
        return ResponseHelper.buildSuccessResponse();
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = RoutingConstants.ROUTING_LIST_SUCCESSFUL, responseContainer = RoutingConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.SHIPMENT_API_LIST)
    public ResponseEntity<IRunnerResponse> fetchShipmentRoute(@RequestBody @Valid ListCommonRequest listCommonRequest,
                                                              @RequestHeader(value = "x-source", required = false) String xSource) throws RunnerException {
        RoutingListResponse response = routingService.list(listCommonRequest, xSource);
        return ResponseHelper.buildSuccessResponse(response, response.getTotalPages(), response.getTotalCount());
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = RoutingConstants.ROUTING_LIST_SUCCESSFUL, responseContainer = RoutingConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.CONSOLIDATION_API_LIST)
    public ResponseEntity<IRunnerResponse> fetchConsolidationRoute(@RequestBody @Valid ListCommonRequest listCommonRequest,
                                                                   @RequestHeader(value = "x-source", required = false) String xSource) throws RunnerException {
        RoutingListResponse response = routingService.list(listCommonRequest, xSource);
        return ResponseHelper.buildSuccessResponse(response, response.getTotalPages(), response.getTotalCount());
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = RoutingConstants.ROUTING_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = RoutingConstants.ROUTING_ID, required = true) @RequestParam Long id,
                                                        @RequestHeader(value = "x-source", required = false) String xSource) throws RunnerException {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return ResponseHelper.buildSuccessResponse(routingService.retrieveById(CommonRequestModel.buildRequest(request), xSource));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = RoutingConstants.ROUTINGS_UPDATE_SUCCESS, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.SHIPMENT_API_UPDATE)
    public ResponseEntity<IRunnerResponse> shipmentUpdate(@RequestBody @Valid RoutingsRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(routingService.update(CommonRequestModel.buildRequest(request), Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = RoutingConstants.ROUTINGS_UPDATE_SUCCESS, response = BulkRoutingResponse.class)})
    @PutMapping(value = ApiConstants.SHIPMENT_API_UPDATE_BULK)
    public ResponseEntity<IRunnerResponse> shipmentUpdateBulk(@RequestBody BulkUpdateRoutingsRequest request) throws RunnerException {
        if (TransportInfoStatus.IH.equals(request.getTransportInfoStatus())) {
            throw new ValidationException("Transport info status can not be IH");
        }
        return ResponseHelper.buildSuccessResponse(routingService.updateBulk(request, Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = RoutingConstants.ROUTINGS_UPDATE_SUCCESS, response = BulkRoutingResponse.class)})
    @PutMapping(value = ApiConstants.CONSOLIDATION_API_UPDATE_BULK)
    public ResponseEntity<IRunnerResponse> consolidationUpdateBulk(@RequestBody BulkUpdateRoutingsRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(routingService.updateBulk(request, Constants.CONSOLIDATION));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = RoutingConstants.ROUTING_DELETE_SUCCESSFUL, response = BulkRoutingResponse.class)})
    @DeleteMapping(value = ApiConstants.SHIPMENT_API_DELETE_BULK)
    public ResponseEntity<IRunnerResponse> shipmentDeleteBulk(@RequestBody List<RoutingsRequest> request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(routingService.deleteBulk(request, Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
    @GetMapping(ApiConstants.GET_ALL_MASTER_DATA)
    public ResponseEntity<IRunnerResponse> getAllMasterData(@RequestParam Long routingId,
                                                            @RequestHeader(value = "x-source", required = false) String xSource) {
        return ResponseHelper.buildSuccessResponse(routingService.getAllMasterData(CommonRequestModel.buildRequest(routingId), xSource));
    }


}
