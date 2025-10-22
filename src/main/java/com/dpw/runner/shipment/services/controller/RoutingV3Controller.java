package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.RoutingConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.BulkUpdateRoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.UpdateTransportStatusRequest;
import com.dpw.runner.shipment.services.dto.response.RoutingListResponse;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.BulkRoutingResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsV3Service;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping(RoutingConstants.ROUTING_API_HANDLE_V3)
@Slf4j
public class RoutingV3Controller {

    private final IRoutingsV3Service routingService;

    @Autowired
    public RoutingV3Controller(IRoutingsV3Service routingService) {
        this.routingService = routingService;
    }


    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = RoutingConstants.ROUTING_CREATE_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.SHIPMENT_API_CREATE)
    public ResponseEntity<IRunnerResponse> shipmentCreate(@RequestBody @Valid RoutingsRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(routingService.create(CommonRequestModel.buildRequest(request), Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = RoutingConstants.ROUTING_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.SHIPMENT_API_DELETE)
    public ResponseEntity<IRunnerResponse> shipmentDelete(@RequestParam @Valid Long id) throws RunnerException {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        routingService.delete(CommonRequestModel.buildRequest(request), Constants.SHIPMENT);
        return ResponseHelper.buildSuccessResponse();
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = RoutingConstants.ROUTING_LIST_SUCCESSFUL, content = @Content( array = @ArraySchema(schema = @Schema(implementation = RoutingsResponse.class))))})
    @PostMapping(ApiConstants.SHIPMENT_API_LIST)
    public ResponseEntity<IRunnerResponse> fetchShipmentRoute(@RequestBody @Valid ListCommonRequest listCommonRequest,
                                                              @RequestHeader(value = "x-source", required = false) String xSource) throws RunnerException {
        RoutingListResponse response = routingService.list(listCommonRequest, xSource);
        return ResponseHelper.buildSuccessResponse(response, response.getTotalPages(), response.getTotalCount());
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = RoutingConstants.ROUTING_LIST_SUCCESSFUL, content = @Content( array = @ArraySchema(schema = @Schema(implementation = RoutingsResponse.class))))})
    @PostMapping(ApiConstants.CONSOLIDATION_API_LIST)
    public ResponseEntity<IRunnerResponse> fetchConsolidationRoute(@RequestBody @Valid ListCommonRequest listCommonRequest,
                                                                   @RequestHeader(value = "x-source", required = false) String xSource) throws RunnerException {
        RoutingListResponse response = routingService.list(listCommonRequest, xSource);
        return ResponseHelper.buildSuccessResponse(response, response.getTotalPages(), response.getTotalCount());
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = RoutingConstants.ROUTING_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@Parameter(description = RoutingConstants.ROUTING_ID, required = true) @RequestParam Long id,
                                                        @RequestHeader(value = "x-source", required = false) String xSource) throws RunnerException {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return ResponseHelper.buildSuccessResponse(routingService.retrieveById(CommonRequestModel.buildRequest(request), xSource));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = RoutingConstants.ROUTINGS_UPDATE_SUCCESS, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
    @PutMapping(ApiConstants.SHIPMENT_API_UPDATE)
    public ResponseEntity<IRunnerResponse> shipmentUpdate(@RequestBody @Valid RoutingsRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(routingService.update(CommonRequestModel.buildRequest(request), Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = RoutingConstants.ROUTINGS_UPDATE_SUCCESS, content = @Content(schema = @Schema(implementation = BulkRoutingResponse.class)))})
    @PutMapping(value = ApiConstants.SHIPMENT_API_UPDATE_BULK)
    public ResponseEntity<IRunnerResponse> shipmentUpdateBulk(@RequestBody BulkUpdateRoutingsRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(routingService.bulkUpdateWithValidateWrapper(request, Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = RoutingConstants.ROUTINGS_UPDATE_SUCCESS, content = @Content(schema = @Schema(implementation = BulkRoutingResponse.class)))})
    @PutMapping(value = ApiConstants.CONSOLIDATION_API_UPDATE_BULK)
    public ResponseEntity<IRunnerResponse> consolidationUpdateBulk(@RequestBody BulkUpdateRoutingsRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(routingService.bulkUpdateWithValidateWrapper(request, Constants.CONSOLIDATION));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = RoutingConstants.ROUTING_DELETE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = BulkRoutingResponse.class)))})
    @DeleteMapping(value = ApiConstants.SHIPMENT_API_DELETE_BULK)
    public ResponseEntity<IRunnerResponse> shipmentDeleteBulk(@RequestBody List<RoutingsRequest> request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(routingService.deleteBulk(request, Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = ShipmentConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
    @GetMapping(ApiConstants.GET_ALL_MASTER_DATA)
    public ResponseEntity<IRunnerResponse> getAllMasterData(@RequestParam Long routingId,
                                                            @RequestHeader(value = "x-source", required = false) String xSource) {
        return ResponseHelper.buildSuccessResponse(routingService.getAllMasterData(CommonRequestModel.buildRequest(routingId), xSource));
    }
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = RoutingConstants.ROUTINGS_UPDATE_TRANSPORT_STATUS_SUCCESS, content = @Content(schema = @Schema(implementation = BulkRoutingResponse.class)))})
    @PutMapping(value = ApiConstants.SHIPMENT_API_UPDATE_TRANSPORT_INFO_STATUS)
    public ResponseEntity<IRunnerResponse> shipmentConsolUpdateTransportInfoStatus(@Valid @RequestBody UpdateTransportStatusRequest request) throws RunnerException {
        return ResponseHelper.buildSuccessResponse(routingService.updateTransportInfoStatus(request));
    }


}
