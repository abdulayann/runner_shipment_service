package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PickupDeliveryDetailsConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IPickupDeliveryDetailsService;
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

@SuppressWarnings("ALL")
@Slf4j
@RestController
@RequestMapping(value = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_API_HANDLE)
public class PickupDeliveryDetailsController {
    @Autowired
    private IPickupDeliveryDetailsService pickupDeliveryDetailsService;

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_CREATE_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid PickupDeliveryDetailsRequest request) {
        String responseMsg;
        try {
            return pickupDeliveryDetailsService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return pickupDeliveryDetailsService.delete(CommonRequestModel.buildRequest(request));
    }


    @ApiResponses(value = {
            @ApiResponse(
                    responseCode = "200",
                    description = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_LIST_SUCCESSFUL,
                    content = @Content(
                            mediaType = "application/json",
                            array = @ArraySchema(schema = @Schema(implementation = IRunnerResponse.class))
                    )
            )
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return pickupDeliveryDetailsService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@Parameter(description = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return pickupDeliveryDetailsService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid PickupDeliveryDetailsRequest request) {
        String responseMsg;
        try {
            return pickupDeliveryDetailsService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }


    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_CREATE_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_API_CREATE_V2)
    public ResponseEntity<IRunnerResponse> createV2(@RequestBody @Valid PickupDeliveryDetailsRequest request) {
        String responseMsg;
        try {
            return pickupDeliveryDetailsService.createV2(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_DELETE_SUCCESSFUL)})
    @DeleteMapping(PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_API_DELETE_V2)
    public ResponseEntity<IRunnerResponse> deleteV2(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return pickupDeliveryDetailsService.deleteV2(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {
            @ApiResponse(
                    responseCode = "200",
                    description = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_LIST_SUCCESSFUL,
                    content = @Content(
                            mediaType = "application/json",
                            array = @ArraySchema(schema = @Schema(implementation = IRunnerResponse.class))
                    )
            )
    })@PostMapping(PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_API_LIST_V2)
    public ResponseEntity<IRunnerResponse> listV2(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return pickupDeliveryDetailsService.listV2(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_API_RETRIEVE_BY_ID_V2)
    public ResponseEntity<IRunnerResponse> retrieveByIdV2(@Parameter(description = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_ID, required = true) @RequestParam Long id, @Parameter(description = "Populate RAKC", required = false) @RequestParam boolean populateRAKC) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return pickupDeliveryDetailsService.retrieveByIdV2(CommonRequestModel.buildRequest(request), populateRAKC);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
    @PutMapping(PickupDeliveryDetailsConstants.PICKUP_DELIVERY_DETAILS_API_UPDATE_V2)
    public ResponseEntity<IRunnerResponse> updateV2(@RequestBody @Valid PickupDeliveryDetailsRequest request) {
        String responseMsg;
        try {
            return pickupDeliveryDetailsService.updateV2(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
