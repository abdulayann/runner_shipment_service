package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.CompleteShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.response.CompleteShipmentResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;
import java.util.concurrent.ExecutionException;

@RestController
@RequestMapping(ShipmentConstants.SHIPMENT_API_HANDLE)
@Slf4j
public class ShipmentController {

    @Autowired
    private IShipmentService shipmentService;

    @ApiResponses(value = {@ApiResponse(code = 200, message = "Successful Shipment Details Data List Retrieval", responseContainer = "List")})
    @PostMapping(value = "/list-shipment")
    public ResponseEntity<RunnerListResponse<ShipmentDetailsResponse>> fetchByQuery(@RequestBody @NonNull ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<ShipmentDetailsResponse>>) shipmentService.fetchShipments(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @PostMapping(value = "/create-test-shipment/{count}")
    public ResponseEntity<?> createTestRecord(@PathVariable Integer count) {
        ResponseEntity<List<ShipmentDetails>> response = ResponseEntity.status(HttpStatus.OK)
                .body(shipmentService.createTestShipment(count));
        return response;
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //TODO-Authorization
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<ShipmentDetailsResponse>> create(@RequestBody @Valid ShipmentRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<ShipmentDetailsResponse>>) shipmentService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ShipmentDetailsResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) shipmentService.delete(CommonRequestModel.buildRequest(request));
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //TODO-Authorization
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, responseContainer = ShipmentConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<ShipmentDetailsResponse>> list(@RequestBody ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<ShipmentDetailsResponse>>) shipmentService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //TODO-Authorization
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<ShipmentDetailsResponse>> retrieveById(@ApiParam(value = ShipmentConstants.SHIPMENT_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<ShipmentDetailsResponse>>) shipmentService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_COMPLETE_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<CompleteShipmentResponse>> completeRetrieveById(@ApiParam(value = ShipmentConstants.SHIPMENT_ID, required = true) @RequestParam Long id) throws ExecutionException, InterruptedException {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<CompleteShipmentResponse>>) shipmentService.completeRetrieveById(CommonRequestModel.buildRequest(request));
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //TODO-Authorization
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid ShipmentRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) shipmentService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //TODO-Authorization
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_COMPLETE_UPDATE)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid CompleteShipmentRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) shipmentService.completeUpdate(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }
}
