package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.concurrent.ExecutionException;

@RestController
@RequestMapping(ShipmentConstants.SHIPMENT_API_HANDLE)
@Slf4j
public class ShipmentController {

    @Autowired
    private IShipmentService shipmentService;

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_COMPLETE_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<ShipmentDetailsResponse>> completeRetrieveById(@ApiParam(value = ShipmentConstants.SHIPMENT_ID, required = true) @RequestParam Long id) throws ExecutionException, InterruptedException {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<ShipmentDetailsResponse>>) shipmentService.completeRetrieveById(CommonRequestModel.buildRequest(request));
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //TODO-Authorization
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_COMPLETE_UPDATE)
    public ResponseEntity<RunnerResponse> completeUpdate(@RequestBody @Valid ShipmentRequest request) {
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
