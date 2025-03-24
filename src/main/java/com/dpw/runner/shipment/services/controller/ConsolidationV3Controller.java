package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
@RequestMapping(ConsolidationConstants.CONSOLIDATION_V3_API_HANDLE)
@Slf4j
public class ConsolidationV3Controller {

    private final IConsolidationV3Service consolidationV3Service;
    private final JsonHelper jsonHelper;

    private static class MyResponseClass extends RunnerResponse<ConsolidationDetailsResponse> {}

    @Autowired
    public ConsolidationV3Controller(IConsolidationV3Service consolidationV3Service,
                                     JsonHelper jsonHelper) {
        this.consolidationV3Service = consolidationV3Service;
        this.jsonHelper = jsonHelper;
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ConsolidationConstants.CREATE_SUCCESSFUL, response = ConsolidationV3Controller.MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid ConsolidationDetailsRequest request) {
        log.info("Received Consolidation create request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(consolidationV3Service.create(request));
    }

    /**
     * Attaches shipments to a consolidation.
     *
     * @param request Shipment attach request
     * @return Standard runner response
     */
    @ApiResponses(value = {
            @ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.ATTACH_SHIPMENT_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.ATTACH_SHIPMENTS)
    public ResponseEntity<IRunnerResponse> attachShipments(@RequestBody @Valid ShipmentAttachDetachV3Request request) throws RunnerException {
        log.info("Received attachShipments request: {}", request);
        String warning = consolidationV3Service.attachShipments(request);
        return ResponseHelper.buildSuccessResponseWithWarning(warning);
    }

}
