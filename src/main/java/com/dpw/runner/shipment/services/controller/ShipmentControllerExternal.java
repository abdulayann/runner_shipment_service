package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.Optional;

import static com.dpw.runner.shipment.services.commons.constants.Constants.SOURCE_SERVICE_TYPE;

@RestController
@RequestMapping(ShipmentConstants.SHIPMENT_EXTERNAL_API_HANDLE)
@Slf4j
public class ShipmentControllerExternal {
    private IShipmentServiceV3 shipmentService;
    private JsonHelper jsonHelper;

    @Autowired
    public ShipmentControllerExternal(IShipmentServiceV3 shipmentService, JsonHelper jsonHelper) {
        this.shipmentService = shipmentService;
        this.jsonHelper = jsonHelper;
    }

    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        log.info("Received shipment list v3 external request with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
        return shipmentService.listShipment(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = ShipmentConstants.SHIPMENT_ID) @RequestParam Optional<Long> id,
                                                        @ApiParam(value = ShipmentConstants.SHIPMENT_GUID) @RequestParam Optional<String> guid,
                                                        @RequestHeader(SOURCE_SERVICE_TYPE) String source) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        log.info("Received Shipment retrieve request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return shipmentService.retrieveShipmentDataByIdExternal(CommonRequestModel.buildRequest(request), source);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@RequestBody @Valid CommonGetRequest retrieveCommonRequest,
                                                        @RequestHeader(SOURCE_SERVICE_TYPE) String source) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        log.info("Received Shipment retrieve request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return shipmentService.retrieveShipmentDataByIdUsingIncludeColumns(CommonRequestModel.buildRequest(retrieveCommonRequest), source);
    }

    @PostMapping(ApiConstants.API_DYNAMIC_LIST)
    public ResponseEntity<IRunnerResponse> getShipmentList(@RequestBody @Valid ListCommonRequest listCommonRequest) throws RunnerException {
        return shipmentService.fetchShipments(listCommonRequest);
    }
    @PostMapping(ApiConstants.API_DYNAMIC_RETRIEVE)
    public  ResponseEntity<IRunnerResponse> retrieveShipmentDetails(@RequestBody @Valid CommonGetRequest commonGetRequest) throws RunnerException {
        if(commonGetRequest.getId() == null && commonGetRequest.getGuid() ==null) {
            throw new ValidationException("Id or Guid is mandatory");
        }
        return shipmentService.getShipmentDetails(commonGetRequest);
    }
}
