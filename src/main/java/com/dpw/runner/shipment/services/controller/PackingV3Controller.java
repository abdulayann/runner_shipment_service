package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ContainerConstants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkPackingResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping(PackingConstants.PACKING_V3_API_HANDLE)
@Slf4j
public class PackingV3Controller {

    private static class MyResponseClass extends RunnerResponse<PackingResponse>{}
    private static class MyListResponseClass extends RunnerListResponse<PackingResponse> {}

    private JsonHelper jsonHelper;
    private IPackingV3Service packingV3Service;

    public PackingV3Controller(JsonHelper jsonHelper, IPackingV3Service packingV3Service) {
        this.jsonHelper = jsonHelper;
        this.packingV3Service = packingV3Service;
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_CREATE_SUCCESSFUL, response = PackingV3Controller.MyResponseClass.class),
            @ApiResponse(code = 404, message = ContainerConstants.NO_DATA, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.SHIPMENT + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createFromShipment(@Valid @RequestBody PackingV3Request request) {
        log.info("Received Packing Create request from Shipment with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(packingV3Service.create(request, Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_CREATE_SUCCESSFUL, response = PackingV3Controller.MyResponseClass.class),
            @ApiResponse(code = 404, message = ContainerConstants.NO_DATA, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.CONSOLIDATION + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createFromConsolidation(@Valid @RequestBody PackingV3Request request) {
        log.info("Received Packing Create request from Consolidation with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(packingV3Service.create(request, Constants.CONSOLIDATION));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_CREATE_SUCCESSFUL, response = PackingV3Controller.MyResponseClass.class),
            @ApiResponse(code = 404, message = ContainerConstants.NO_DATA, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.CUSTOMER_BOOKING + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createFromCustomerBooking(@Valid @RequestBody PackingV3Request request) {
        log.info("Received Packing Create request from CustomerBooking with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(packingV3Service.create(request, Constants.BOOKING));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_UPDATE_SUCCESSFUL, response = PackingV3Controller.MyResponseClass.class)})
    @PutMapping(value = ApiConstants.SHIPMENT + ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> updateFromShipment(@RequestBody PackingV3Request request) throws RunnerException {
        log.info("Received Packing Update request from Shipment with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(packingV3Service.update(request, Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_UPDATE_SUCCESSFUL, response = PackingV3Controller.MyResponseClass.class)})
    @PutMapping(value = ApiConstants.CONSOLIDATION + ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> updateFromConsolidation(@RequestBody PackingV3Request request) throws RunnerException {
        log.info("Received Packing Update request from Consolidation with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(packingV3Service.update(request, Constants.CONSOLIDATION));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_UPDATE_SUCCESSFUL, response = PackingV3Controller.MyResponseClass.class)})
    @PutMapping(value = ApiConstants.CUSTOMER_BOOKING + ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> updateFromCustomerBooking(@RequestBody PackingV3Request request) throws RunnerException {
        log.info("Received Packing Update request from CustomerBooking with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(packingV3Service.update(request, Constants.BOOKING));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @DeleteMapping(ApiConstants.SHIPMENT + ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> deleteFromShipment(@RequestParam @Valid Long id) {
        return ResponseHelper.buildSuccessResponse(packingV3Service.delete(id, Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @DeleteMapping(ApiConstants.CONSOLIDATION + ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> deleteFromConsolidation(@RequestParam @Valid Long id) {
        return ResponseHelper.buildSuccessResponse(packingV3Service.delete(id, Constants.CONSOLIDATION));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @DeleteMapping(ApiConstants.CUSTOMER_BOOKING + ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> deleteFromCustomerBooking(@RequestParam @Valid Long id) {
        return ResponseHelper.buildSuccessResponse(packingV3Service.delete(id, Constants.BOOKING));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_UPDATE_SUCCESSFUL, response = BulkPackingResponse.class)})
    @PutMapping(value = ApiConstants.SHIPMENT + ApiConstants.API_UPDATE_BULK)
    public ResponseEntity<IRunnerResponse> updateBulkFromShipment(@RequestBody List<PackingV3Request> request) {
        return ResponseHelper.buildSuccessResponse(packingV3Service.updateBulk(request, Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_UPDATE_SUCCESSFUL, response = BulkPackingResponse.class)})
    @PutMapping(value = ApiConstants.CONSOLIDATION + ApiConstants.API_UPDATE_BULK)
    public ResponseEntity<IRunnerResponse> updateBulkFromConsolidation(@RequestBody List<PackingV3Request> request) {
        return ResponseHelper.buildSuccessResponse(packingV3Service.updateBulk(request, Constants.CONSOLIDATION));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_UPDATE_SUCCESSFUL, response = BulkPackingResponse.class)})
    @PutMapping(value = ApiConstants.CUSTOMER_BOOKING + ApiConstants.API_UPDATE_BULK)
    public ResponseEntity<IRunnerResponse> updateBulkFromCustomerBooking(@RequestBody List<PackingV3Request> request) {
        return ResponseHelper.buildSuccessResponse(packingV3Service.updateBulk(request, Constants.BOOKING));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_DELETE_SUCCESSFUL, response = BulkPackingResponse.class)})
    @DeleteMapping(value = ApiConstants.SHIPMENT + ApiConstants.API_DELETE_BULK)
    public ResponseEntity<IRunnerResponse> deleteBulkFromShipment(@RequestBody List<PackingV3Request> request) {
        return ResponseHelper.buildSuccessResponse(packingV3Service.deleteBulk(request, Constants.SHIPMENT));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_DELETE_SUCCESSFUL, response = BulkPackingResponse.class)})
    @DeleteMapping(value = ApiConstants.CONSOLIDATION + ApiConstants.API_DELETE_BULK)
    public ResponseEntity<IRunnerResponse> deleteBulkFromConsolidation(@RequestBody List<PackingV3Request> request) {
        return ResponseHelper.buildSuccessResponse(packingV3Service.deleteBulk(request, Constants.CONSOLIDATION));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_DELETE_SUCCESSFUL, response = BulkPackingResponse.class)})
    @DeleteMapping(value = ApiConstants.CUSTOMER_BOOKING + ApiConstants.API_DELETE_BULK)
    public ResponseEntity<IRunnerResponse> deleteBulkFromCustomerBooking(@RequestBody List<PackingV3Request> request) {
        return ResponseHelper.buildSuccessResponse(packingV3Service.deleteBulk(request, Constants.BOOKING));
    }

    @GetMapping(ApiConstants.API_DOWNLOAD)
    public void downloadCSV(HttpServletResponse response, @ModelAttribute BulkDownloadRequest request) throws RunnerException {
        packingV3Service.downloadPacking(response, request);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.RETRIEVE_BY_ID_SUCCESSFUL, response = PackingV3Controller.MyResponseClass.class)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = Constants.ID) @RequestParam Optional<Long> id, @ApiParam(value = Constants.GUID) @RequestParam Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return ResponseHelper.buildSuccessResponse(packingV3Service.retrieveById(CommonRequestModel.buildRequest(request)));
    }

}
