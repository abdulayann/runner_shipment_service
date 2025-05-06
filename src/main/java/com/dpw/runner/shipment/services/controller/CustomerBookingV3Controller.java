package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkPackingResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

import java.util.List;

import static com.dpw.runner.shipment.services.commons.constants.Constants.BOOKING;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = CustomerBookingConstants.CUSTOMER_BOOKING_API_HANDLE_V3)
public class CustomerBookingV3Controller {

    private class MyResponseClass extends RunnerResponse<ContainerResponse> {}

    private IContainerV3Service containerV3Service;

    private IPackingV3Service packingV3Service;

    @Autowired
    public CustomerBookingV3Controller(IPackingV3Service packingV3Service, IContainerV3Service containerV3Service) {
        this.containerV3Service = containerV3Service;
        this.packingV3Service = packingV3Service;
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_CREATE_SUCCESSFUL, response = MyResponseClass.class)})
    @PostMapping(ApiConstants.BOOKING_API_CREATE_CONTAINERS)
    public ResponseEntity<IRunnerResponse> createBookingContainers(@RequestBody @Valid ContainerV3Request containerV3Request) {
        ContainerResponse containerResponse = containerV3Service.create(containerV3Request, BOOKING);
        return new ResponseEntity(containerResponse, HttpStatus.OK);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_UPDATE_SUCCESSFUL, response = BulkContainerResponse.class)})
    @PostMapping(ApiConstants.BOOKING_API_UPDATE_CONTAINERS)
    public ResponseEntity<IRunnerResponse> updateBookingContainers(@RequestBody @Valid List<ContainerV3Request> containerV3Requests) {
        BulkContainerResponse containerResponse = containerV3Service.updateBulk(containerV3Requests, BOOKING);
        return new ResponseEntity(containerResponse, HttpStatus.OK);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_LIST_SUCCESSFUL, response = ContainerListResponse.class)})
    @GetMapping(ApiConstants.BOOKING_API_LIST_CONTAINERS)
    public ResponseEntity<IRunnerResponse> listBookingContainers(@RequestBody ListCommonRequest listCommonRequest) throws RunnerException {
        ContainerListResponse containerListResponse = containerV3Service.list(listCommonRequest, true);
        return new ResponseEntity(containerListResponse, HttpStatus.OK);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_DELETE_SUCCESSFUL, response = BulkContainerResponse.class)})
    @DeleteMapping(ApiConstants.BOOKING_API_DELETE_CONTAINERS)
    public ResponseEntity<IRunnerResponse> deleteBookingContainers(@RequestBody @Valid List<ContainerV3Request> containerV3Requests) {
        BulkContainerResponse bulkContainerResponse = containerV3Service.deleteBulk(containerV3Requests, BOOKING);
        return new ResponseEntity(bulkContainerResponse, HttpStatus.OK);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_CREATE_SUCCESSFUL, response = PackingResponse.class)})
    @PostMapping(ApiConstants.BOOKING_API_CREATE_PACKAGES)
    public ResponseEntity<IRunnerResponse> createBookingPackages(@RequestBody @Valid PackingV3Request packingV3Request) throws RunnerException {
        PackingResponse packingResponse = packingV3Service.create(packingV3Request, BOOKING);
        return new ResponseEntity(packingResponse, HttpStatus.OK);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_UPDATE_SUCCESSFUL, response = BulkPackingResponse.class)})
    @PostMapping(ApiConstants.BOOKING_API_UPDATE_PACKAGES)
    public ResponseEntity<IRunnerResponse> updateBookingPackages(@RequestBody @Valid List<PackingV3Request> packingV3RequestList) throws RunnerException {
        BulkPackingResponse bulkPackingResponse = packingV3Service.updateBulk(packingV3RequestList, BOOKING);
        return new ResponseEntity(bulkPackingResponse, HttpStatus.OK);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_LIST_SUCCESSFUL)})
    @GetMapping(ApiConstants.BOOKING_API_LIST_PACKAGES)
    public ResponseEntity<IRunnerResponse> listBookingPackages(@RequestBody ListCommonRequest listCommonRequest) {
        PackingListResponse packingListResponse = packingV3Service.list(CommonRequestModel.buildRequest(listCommonRequest), true);
        return new ResponseEntity(packingListResponse, HttpStatus.OK);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PackingConstants.PACKING_DELETE_SUCCESSFUL, response = BulkPackingResponse.class)})
    @DeleteMapping(ApiConstants.BOOKING_API_DELETE_PACKAGES)
    public ResponseEntity<IRunnerResponse> deleteBookingPackages(@RequestBody @Valid List<PackingV3Request> packingV3RequestList) {
        BulkPackingResponse response = packingV3Service.deleteBulk(packingV3RequestList, BOOKING);
        return new ResponseEntity(response, HttpStatus.OK);
    }
}
