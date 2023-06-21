package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.BookingCarriageConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.Pageable;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.response.BookingCarriageResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.NonNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
public class ShipmentController {

    @Autowired
    private IShipmentService shipmentService;

    @ApiResponses(value = { @ApiResponse(code = 200, message = BookingCarriageConstants.BOOKING_CARRIAGE_LIST_SUCCESSFUL, responseContainer = BookingCarriageConstants.RESPONSE_CONTAINER_LIST) })
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
}
