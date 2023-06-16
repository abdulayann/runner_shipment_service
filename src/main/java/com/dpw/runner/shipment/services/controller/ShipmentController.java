package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.Pageable;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.exception.exceptions.FileNotFoundException;
import com.dpw.runner.shipment.services.exception.exceptions.InvalidAccessTokenException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.service.IShipmentService;
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


    @PostMapping(value = "/list-shipment")
    public ResponseEntity<?> fetchByQuery(@RequestBody @NonNull Pageable pageable) {
        ResponseEntity<RunnerResponse> response = ResponseEntity.status(HttpStatus.OK).body(shipmentService.fetchShipments(pageable));
        return response;
    }

    @PostMapping(value = "/create-test-shipment/{count}")
    public ResponseEntity<?> createTestRecord(@PathVariable Integer count) {
        ResponseEntity<List<ShipmentDetails>> response = ResponseEntity.status(HttpStatus.OK)
                .body(shipmentService.createTestShipment(count));
        return response;
    }

    @GetMapping(value = "/InvalidToken")
    public ResponseEntity<?> invalidToken() {
        throw new InvalidAccessTokenException("Failed to validate token");
    }

    @GetMapping(value = "/File/{id}")
    public ResponseEntity<?> accessFile(@PathVariable Integer id) {

        if(id >= 0) {
            ResponseEntity<String> response = ResponseEntity.status(HttpStatus.OK)
                    .body("File Found");
            return response;
        } else {
            throw new FileNotFoundException("File with id = "+id+ " not found.");
        }
   }

    @GetMapping(value = "/generalException")
    public ResponseEntity<?> ruunerException() {
            throw new RunnerException("Runner Exception");
    }

    @GetMapping(value = "/Authenticate")
    public ResponseEntity<?> authentication() {
        throw new RunnerException("Invalid Authentication");
    }
}
