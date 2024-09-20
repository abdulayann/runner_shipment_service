package com.dpw.runner.shipment.services.controller;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
public class HealthController {

    @GetMapping
    public ResponseEntity<String> health() {
        return ResponseEntity.status(HttpStatus.OK).body("Shipment Service Up Now");
    }

}
