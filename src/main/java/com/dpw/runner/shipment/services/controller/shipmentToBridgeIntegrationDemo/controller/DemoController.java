package com.dpw.runner.shipment.services.controller.shipmentToBridgeIntegrationDemo.controller;

import com.dpw.runner.shipment.services.controller.shipmentToBridgeIntegrationDemo.DTOs.RootPayload;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.log4j.Log4j2;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/demo/payload")
@Log4j2
public class DemoController {

    private final ObjectMapper objectMapper;

    public DemoController(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    @PostMapping
    public String receivePayload(@RequestBody RootPayload payload) throws JsonProcessingException {
        String json = objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(payload);
        log.info("Received Payload:\n" + json);
        return "Payload received and printed to console.";
    }
}
