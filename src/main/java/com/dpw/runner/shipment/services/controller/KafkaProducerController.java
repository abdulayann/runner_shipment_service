package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.service.impl.KafkaProducerService;
import org.apache.kafka.common.protocol.types.Field;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = "/kafka")
public class KafkaProducerController {

    private final KafkaProducerService producerService;

    @Autowired
    public KafkaProducerController(KafkaProducerService producerService) {
        this.producerService = producerService;
    }


    @PostMapping(value = "/publish")
    public ResponseEntity<?> sendMessageToKafkaTopic(@RequestParam("message") String message){
        this.producerService.sendMessage("Hello -> " +  message);
        return ResponseEntity.status(HttpStatus.OK).body(String.format("Hello with the message : %s ", message));
    }
}
