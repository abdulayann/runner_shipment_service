package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
public class EventController {

    private static final Logger LOG = LoggerFactory.getLogger(EventController.class);
    @Autowired
    IEventService eventService;

    @GetMapping(value = "/events/list")
    public ResponseEntity<?> sample(@RequestParam Long shipmentId) {
        List<Events> events = eventService.fetchEvents(shipmentId);
        ResponseEntity<List<Events>> response = ResponseEntity.status(HttpStatus.OK)
                .body(events);
        return response;
    }

    @PostMapping(value = "/events/create")
    public ResponseEntity<?> createEvent(@RequestBody List<Events> events) {
        LOG.info("input received : {}", events);
        eventService.createEvent(events);
        ResponseEntity<String> response = ResponseEntity.status(HttpStatus.OK)
                .body("Successfully created events !");
        return response;
    }

    @PostMapping(value = "/events/update/{id}")
    public ResponseEntity<?> updateEvent(@PathVariable("id") Long id, @RequestBody Events event) {
        eventService.updateEvent(id, event);
        ResponseEntity<Events> response = ResponseEntity.status(HttpStatus.OK).body(event);
        return response;
    }

    @DeleteMapping(value = "/events/delete")
    public ResponseEntity<?> deleteEvents(@RequestBody List<Events> events){
        eventService.deleteEvent(events);
        ResponseEntity<String> response = ResponseEntity.status(HttpStatus.OK)
                .body("Events deleted successfully !");
        return response;
    }

}
