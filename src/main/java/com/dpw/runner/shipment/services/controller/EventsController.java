package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;


@Slf4j
@RestController
@RequestMapping(value = EventConstants.EVENT_API_HANDLE)
public class EventsController {

    @Autowired
    private IEventService eventService;

    @GetMapping(value = ApiConstants.API_LIST)
    public ResponseEntity<?> listEvents(@RequestBody EventsRequest request) {
        return eventService.list(request);
    }

    @PostMapping(value = ApiConstants.API_CREATE)
    public ResponseEntity<?> createEvent(@RequestBody EventsRequest request) {
        ResponseEntity<EventsResponse> response = (ResponseEntity<EventsResponse>) eventService.create(request);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @PutMapping(value = ApiConstants.API_UPDATE)
    public ResponseEntity<?> updateEvent(@RequestBody List<EventsRequest> request) {
        return null;
    }

    @DeleteMapping(value = ApiConstants.API_DELETE)
    public ResponseEntity<?> deleteEvent(@RequestBody EventsRequest request) {
        return null;
    }


}
