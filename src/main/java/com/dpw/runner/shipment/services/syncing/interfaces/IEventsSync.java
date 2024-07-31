package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.commons.entity.Events;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IEventsSync {
    ResponseEntity<?> sync(List<Events> eventsList);
}
