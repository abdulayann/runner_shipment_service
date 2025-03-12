package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.Events;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IEventsSync {
    ResponseEntity<IRunnerResponse> sync(List<Events> eventsList);
}
