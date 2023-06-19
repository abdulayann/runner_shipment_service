package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.entity.Events;

import java.util.List;

public interface IEventService {

    List<Events> fetchEvents(Long shipmentId);

    void createEvent(List<Events> events);

    void updateEvent(Long id, Events event);

    void deleteEvent(List<Events> events);
}

