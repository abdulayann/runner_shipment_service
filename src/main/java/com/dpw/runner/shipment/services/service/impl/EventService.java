package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.repository.interfaces.IEventsDao;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class EventService implements IEventService {

    @Autowired
    IEventsDao eventsDao;

    @Override
    public List<Events> fetchEvents(Long shipmentId) {
        // Find all entries with given shipment ID from the event table
        List<Events> response = eventsDao.findByShipmentId(shipmentId);
        return response;
    }

    @Override
    public void createEvent(List<Events> event) {
        eventsDao.saveAll(event);
    }

    public void updateEvent(Long id, Events updatedEvent) {
        eventsDao.save(updatedEvent);
    }

    @Override
    public void deleteEvent(List<Events> events) {
        eventsDao.deleteAll(events);
    }

}
