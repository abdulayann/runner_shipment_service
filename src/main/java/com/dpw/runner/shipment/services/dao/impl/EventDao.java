package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.repository.interfaces.IEventRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class EventDao implements IEventDao {
    @Autowired
    private IEventRepository eventRepository;

    @Override
    public Events save(Events events) {
        return eventRepository.save(events);
    }

    @Override
    public Page<Events> findAll(Specification<Events> spec, Pageable pageable) {
        return eventRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Events> findById(Long id) {
        return eventRepository.findById(id);
    }

    @Override
    public void delete(Events events) {
        eventRepository.delete(events);
    }
}
