package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IEventDumpDao;
import com.dpw.runner.shipment.services.entity.EventsDump;
import com.dpw.runner.shipment.services.repository.interfaces.IEventDumpRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public class EventDumpDao implements IEventDumpDao {

    private IEventDumpRepository eventDumpRepository;

    @Autowired
    public EventDumpDao(IEventDumpRepository eventDumpRepository) {
        this.eventDumpRepository = eventDumpRepository;
    }

    @Override
    public Page<EventsDump> findAll(Specification<EventsDump> spec, Pageable pageable) {
        return eventDumpRepository.findAll(spec, pageable);
    }

    @Override
    public List<EventsDump> saveAll(List<EventsDump> eventsList) {
        return eventDumpRepository.saveAll(eventsList);
    }
}
