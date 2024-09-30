package com.dpw.runner.booking.services.dao.interfaces;

import com.dpw.runner.booking.services.entity.EventsDump;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;

public interface IEventDumpDao {
    Page<EventsDump> findAll(Specification<EventsDump> spec, Pageable pageable);
    List<EventsDump> saveAll(List<EventsDump> eventsList);
}
