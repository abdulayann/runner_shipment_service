package com.dpw.runner.booking.services.repository.interfaces;

import com.dpw.runner.booking.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.booking.services.entity.EventsDump;
import com.dpw.runner.booking.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

@Repository
@Generated
public interface IEventDumpRepository extends MultiTenancyRepository<EventsDump> {
    Page<EventsDump> findAll(Specification<EventsDump> spec, Pageable pageable);
}
