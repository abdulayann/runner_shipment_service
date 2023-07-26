package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Notes;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface INotesRepository extends MultiTenancyRepository<Notes> {

    Page<Notes> findAll(Specification<Notes> left, Pageable right);
    Optional<Notes> findById(Long id);
    public List<Notes> findByEntityIdAndEntityType(Long entityId, String entityType);
    List<Notes> findAll();

}
