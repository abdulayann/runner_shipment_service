package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.Jobs;
import com.dpw.runner.shipment.services.entity.Notes;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface INotesRepository extends JpaRepository<Notes, Long> {

    Page<Notes> findAll(Specification<Notes> left, Pageable right);
    public List<Notes> findByEntityIdAndEntityType(Long entityId, String entityType);
    Optional<Notes> findByGuid(UUID guid);
}
