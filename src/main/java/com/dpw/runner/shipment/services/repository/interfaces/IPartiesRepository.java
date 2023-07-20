package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.FileRepo;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface IPartiesRepository extends JpaRepository<Parties, Long> {
    Page<Parties> findAll(Specification<Parties> spec, Pageable pageable);
    public List<Parties> findByEntityIdAndEntityType(Long entityId, String entityType);
    Optional<Parties> findByGuid(UUID guid);
}
