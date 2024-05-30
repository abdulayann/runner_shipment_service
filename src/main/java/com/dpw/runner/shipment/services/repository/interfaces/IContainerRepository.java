package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Containers;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;


public interface IContainerRepository extends MultiTenancyRepository<Containers> {
    List<Containers> findAll();
    Page<Containers> findAll(Specification<Containers> spec, Pageable pageable);
    List<Containers> findByGuid(UUID guid);
    default Optional<Containers> findById(Long id) {
        Specification<Containers> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    List<Containers> findByConsolidationId(Long consolidationId);
    void deleteById(Long id);
}
