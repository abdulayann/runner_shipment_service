package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Allocations;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;


@Repository
@Generated
public interface IAllocationsRepository extends MultiTenancyRepository<Allocations> {
    List<Allocations> findAll();

    Page<Allocations> findAll(Specification<Allocations> spec, Pageable pageable);

    default Optional<Allocations> findById(Long id) {
        Specification<Allocations> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
}
