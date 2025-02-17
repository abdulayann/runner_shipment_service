package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Jobs;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@Generated
public interface IJobRepository extends MultiTenancyRepository<Jobs> {
    List<Jobs> findByShipmentId(Long id);

    default Optional<Jobs> findById(Long id) {
        Specification<Jobs> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    List<Jobs> findAll();

    Page<Jobs> findAll(Specification<Jobs> spec, Pageable pageable);
}
