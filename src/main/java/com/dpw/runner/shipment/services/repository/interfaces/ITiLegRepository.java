package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
@Generated
public interface ITiLegRepository extends MultiTenancyRepository<TiLegs> {
    List<TiLegs> findAll();

    Page<TiLegs> findAll(Specification<TiLegs> spec, Pageable pageable);

    default Optional<TiLegs> findById(Long id) {
        Specification<TiLegs> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    default Optional<TiLegs> findByGuid(UUID id) {
        Specification<TiLegs> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }

}
