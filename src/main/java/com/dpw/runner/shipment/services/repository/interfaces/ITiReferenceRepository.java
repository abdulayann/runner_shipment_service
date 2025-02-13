package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.TiReferences;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@Generated
public interface ITiReferenceRepository extends MultiTenancyRepository<TiReferences> {
    List<TiReferences> findAll();

    Page<TiReferences> findAll(Specification<TiReferences> spec, Pageable pageable);

    default Optional<TiReferences> findById(Long id) {
        Specification<TiReferences> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    List<TiReferences> findByIdIn(List<Long> ids);
}
