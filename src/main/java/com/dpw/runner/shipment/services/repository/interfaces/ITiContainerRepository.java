package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.TiContainers;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@Generated
public interface ITiContainerRepository extends MultiTenancyRepository<TiContainers> {
    List<TiContainers> findAll();
    Page<TiContainers> findAll(Specification<TiContainers> spec, Pageable pageable);
    default Optional<TiContainers> findById(Long id) {
        Specification<TiContainers> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    List<TiContainers> findByIdIn(List<Long> ids);
}
