package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.TiPackages;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@Generated
public interface ITiPackageRepository extends MultiTenancyRepository<TiPackages> {
    List<TiPackages> findAll();
    Page<TiPackages> findAll(Specification<TiPackages> spec, Pageable pageable);
    default Optional<TiPackages> findById(Long id) {
        Specification<TiPackages> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    List<TiPackages> findByIdIn(List<Long> ids);
}
