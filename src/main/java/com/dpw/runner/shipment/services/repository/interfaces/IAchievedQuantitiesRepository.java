package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.AchievedQuantities;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;


@Repository
@Generated
public interface IAchievedQuantitiesRepository extends MultiTenancyRepository<AchievedQuantities> {
    List<AchievedQuantities> findAll();

    Page<AchievedQuantities> findAll(Specification<AchievedQuantities> spec, Pageable pageable);

    default Optional<AchievedQuantities> findById(Long id) {
        Specification<AchievedQuantities> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
}
