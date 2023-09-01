package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface ITruckDriverDetailsRepository extends MultiTenancyRepository<TruckDriverDetails> {
    Page<TruckDriverDetails> findAll(Specification<TruckDriverDetails> spec, Pageable pageable);
    default Optional<TruckDriverDetails> findById(Long id) {
        Specification<TruckDriverDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    List<TruckDriverDetails> findAll();
}
