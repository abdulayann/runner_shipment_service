package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.InterBranchEntity;
import org.springframework.data.domain.Page;


import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
@Generated
@InterBranchEntity
public interface IRoutingsRepository extends MultiTenancyRepository<Routings> {
    Page<Routings> findAll(Specification<Routings> spec, Pageable pageable);

    List<Routings> findByShipmentId(Long shipmentId);

    default Optional<Routings> findById(Long id) {
        Specification<Routings> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    List<Routings> findAll();

    default Optional<Routings> findByGuid(UUID id) {
        Specification<Routings> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }

    List<Routings> findByConsolidationId(Long consolidationId);
}
