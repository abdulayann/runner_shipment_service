package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IShippingInstructionRepository extends MultiTenancyRepository<ShippingInstruction> {
    List<ShippingInstruction> findAll();

    Page<ShippingInstruction> findAll(Specification<ShippingInstruction> spec, Pageable pageable);

    default Optional<ShippingInstruction> findByGuid(UUID id) {
        Specification<ShippingInstruction> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }
}

