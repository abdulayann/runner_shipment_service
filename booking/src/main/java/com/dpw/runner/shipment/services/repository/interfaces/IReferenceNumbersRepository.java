package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
@Generated
public interface IReferenceNumbersRepository extends MultiTenancyRepository<ReferenceNumbers> {
    Page<ReferenceNumbers> findAll(Specification<ReferenceNumbers> spec, Pageable pageable);
    List<ReferenceNumbers> findByShipmentId(Long shipmentId);
    default Optional<ReferenceNumbers> findById(Long id) {
        Specification<ReferenceNumbers> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    List<ReferenceNumbers> findAll();
}
