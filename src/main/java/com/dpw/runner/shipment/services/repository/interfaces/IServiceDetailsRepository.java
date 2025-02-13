package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

@Generated
public interface IServiceDetailsRepository extends MultiTenancyRepository<ServiceDetails> {
    Page<ServiceDetails> findAll(Specification<ServiceDetails> spec, Pageable pageable);

    List<ServiceDetails> findByShipmentId(Long shipmentId);

    default Optional<ServiceDetails> findById(Long id) {
        Specification<ServiceDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    List<ServiceDetails> findAll();
}
