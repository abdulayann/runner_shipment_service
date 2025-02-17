package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ELDetails;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Generated
public interface IELDetailsRepository extends MultiTenancyRepository<ELDetails> {
    Page<ELDetails> findAll(Specification<ELDetails> spec, Pageable pageable);

    Optional<ELDetails> findByElNumber(String elNumber);

    List<ELDetails> findByShipmentId(Long shipmentId);

    default Optional<ELDetails> findById(Long id) {
        Specification<ELDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    default Optional<ELDetails> findByGuid(UUID id) {
        Specification<ELDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }

    List<ELDetails> findAll();

}
