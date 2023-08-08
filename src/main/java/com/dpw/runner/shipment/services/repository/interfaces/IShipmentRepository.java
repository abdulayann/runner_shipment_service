package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.nimbusds.jose.util.Pair;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;


@Repository
public interface IShipmentRepository extends MultiTenancyRepository<ShipmentDetails> {
    List<ShipmentDetails> findAll();
    Page<ShipmentDetails> findAll(Specification<ShipmentDetails> spec, Pageable pageable);
    default Optional<ShipmentDetails> findById(Long id) {
        Specification<ShipmentDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    default Optional<ShipmentDetails> findByGuid(UUID id) {
        Specification<ShipmentDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }
    Optional<ShipmentDetails> findByHouseBill(String Hbl);
}
