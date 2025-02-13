package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@Generated
public interface IHblRepository extends MultiTenancyRepository<Hbl> {
    List<Hbl> findByShipmentId(Long shipmentId);

    default Optional<Hbl> findById(Long id) {
        Specification<Hbl> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
}
