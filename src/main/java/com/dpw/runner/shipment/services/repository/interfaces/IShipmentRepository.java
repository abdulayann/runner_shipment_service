package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;


@Repository
public interface IShipmentRepository extends MultiTenancyRepository<ShipmentDetails>, PermissionsRepository<ShipmentDetails> {
    Page<ShipmentDetails> findAll(Specification<ShipmentDetails> spec, Pageable pageable);
}
