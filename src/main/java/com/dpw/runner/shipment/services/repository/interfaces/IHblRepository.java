package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Hbl;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface IHblRepository extends MultiTenancyRepository<Hbl> {
    List<Hbl> findByShipmentId(Long shipmentId);
    Optional<Hbl> findById(Long id);
}
