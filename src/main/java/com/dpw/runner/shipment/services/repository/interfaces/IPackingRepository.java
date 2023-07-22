package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Packing;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface IPackingRepository extends MultiTenancyRepository<Packing> {

    List<Packing> findByShipmentId(Long shipmentId);

    @Query("SELECT c FROM Packing c")
    List<Packing> getAllPackings();
}
