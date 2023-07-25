package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Containers;
import org.springframework.data.jpa.repository.Query;

import java.util.List;


public interface IContainerRepository extends MultiTenancyRepository<Containers> {
    @Query("SELECT c FROM Containers c")
    List<Containers> getAllContainers();
}
