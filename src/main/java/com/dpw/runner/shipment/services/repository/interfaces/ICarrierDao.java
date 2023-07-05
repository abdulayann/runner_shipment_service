package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ICarrierDao extends MultiTenancyRepository<CarrierDetails> {

    List<CarrierDetails> findByShipmentId(Long shipmentId);

}
