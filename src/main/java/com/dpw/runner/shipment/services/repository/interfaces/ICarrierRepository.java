package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface ICarrierRepository extends MultiTenancyRepository<CarrierDetails> {
    Optional<CarrierDetails> findByGuid(UUID guid);
}
