package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@Generated
public interface ICarrierBookingRepository extends MultiTenancyRepository<CarrierBooking> {

    List<CarrierBooking> findAll();

    Page<CarrierBooking> findAll(Specification<CarrierBooking> spec, Pageable pageable);

}
