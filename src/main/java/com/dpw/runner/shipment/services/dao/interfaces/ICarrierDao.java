package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.CarrierDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface ICarrierDao {
    CarrierDetails save(CarrierDetails carrierDetails);
    Page<CarrierDetails> findAll(Specification<CarrierDetails> spec, Pageable pageable);
    Optional<CarrierDetails> findById(Long id);
    void delete(CarrierDetails carrierDetails);
    CarrierDetails updateEntityFromShipmentConsole(CarrierDetails carrierDetails) throws RunnerException;
}
