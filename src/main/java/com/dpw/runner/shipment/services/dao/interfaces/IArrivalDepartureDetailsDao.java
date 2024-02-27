package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ArrivalDepartureDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IArrivalDepartureDetailsDao {
    ArrivalDepartureDetails save(ArrivalDepartureDetails arrivalDepartureDetails);
    Page<ArrivalDepartureDetails> findAll(Specification<ArrivalDepartureDetails> spec, Pageable pageable);
    Optional<ArrivalDepartureDetails> findById(Long id);
    void delete(ArrivalDepartureDetails arrivalDepartureDetails);
    ArrivalDepartureDetails updateEntityFromShipmentConsole(ArrivalDepartureDetails arrivalDepartureDetails) throws RunnerException;
}
