package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface ITruckDriverDetailsRepository extends JpaRepository<TruckDriverDetails, Long> {
    Page<TruckDriverDetails> findAll(Specification<TruckDriverDetails> spec, Pageable pageable);
    Optional<TruckDriverDetails> findByGuid(UUID guid);

}
