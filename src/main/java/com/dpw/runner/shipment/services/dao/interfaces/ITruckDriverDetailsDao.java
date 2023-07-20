package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;
import java.util.UUID;

public interface ITruckDriverDetailsDao {
    TruckDriverDetails save(TruckDriverDetails truckDriverDetails);
    Page<TruckDriverDetails> findAll(Specification<TruckDriverDetails> spec, Pageable pageable);
    Optional<TruckDriverDetails> findById(Long id);
    void delete(TruckDriverDetails truckDriverDetails);
    Optional<TruckDriverDetails> findByGuid(UUID guid);
}
