package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;
import java.util.UUID;

public interface IShipmentDao {
    ShipmentDetails save(ShipmentDetails shipmentDetails);
    Page<ShipmentDetails> findAll(Specification<ShipmentDetails> spec, Pageable pageable);
    Optional<ShipmentDetails> findById(Long id);
    void delete(ShipmentDetails shipmentDetails);
    Optional<ShipmentDetails> findByGuid(UUID guid);
}
