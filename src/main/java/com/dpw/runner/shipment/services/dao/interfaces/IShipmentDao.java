package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IShipmentDao {
    ShipmentDetails save(ShipmentDetails shipmentDetails);
    ShipmentDetails update(ShipmentDetails shipmentDetails);
    Page<ShipmentDetails> findAll(Specification<ShipmentDetails> spec, Pageable pageable);
    Optional<ShipmentDetails> findById(Long id);
    void delete(ShipmentDetails shipmentDetails);
    List<ShipmentDetails> saveAll(List<ShipmentDetails> shipments);
}
