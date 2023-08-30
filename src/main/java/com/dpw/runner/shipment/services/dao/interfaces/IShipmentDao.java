package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IShipmentDao {
    ShipmentDetails save(ShipmentDetails shipmentDetails);
    ShipmentDetails update(ShipmentDetails shipmentDetails);
    Page<ShipmentDetails> findAll(Specification<ShipmentDetails> spec, Pageable pageable);
    Optional<ShipmentDetails> findById(Long id);
    void delete(ShipmentDetails shipmentDetails);
    List<ShipmentDetails> saveAll(List<ShipmentDetails> shipments);
    Optional<ShipmentDetails> findByGuid(UUID id);
    Optional<ShipmentDetails> findByHouseBill(String Hbl);
    void updateDateAndStatus(long id, LocalDateTime date, Integer status);
}
