package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IReferenceNumbersRepository extends JpaRepository<ReferenceNumbers, Long> {
    Page<ReferenceNumbers> findAll(Specification<ReferenceNumbers> spec, Pageable pageable);
    List<ReferenceNumbers> findByShipmentId(Long shipmentId);
    Optional<ReferenceNumbers> findByGuid(UUID guid);
}
