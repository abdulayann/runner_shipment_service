package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IReferenceNumbersDao {
    ReferenceNumbers save(ReferenceNumbers referenceNumbers);
    Page<ReferenceNumbers> findAll(Specification<ReferenceNumbers> spec, Pageable pageable);
    Optional<ReferenceNumbers> findById(Long id);
    void delete(ReferenceNumbers referenceNumbers);
    List<ReferenceNumbers> updateEntityFromShipment(List<ReferenceNumbers> referenceNumbersList, Long shipmentId) throws Exception;
    Optional<ReferenceNumbers> findByGuid(UUID guid);
}
