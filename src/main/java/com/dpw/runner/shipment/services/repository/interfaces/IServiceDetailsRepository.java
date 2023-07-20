package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IServiceDetailsRepository extends JpaRepository<ServiceDetails, Long> {
    Page<ServiceDetails> findAll(Specification<ServiceDetails> spec, Pageable pageable);
    List<ServiceDetails> findByShipmentId(Long shipmentId);
    Optional<ServiceDetails> findByGuid(UUID guid);
}
