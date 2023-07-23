package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.ConsolShipLinkDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IConsolShipLinkRepository extends JpaRepository<ConsolShipLinkDetails, Long> {
     List<ConsolShipLinkDetails> findByConsolidationId(Long containerId);
     List<ConsolShipLinkDetails> findByShipmentId(Long shipmentId);
}
