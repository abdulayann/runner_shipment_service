package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IConsolidationDetailsDao {
    ConsolidationDetails save(ConsolidationDetails consolidationDetails);
    ConsolidationDetails update(ConsolidationDetails consolidationDetails);
    Page<ConsolidationDetails> findAll(Specification<ConsolidationDetails> spec, Pageable pageable);
    Optional<ConsolidationDetails> findById(Long id);
    void delete(ConsolidationDetails consolidationDetails);
}
