package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.AchievedQuantities;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IAchievedQuantitiesDao {
    AchievedQuantities save(AchievedQuantities achievedQuantities);
    Page<AchievedQuantities> findAll(Specification<AchievedQuantities> spec, Pageable pageable);
    Optional<AchievedQuantities> findById(Long id);
    void delete(AchievedQuantities achievedQuantities);
    AchievedQuantities updateEntityFromShipmentConsole(AchievedQuantities achievedQuantities) throws Exception;
}
