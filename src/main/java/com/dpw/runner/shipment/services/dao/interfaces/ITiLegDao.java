package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.TiLegs;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface ITiLegDao {
    TiLegs save(TiLegs tiLegs);
    List<TiLegs> saveAll(List<TiLegs> tiLegsList);
    Optional<TiLegs> findByGuid(UUID id);

    Page<TiLegs> findAll(Specification<TiLegs> spec, Pageable pageable);
    Optional<TiLegs> findById(Long id);
    void delete(TiLegs tiLegs);
    List<TiLegs> updateTiLegDetails(List<TiLegs> tiLegsList);
}
