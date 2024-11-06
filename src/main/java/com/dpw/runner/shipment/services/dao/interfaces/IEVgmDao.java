package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.EVgm;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;
import java.util.UUID;

public interface IEVgmDao {
    EVgm save(EVgm eVgm);

    Page<EVgm> findAll(Specification<EVgm> spec, Pageable pageable);

    Optional<EVgm> findById(Long id);

    Optional<EVgm> findByGuid(UUID id);

    void delete(EVgm eVgm);
}
