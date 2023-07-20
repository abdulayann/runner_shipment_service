package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface IAdditionalDetailRepository extends JpaRepository<AdditionalDetails, Long> {
    Page<AdditionalDetails> findAll(Specification<AdditionalDetails> spec, Pageable pageable);
    Optional<AdditionalDetails> findByGuid(UUID guid);
}