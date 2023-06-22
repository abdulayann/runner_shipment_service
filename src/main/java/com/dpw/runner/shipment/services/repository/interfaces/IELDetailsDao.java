package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.ELDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.Optional;

public interface IELDetailsDao extends JpaRepository<ELDetails, Long> {
    Page<ELDetails> findAll(Specification<ELDetails> spec, Pageable pageable);

    @Query("SELECT e FROM ELDetails e WHERE e.elNumber = :elNumber")
    Optional<ELDetails> findByElNumber(String elNumber);
}
