package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ELDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IELDetailsDao {
    ELDetails save(ELDetails elDetails);
    Page<ELDetails> findAll(Specification<ELDetails> spec, Pageable pageable);
    Optional<ELDetails> findById(Long id);
    void delete(ELDetails elDetails);
    Optional<ELDetails> findByElNumber(String elNumber);
}
