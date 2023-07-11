package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Jobs;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IJobDao {
    Jobs save(Jobs jobs);
    Page<Jobs> findAll(Specification<Jobs> spec, Pageable pageable);
    Optional<Jobs> findById(Long id);
    void delete(Jobs jobs);
}
