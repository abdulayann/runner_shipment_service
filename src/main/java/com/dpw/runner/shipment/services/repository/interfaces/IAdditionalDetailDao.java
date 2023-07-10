package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.AdditionalDetail;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface IAdditionalDetailDao extends JpaRepository<AdditionalDetail, Long> {
    Page<AdditionalDetail> findAll(Specification<AdditionalDetail> spec, Pageable pageable);
}