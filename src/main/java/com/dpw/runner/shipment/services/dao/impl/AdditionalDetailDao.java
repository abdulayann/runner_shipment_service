package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IAdditionalDetailDao;
import com.dpw.runner.shipment.services.entity.AdditionalDetail;
import com.dpw.runner.shipment.services.repository.interfaces.IAdditionalDetailRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class AdditionalDetailDao implements IAdditionalDetailDao {
    @Autowired
    private IAdditionalDetailRepository additionalDetailRepository;

    @Override
    public AdditionalDetail save(AdditionalDetail additionalDetail) {
        return additionalDetailRepository.save(additionalDetail);
    }

    @Override
    public Page<AdditionalDetail> findAll(Specification<AdditionalDetail> spec, Pageable pageable) {
        return additionalDetailRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<AdditionalDetail> findById(Long id) {
        return additionalDetailRepository.findById(id);
    }

    @Override
    public void delete(AdditionalDetail additionalDetail) {
        additionalDetailRepository.delete(additionalDetail);
    }
}
