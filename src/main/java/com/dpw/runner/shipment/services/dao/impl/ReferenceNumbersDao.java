package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IReferenceNumbersDao;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.repository.interfaces.IReferenceNumbersRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class ReferenceNumbersDao implements IReferenceNumbersDao {
    @Autowired
    private IReferenceNumbersRepository referenceNumbersRepository;

    @Override
    public ReferenceNumbers save(ReferenceNumbers referenceNumbers) {
        return referenceNumbersRepository.save(referenceNumbers);
    }

    @Override
    public Page<ReferenceNumbers> findAll(Specification<ReferenceNumbers> spec, Pageable pageable) {
        return referenceNumbersRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ReferenceNumbers> findById(Long id) {
        return referenceNumbersRepository.findById(id);
    }

    @Override
    public void delete(ReferenceNumbers referenceNumbers) {
        referenceNumbersRepository.delete(referenceNumbers);
    }
}
