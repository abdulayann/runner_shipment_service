package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IELDetailsDao;
import com.dpw.runner.shipment.services.entity.ELDetails;
import com.dpw.runner.shipment.services.repository.interfaces.IELDetailsRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class ELDetailsDao implements IELDetailsDao {
    @Autowired
    private IELDetailsRepository elDetailsRepository;

    @Override
    public ELDetails save(ELDetails elDetails) {
        return elDetailsRepository.save(elDetails);
    }

    @Override
    public Page<ELDetails> findAll(Specification<ELDetails> spec, Pageable pageable) {
        return elDetailsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ELDetails> findById(Long id) {
        return elDetailsRepository.findById(id);
    }

    @Override
    public void delete(ELDetails elDetails) {
        elDetailsRepository.delete(elDetails);
    }

    @Override
    public Optional<ELDetails> findByElNumber(String elNumber) {
        return elDetailsRepository.findByElNumber(elNumber);
    }
}
