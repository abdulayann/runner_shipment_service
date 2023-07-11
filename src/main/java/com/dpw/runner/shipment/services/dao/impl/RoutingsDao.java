package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.repository.interfaces.IRoutingsRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class RoutingsDao implements IRoutingsDao {
    @Autowired
    private IRoutingsRepository routingsRepository;

    @Override
    public Routings save(Routings routings) {
        return routingsRepository.save(routings);
    }

    @Override
    public Page<Routings> findAll(Specification<Routings> spec, Pageable pageable) {
        return routingsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Routings> findById(Long id) {
        return routingsRepository.findById(id);
    }

    @Override
    public void delete(Routings routings) {
        routingsRepository.delete(routings);
    }
}
