package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ITiLegDao;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.repository.interfaces.ITiLegRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
@Slf4j
public class TiLegDao implements ITiLegDao {

    private final ITiLegRepository tiLegRepository;

    @Autowired
    public TiLegDao(ITiLegRepository tiLegRepository) {
        this.tiLegRepository = tiLegRepository;
    }

    @Override
    public TiLegs save(TiLegs tiLegs) {
        return tiLegRepository.save(tiLegs);
    }

    @Override
    public List<TiLegs> saveAll(List<TiLegs> tiLegsList) {
        return tiLegRepository.saveAll(tiLegsList);
    }

    @Override
    public Optional<TiLegs> findByGuid(UUID id) {
        return tiLegRepository.findByGuid(id);
    }

    @Override
    public Page<TiLegs> findAll(Specification<TiLegs> spec, Pageable pageable) {
        return tiLegRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<TiLegs> findById(Long id) {
        return tiLegRepository.findById(id);
    }

    @Override
    public void delete(TiLegs tiLegs) {
        tiLegRepository.delete(tiLegs);
    }

    @Override
    public List<TiLegs> updateTiLegDetails(List<TiLegs> tiLegsList) {
        return tiLegRepository.saveAll(tiLegsList);
    }
}
