package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.repository.interfaces.IPackingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class PackingDao implements IPackingDao {
    @Autowired
    private IPackingRepository packingRepository;

    @Override
    public Packing save(Packing packing) {
        return packingRepository.save(packing);
    }

    @Override
    public Page<Packing> findAll(Specification<Packing> spec, Pageable pageable) {
        return packingRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Packing> findById(Long id) {
        return packingRepository.findById(id);
    }

    @Override
    public void delete(Packing packing) {
        packingRepository.delete(packing);
    }
}
