package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ITiPackageDao;
import com.dpw.runner.shipment.services.entity.TiPackages;
import com.dpw.runner.shipment.services.repository.interfaces.ITiPackageRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@Slf4j
public class TiPackageDao implements ITiPackageDao {

    private final ITiPackageRepository tiPackageRepository;

    @Autowired
    public TiPackageDao(ITiPackageRepository tiPackageRepository) {
        this.tiPackageRepository = tiPackageRepository;
    }

    @Override
    public List<TiPackages> saveAll(List<TiPackages> tiPackagesList) {
        return tiPackageRepository.saveAll(tiPackagesList);
    }

    @Override
    public TiPackages save(TiPackages tiPackages) {
        return tiPackageRepository.save(tiPackages);
    }

    @Override
    public Page<TiPackages> findAll(Specification<TiPackages> spec, Pageable pageable) {
        return tiPackageRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<TiPackages> findById(Long id) {
        return tiPackageRepository.findById(id);
    }

    @Override
    public void delete(TiPackages tiPackages) {
        tiPackageRepository.delete(tiPackages);
    }

    @Override
    public List<TiPackages> findByIds(List<Long> id) {
        return tiPackageRepository.findByIdIn(id);
    }
}
