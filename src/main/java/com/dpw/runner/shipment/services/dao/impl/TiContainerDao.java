package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ITiContainerDao;
import com.dpw.runner.shipment.services.entity.TiContainers;
import com.dpw.runner.shipment.services.repository.interfaces.ITiContainerRepository;
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
public class TiContainerDao implements ITiContainerDao {

    private final ITiContainerRepository tiContainerRepository;

    @Autowired
    public TiContainerDao(ITiContainerRepository tiContainerRepository) {
        this.tiContainerRepository = tiContainerRepository;
    }

    @Override
    public List<TiContainers> saveAll(List<TiContainers> tiContainersList) {
        return tiContainerRepository.saveAll(tiContainersList);
    }

    @Override
    public TiContainers save(TiContainers tiContainers) {
        return tiContainerRepository.save(tiContainers);
    }

    @Override
    public Page<TiContainers> findAll(Specification<TiContainers> spec, Pageable pageable) {
        return tiContainerRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<TiContainers> findById(Long id) {
        return tiContainerRepository.findById(id);
    }

    @Override
    public void delete(TiContainers tiContainers) {
        tiContainerRepository.delete(tiContainers);
    }

    @Override
    public List<TiContainers> findByIds(List<Long> id) {
        return tiContainerRepository.findByIdIn(id);
    }
}
