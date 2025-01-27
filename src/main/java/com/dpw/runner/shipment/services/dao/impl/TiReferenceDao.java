package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ITiReferenceDao;
import com.dpw.runner.shipment.services.entity.TiReferences;
import com.dpw.runner.shipment.services.repository.interfaces.ITiReferenceRepository;
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
public class TiReferenceDao implements ITiReferenceDao {
    @Autowired
    private ITiReferenceRepository tiReferenceRepository;
    @Override
    public List<TiReferences> saveAll(List<TiReferences> tiReferencesList) {
        return tiReferenceRepository.saveAll(tiReferencesList);
    }

    @Override
    public TiReferences save(TiReferences tiReferences) {
        return tiReferenceRepository.save(tiReferences);
    }

    @Override
    public Page<TiReferences> findAll(Specification<TiReferences> spec, Pageable pageable) {
        return tiReferenceRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<TiReferences> findById(Long id) {
        return tiReferenceRepository.findById(id);
    }

    @Override
    public void delete(TiReferences tiReferences) {
        tiReferenceRepository.delete(tiReferences);
    }

    @Override
    public List<TiReferences> findByIds(List<Long> id) {
        return tiReferenceRepository.findByIdIn(id);
    }
}
