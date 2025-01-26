package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ITiTruckDriverDetailDao;
import com.dpw.runner.shipment.services.entity.TiTruckDriverDetails;
import com.dpw.runner.shipment.services.repository.interfaces.ITiTruckDriverDetailRepository;
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
public class TiTruckDriverDetailDao implements ITiTruckDriverDetailDao {
    @Autowired
    private ITiTruckDriverDetailRepository tiTruckDriverDetailRepository;
    @Override
    public List<TiTruckDriverDetails> saveAll(List<TiTruckDriverDetails> tiTruckDriverDetailsList) {
        return tiTruckDriverDetailRepository.saveAll(tiTruckDriverDetailsList);
    }

    @Override
    public TiTruckDriverDetails save(TiTruckDriverDetails tiTruckDriverDetails) {
        return tiTruckDriverDetailRepository.save(tiTruckDriverDetails);
    }

    @Override
    public Page<TiTruckDriverDetails> findAll(Specification<TiTruckDriverDetails> spec, Pageable pageable) {
        return tiTruckDriverDetailRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<TiTruckDriverDetails> findById(Long id) {
        return tiTruckDriverDetailRepository.findById(id);
    }

    @Override
    public void delete(TiTruckDriverDetails tiTruckDriverDetails) {
        tiTruckDriverDetailRepository.delete(tiTruckDriverDetails);
    }

    @Override
    public List<TiTruckDriverDetails> findByIds(List<Long> id) {
        return tiTruckDriverDetailRepository.findByIdIn(id);
    }
}
