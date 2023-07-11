package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.repository.interfaces.ITruckDriverDetailsRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class TruckDriverDetailsDao implements ITruckDriverDetailsDao {
    @Autowired
    private ITruckDriverDetailsRepository truckDriverDetailsRepository;

    @Override
    public TruckDriverDetails save(TruckDriverDetails truckDriverDetails) {
        return truckDriverDetailsRepository.save(truckDriverDetails);
    }

    @Override
    public Page<TruckDriverDetails> findAll(Specification<TruckDriverDetails> spec, Pageable pageable) {
        return truckDriverDetailsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<TruckDriverDetails> findById(Long id) {
        return truckDriverDetailsRepository.findById(id);
    }

    @Override
    public void delete(TruckDriverDetails truckDriverDetails) {
        truckDriverDetailsRepository.delete(truckDriverDetails);
    }
}
