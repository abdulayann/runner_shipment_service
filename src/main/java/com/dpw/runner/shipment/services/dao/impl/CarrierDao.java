package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDao;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.repository.interfaces.ICarrierRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class CarrierDao implements ICarrierDao {
    @Autowired
    private ICarrierRepository carrierRepository;

    @Override
    public CarrierDetails save(CarrierDetails carrierDetails) {
        return carrierRepository.save(carrierDetails);
    }

    @Override
    public Page<CarrierDetails> findAll(Specification<CarrierDetails> spec, Pageable pageable) {
        return carrierRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<CarrierDetails> findById(Long id) {
        return carrierRepository.findById(id);
    }

    @Override
    public void delete(CarrierDetails carrierDetails) {
        carrierRepository.delete(carrierDetails);
    }
}
