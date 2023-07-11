package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IServiceDetailsDao;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.repository.interfaces.IServiceDetailsRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class ServiceDetailsDao implements IServiceDetailsDao {
    @Autowired
    private IServiceDetailsRepository serviceDetailsRepository;

    @Override
    public ServiceDetails save(ServiceDetails serviceDetails) {
        return serviceDetailsRepository.save(serviceDetails);
    }

    @Override
    public Page<ServiceDetails> findAll(Specification<ServiceDetails> spec, Pageable pageable) {
        return serviceDetailsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ServiceDetails> findById(Long id) {
        return serviceDetailsRepository.findById(id);
    }

    @Override
    public void delete(ServiceDetails serviceDetails) {
        serviceDetailsRepository.delete(serviceDetails);
    }
}
