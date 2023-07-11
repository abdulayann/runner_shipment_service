package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IJobDao;
import com.dpw.runner.shipment.services.entity.Jobs;
import com.dpw.runner.shipment.services.repository.interfaces.IJobRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class JobDao implements IJobDao {
    @Autowired
    private IJobRepository jobRepository;

    @Override
    public Jobs save(Jobs jobs) {
        return jobRepository.save(jobs);
    }

    @Override
    public Page<Jobs> findAll(Specification<Jobs> spec, Pageable pageable) {
        return jobRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Jobs> findById(Long id) {
        return jobRepository.findById(id);
    }

    @Override
    public void delete(Jobs jobs) {
        jobRepository.delete(jobs);
    }
}
