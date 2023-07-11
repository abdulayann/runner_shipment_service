package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.repository.interfaces.IPartiesRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public class PartiesDao implements IPartiesDao {
    @Autowired
    private IPartiesRepository partiesRepository;

    @Override
    public List<Parties> saveAll(List<Parties> parties) {
        return partiesRepository.saveAll(parties);
    }

    @Override
    public Parties save(Parties parties) {
        return partiesRepository.save(parties);
    }

    @Override
    public Page<Parties> findAll(Specification<Parties> spec, Pageable pageable) {
        return partiesRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Parties> findById(Long id) {
        return partiesRepository.findById(id);
    }

    @Override
    public void delete(Parties parties) {
        partiesRepository.delete(parties);
    }
}
