package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.IPartiesDao;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.repository.interfaces.IPartiesRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public class PartiesDao implements IPartiesDao {
    @Autowired
    private IPartiesRepository partiesRepository;

    @Override
    public List<Parties> saveAll(List<Parties> parties) {
        return partiesRepository.saveAll(parties);
    }
}
