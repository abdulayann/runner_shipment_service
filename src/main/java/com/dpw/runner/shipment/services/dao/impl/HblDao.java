package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.commons.entity.Hbl;
import com.dpw.runner.shipment.services.repository.interfaces.IHblRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@Slf4j
public class HblDao implements IHblDao {
    @Autowired
    private IHblRepository hblRepository;


    @Override
    public Hbl save(Hbl hbl) {
        return hblRepository.save(hbl);
    }

    @Override
    public List<Hbl> findByShipmentId(Long shipmentId) {
        return hblRepository.findByShipmentId(shipmentId);
    }

    @Override
    public Optional<Hbl> findById(Long id) {
        return hblRepository.findById(id);
    }

    @Override
    public void delete(Hbl hbl) {
        hblRepository.delete(hbl);
    }
}
