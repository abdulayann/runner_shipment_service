package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class ShipmentDao implements IShipmentDao {
    @Autowired
    private IShipmentRepository shipmentRepository;

    @Override
    public ShipmentDetails save(ShipmentDetails shipmentDetails) {
        return shipmentRepository.save(shipmentDetails);
    }

    @Override
    public Page<ShipmentDetails> findAll(Specification<ShipmentDetails> spec, Pageable pageable) {
        return shipmentRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ShipmentDetails> findById(Long id) {
        return shipmentRepository.findById(id);
    }

    @Override
    public void delete(ShipmentDetails shipmentDetails) {
        shipmentRepository.delete(shipmentDetails);
    }
}
