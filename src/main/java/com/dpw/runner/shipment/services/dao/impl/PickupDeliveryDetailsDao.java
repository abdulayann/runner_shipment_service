package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.repository.interfaces.IPickupDeliveryDetailsRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class PickupDeliveryDetailsDao implements IPickupDeliveryDetailsDao {
    @Autowired
    private IPickupDeliveryDetailsRepository pickupDeliveryDetailsRepository;

    @Override
    public PickupDeliveryDetails save(PickupDeliveryDetails pickupDeliveryDetails) {
        return pickupDeliveryDetailsRepository.save(pickupDeliveryDetails);
    }

    @Override
    public Page<PickupDeliveryDetails> findAll(Specification<PickupDeliveryDetails> spec, Pageable pageable) {
        return pickupDeliveryDetailsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<PickupDeliveryDetails> findById(Long id) {
        return pickupDeliveryDetailsRepository.findById(id);
    }

    @Override
    public void delete(PickupDeliveryDetails pickupDeliveryDetails) {
        pickupDeliveryDetailsRepository.delete(pickupDeliveryDetails);
    }
}
