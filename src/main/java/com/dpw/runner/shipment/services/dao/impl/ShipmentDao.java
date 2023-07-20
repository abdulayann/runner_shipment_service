package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.Set;
import java.util.UUID;

@Repository
public class ShipmentDao implements IShipmentDao {
    @Autowired
    private IShipmentRepository shipmentRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public ShipmentDetails save(ShipmentDetails shipmentDetails) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(shipmentDetails) , Constants.SHIPMENT, LifecycleHooks.ON_CREATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(errors.toString());
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
    public Optional<ShipmentDetails> findByGuid(UUID guid) {
        return shipmentRepository.findByGuid(guid);
    }

    @Override
    public void delete(ShipmentDetails shipmentDetails) {
        shipmentRepository.delete(shipmentDetails);
    }
}
