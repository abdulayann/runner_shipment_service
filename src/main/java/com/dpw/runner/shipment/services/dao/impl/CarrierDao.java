package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.repository.interfaces.ICarrierRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class CarrierDao implements com.dpw.runner.shipment.services.dao.ICarrierDao {
    @Autowired
    private ICarrierRepository carrierRepository;

    @Override
    public CarrierDetails save(CarrierDetails carrierDetails) {
        return carrierRepository.save(carrierDetails);
    }
}
