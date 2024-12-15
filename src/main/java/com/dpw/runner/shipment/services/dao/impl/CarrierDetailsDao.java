package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDetailsDao;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.repository.interfaces.ICarrierRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public class CarrierDetailsDao implements ICarrierDetailsDao {

    @Autowired
    private ICarrierRepository carrierRepository;

    @Override
    public CarrierDetails save(CarrierDetails carrierDetails) {
        return carrierRepository.save(carrierDetails);
    }

    @Override
    public void saveUnLocCodes(CarrierDetails carrierDetails) {
        carrierRepository.saveUnLocCodes(carrierDetails.getId(), carrierDetails.getOriginLocCode(), carrierDetails.getOriginPortLocCode(),
                carrierDetails.getDestinationLocCode(), carrierDetails.getDestinationPortLocCode());
    }

    @Override
    public List<CarrierDetails> findByIds(List<Long> id) {
        return carrierRepository.findByIdIn(id);
    }
}
