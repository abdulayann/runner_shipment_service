package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDetailsDao;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.repository.interfaces.ICarrierRepository;
import java.time.LocalDateTime;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class CarrierDetailsDao implements ICarrierDetailsDao {

    @Autowired
    private ICarrierRepository carrierRepository;

    @Override
    public void saveUnLocCodes(CarrierDetails carrierDetails) {
        carrierRepository.saveUnLocCodes(carrierDetails.getId(), carrierDetails.getOriginLocCode(), carrierDetails.getOriginPortLocCode(),
                carrierDetails.getDestinationLocCode(), carrierDetails.getDestinationPortLocCode());
    }

    @Override
    public List<CarrierDetails> findByIds(List<Long> id) {
        return carrierRepository.findByIdIn(id);
    }

    @Override
    public void updateAta(Long id, LocalDateTime shipmentAta) {
        carrierRepository.updateAta(id, shipmentAta);
    }

    @Override
    public void updateAtd(Long id, LocalDateTime shipmentAtd) {
        carrierRepository.updateAtd(id, shipmentAtd);
    }

    @Override
    public void update(CarrierDetails carrierDetails){
        carrierRepository.save(carrierDetails);
    }
}
