package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.CarrierDetails;

import java.util.List;

public interface ICarrierDetailsDao {
    CarrierDetails save(CarrierDetails carrierDetails);
    void saveUnLocCodes(CarrierDetails carrierDetails);
    List<CarrierDetails> findByIds(List<Long> id);
}
