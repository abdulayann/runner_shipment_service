package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.CarrierDetails;

public interface ICarrierDetailsDao {
    void saveUnLocCodes(CarrierDetails carrierDetails);
}
