package com.dpw.runner.shipment.services.dao;

import com.dpw.runner.shipment.services.entity.CarrierDetails;

public interface ICarrierDao {
    CarrierDetails save(CarrierDetails carrierDetails);
}
