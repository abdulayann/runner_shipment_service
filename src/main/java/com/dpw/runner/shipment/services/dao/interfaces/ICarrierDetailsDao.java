package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.CarrierDetails;

import java.time.LocalDateTime;
import java.util.List;

public interface ICarrierDetailsDao {
    void saveUnLocCodes(CarrierDetails carrierDetails);

    List<CarrierDetails> findByIds(List<Long> id);

    void updateAta(Long id, LocalDateTime shipmentAta);

    void updateAtd(Long id, LocalDateTime shipmentAtd);
}
