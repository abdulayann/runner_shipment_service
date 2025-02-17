package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Hbl;

import java.util.List;
import java.util.Optional;

public interface IHblDao {
    Hbl save(Hbl hbl);

    List<Hbl> findByShipmentId(Long shipmentId);

    Optional<Hbl> findById(Long Id);

    void delete(Hbl Hbl);
}
