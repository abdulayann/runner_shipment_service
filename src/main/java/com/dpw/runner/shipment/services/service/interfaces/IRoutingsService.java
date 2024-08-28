package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import java.util.List;

public interface IRoutingsService {
    List<Routings> updateEntityFromShipment(List<Routings> routingsList, Long shipmentId, List<Routings> oldEntityList) throws RunnerException;
    List<Routings> updateEntityFromShipment(List<Routings> routingsList, Long shipmentId) throws RunnerException;
}
