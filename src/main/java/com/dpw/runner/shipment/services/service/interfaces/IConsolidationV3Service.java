package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentGridChangeResponse;
import com.dpw.runner.shipment.services.dto.request.ShipmentAttachDetachV3Request;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import java.util.List;

public interface IConsolidationV3Service {

    ShipmentGridChangeResponse calculateAchievedValues(Long consolidationId) throws RunnerException;

    String attachShipments(ShipmentAttachDetachV3Request shipmentAttachDetachRequest) throws RunnerException;

    void syncMainCarriageRoutingToShipment(List<Routings> consolidationRoutings, ShipmentDetails shipmentDetails, boolean saveRoutes, boolean reverseSyncFromConsolToShipment)
            throws RunnerException;

    void checkSciForAttachConsole(Long consoleId) throws RunnerException;
}