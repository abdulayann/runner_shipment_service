package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

public interface IShipmentServiceAdapter {

    ShipmentDetailsResponse createShipment(ShipmentDetailsResponse shipmentRequest) throws RunnerException;
    ResponseEntity<IRunnerResponse> getShipmentIdbyGuid(String guid) throws RunnerException;
    ConsolidationDetailsResponse createConsolidation(ConsolidationDetailsRequest consolidationDetailsRequest) throws RunnerException;
}
