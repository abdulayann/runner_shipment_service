package com.dpw.runner.booking.services.adapters.interfaces;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import com.dpw.runner.booking.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.booking.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

public interface IShipmentServiceAdapter {

    ShipmentDetailsResponse createShipment(ShipmentDetailsResponse shipmentRequest) throws RunnerException;
    ResponseEntity<IRunnerResponse> getShipmentIdbyGuid(String guid) throws RunnerException;
}
