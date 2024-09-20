package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

public interface IShipmentServiceAdapter {

    ResponseEntity<IRunnerResponse> createShipmentInV2(CustomerBookingRequest customerBookingRequest) throws RunnerException;
}
