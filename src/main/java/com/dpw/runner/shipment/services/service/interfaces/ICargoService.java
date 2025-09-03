package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.request.CargoChargeableRequest;
import com.dpw.runner.shipment.services.dto.request.CargoDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.CargoChargeableResponse;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import javax.validation.Valid;

public interface ICargoService {
    CargoDetailsResponse getCargoDetails(@Valid CargoDetailsRequest cargoDetailsRequest) throws RunnerException;
    CargoChargeableResponse calculateChargeable(CargoChargeableRequest request) throws RunnerException;
}
