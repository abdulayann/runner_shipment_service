package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.request.ContainerDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import javax.validation.Valid;

public interface ICargoService {
    CargoDetailsResponse getContainerDetails(@Valid ContainerDetailsRequest getContainerDetailsRequest) throws RunnerException;
}
