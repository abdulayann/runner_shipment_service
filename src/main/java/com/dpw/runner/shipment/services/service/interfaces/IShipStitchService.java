package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import org.springframework.http.ResponseEntity;

public interface IShipStitchService {
    ResponseEntity<IRunnerResponse> updateEntityFromShipment(CommonRequestModel commonRequestModel, Long id);
}