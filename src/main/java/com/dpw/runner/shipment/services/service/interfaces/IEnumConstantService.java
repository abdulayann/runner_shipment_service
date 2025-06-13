package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import org.springframework.http.ResponseEntity;

public interface IEnumConstantService {
    ResponseEntity<IRunnerResponse> list(Boolean isFromV3);
}
