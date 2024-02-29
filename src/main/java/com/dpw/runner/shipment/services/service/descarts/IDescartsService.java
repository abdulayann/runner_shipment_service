package com.dpw.runner.shipment.services.service.descarts;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.DenialParty.request.SearchEntityRequest;
import org.springframework.http.ResponseEntity;

public interface IDescartsService {
    ResponseEntity<IRunnerResponse> searchEntity(SearchEntityRequest commonRequestModel );
}
