package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.DenialPartySearchEntityRequest;
import org.springframework.http.ResponseEntity;

public interface IDenialPartyScreeningService {
    ResponseEntity<IRunnerResponse> createRequestAndSearchEntity(DenialPartySearchEntityRequest commonRequestModel);
}
