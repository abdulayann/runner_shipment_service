package com.dpw.runner.shipment.services.service.descarts;

import com.dpw.runner.shipment.services.dto.DenialParty.request.SearchEntityRequest;
import com.dpw.runner.shipment.services.dto.request.DenialPartySearchEntityRequest;
import org.springframework.http.ResponseEntity;

public interface IDescartsService {
    ResponseEntity<?> searchEntity(SearchEntityRequest commonRequestModel );
}
