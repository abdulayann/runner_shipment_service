package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.request.DenialPartySearchEntityRequest;
import com.dpw.runner.shipment.services.dto.response.DenialPartySearchResponse;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IDenialPartyScreeningService {
    ResponseEntity<?> createRequestAndSearchEntity(DenialPartySearchEntityRequest commonRequestModel);
}
