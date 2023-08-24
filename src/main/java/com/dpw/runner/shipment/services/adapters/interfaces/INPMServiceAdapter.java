package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.npm.NPMFetchOffersRequest;
import org.springframework.http.ResponseEntity;

public interface INPMServiceAdapter {
    ResponseEntity<?> fetchContracts(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> fetchOffers(CommonRequestModel commonRequestModel) throws Exception;
}
