package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.npm.UpdateContractRequest;
import org.springframework.http.ResponseEntity;

public interface INPMServiceAdapter {
    ResponseEntity<?> fetchContracts(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> updateContracts(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> fetchOffers(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> fetchOffersV8(CommonRequestModel commonRequestModel) throws Exception;
}
