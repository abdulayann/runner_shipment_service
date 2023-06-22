package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface IELDetailsService {
    ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> list(CommonRequestModel commonRequestModel);

    ResponseEntity<?> delete(CommonRequestModel commonRequestModel);

    ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel);

    ResponseEntity<?> validateElNumber(CommonRequestModel commonRequestModel);
}

