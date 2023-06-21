package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface IShipmentSettingsService {
    ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel);

    ResponseEntity<?> list(CommonRequestModel commonRequestModel);
}
