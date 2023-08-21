package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface IPlatformServiceAdapter {
    ResponseEntity<?> createAtPlatform(CommonRequestModel requestModel) throws Exception;

    ResponseEntity<?> updateAtPlaform(CommonRequestModel requestModel) throws Exception;

}
