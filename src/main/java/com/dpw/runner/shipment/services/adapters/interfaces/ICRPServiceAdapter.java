package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface ICRPServiceAdapter {
    ResponseEntity<?> retrieveCRPService(CommonRequestModel requestModel) throws Exception;

    ResponseEntity<?> listCRPService(CommonRequestModel requestModel) throws Exception;

}
