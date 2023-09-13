package com.dpw.runner.shipment.services.service.interfaces;


import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface IHblService extends ICommonService {

    ResponseEntity<?> generateHBL(CommonRequestModel commonRequestModel);
    ResponseEntity<?> retrieveByShipmentId(CommonRequestModel buildRequest);
    ResponseEntity<?> resetHbl(CommonRequestModel buildRequest);
    ResponseEntity<?> saveV1Hbl(CommonRequestModel commonRequestModel) throws Exception;
}
