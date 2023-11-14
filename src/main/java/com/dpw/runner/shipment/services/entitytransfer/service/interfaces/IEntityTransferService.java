package com.dpw.runner.shipment.services.entitytransfer.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface IEntityTransferService {
    ResponseEntity<?> sendShipment(CommonRequestModel commonRequestModel);
    ResponseEntity<?> sendConsolidation(CommonRequestModel commonRequestModel);
    ResponseEntity<?> importShipment(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<?> importConsolidation(CommonRequestModel commonRequestModel);
    ResponseEntity<?> sendConsolidationValidation(CommonRequestModel commonRequestModel);
    ResponseEntity<?> sendShipmentValidation(CommonRequestModel commonRequestModel);
}
