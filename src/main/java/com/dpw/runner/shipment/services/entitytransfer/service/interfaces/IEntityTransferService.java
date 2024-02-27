package com.dpw.runner.shipment.services.entitytransfer.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

public interface IEntityTransferService {
    ResponseEntity<?> sendShipment(CommonRequestModel commonRequestModel);
    ResponseEntity<?> sendConsolidation(CommonRequestModel commonRequestModel);
    ResponseEntity<?> importShipment(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<?> importConsolidation(CommonRequestModel commonRequestModel);
    ResponseEntity<?> sendConsolidationValidation(CommonRequestModel commonRequestModel);
    ResponseEntity<?> sendShipmentValidation(CommonRequestModel commonRequestModel);
    ResponseEntity<?> checkTaskExist(CommonRequestModel commonRequestModel);
}
