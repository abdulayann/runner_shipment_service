package com.dpw.runner.shipment.services.entitytransfer.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendConsoleValidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendShipmentValidationResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

public interface IEntityTransferService {
    ResponseEntity<IRunnerResponse> sendShipment(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> sendConsolidation(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> importShipment(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> importConsolidation(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> sendConsolidationValidation(CommonRequestModel commonRequestModel);

    SendConsoleValidationResponse automaticTransferConsoleValidation(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> sendShipmentValidation(CommonRequestModel commonRequestModel);

    SendShipmentValidationResponse automaticTransferShipmentValidation(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> checkTaskExist(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> postArValidation(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> checkEntityExists(CommonRequestModel commonRequestModel);
}
