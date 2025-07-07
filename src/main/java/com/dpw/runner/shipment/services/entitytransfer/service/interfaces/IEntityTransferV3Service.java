package com.dpw.runner.shipment.services.entitytransfer.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.fasterxml.jackson.databind.JsonMappingException;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IEntityTransferV3Service {
    List<Integer> sendShipment(CommonRequestModel commonRequestModel);
    String importShipment(CommonRequestModel commonRequestModel) throws RunnerException, JsonMappingException;
    List<Integer> sendConsolidation(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> importConsolidation(CommonRequestModel commonRequestModel) throws RunnerException, JsonMappingException;
    List<String> getTenantName(List<Integer> tenantIds);
}
