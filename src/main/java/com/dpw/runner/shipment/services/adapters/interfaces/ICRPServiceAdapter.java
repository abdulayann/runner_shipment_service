package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.response.RunnerResponse;
import org.springframework.http.ResponseEntity;

public interface ICRPServiceAdapter {
    ResponseEntity<IRunnerResponse> retrieveCRPService(CommonRequestModel requestModel) throws Exception;

    ResponseEntity<IRunnerResponse> listCRPService(CommonRequestModel requestModel) throws Exception;

}
