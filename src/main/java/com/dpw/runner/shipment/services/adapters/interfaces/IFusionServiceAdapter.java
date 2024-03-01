package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.response.RunnerResponse;
import org.springframework.http.ResponseEntity;

public interface IFusionServiceAdapter {
    ResponseEntity<IRunnerResponse> checkCreditLimitP100(CommonRequestModel requestModel) throws RunnerException;
}
