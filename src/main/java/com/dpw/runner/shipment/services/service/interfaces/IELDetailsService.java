package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

public interface IELDetailsService extends ICommonService {
    ResponseEntity<IRunnerResponse> validateElNumber(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> V1ELDetailsCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws RunnerException;

}

