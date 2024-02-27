package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

public interface IRoutingsService extends ICommonService {
    ResponseEntity<IRunnerResponse> V1RoutingsCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws RunnerException;

}
