package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import org.springframework.http.ResponseEntity;

public interface IIntegrationResponseService extends ICommonService {
    ResponseEntity<IRunnerResponse> fetchIntegrationResponses(CommonRequestModel commonRequestModel);
}