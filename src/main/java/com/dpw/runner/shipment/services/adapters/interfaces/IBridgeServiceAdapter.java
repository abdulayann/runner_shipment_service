package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

public interface IBridgeServiceAdapter {
    IRunnerResponse requestTactResponse(CommonRequestModel commonRequestModel) throws RunnerException;
    IRunnerResponse requestOutBoundFileTransfer(CommonRequestModel commonRequestModel) throws RunnerException;
    IRunnerResponse bridgeApiIntegration(Object payload, String integrationCode, String transactionId, String referenceId) throws RunnerException;

}
