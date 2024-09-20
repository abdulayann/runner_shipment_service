package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

public interface IMDMServiceAdapter {
    ResponseEntity<IRunnerResponse> getCreditInfo(CommonRequestModel commonRequestModel) throws RunnerException;

    String getApprovalStausForParties(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> createShipmentTaskFromBooking(CommonRequestModel commonRequestModel) throws RunnerException;
}
