package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

import java.util.concurrent.CompletableFuture;

@SuppressWarnings("ALL")
public interface ICommonService {
    ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel);

    CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel);
}
