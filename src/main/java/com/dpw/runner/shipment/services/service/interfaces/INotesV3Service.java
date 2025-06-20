package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import java.util.concurrent.CompletableFuture;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.transaction.annotation.Transactional;

public interface INotesV3Service {

    @Transactional
    ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel);

    @Transactional
    ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel);

    @Async
    CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel);
}
