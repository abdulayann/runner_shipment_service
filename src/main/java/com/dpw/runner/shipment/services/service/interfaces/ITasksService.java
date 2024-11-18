package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import java.util.concurrent.CompletableFuture;
import org.springframework.http.ResponseEntity;

public interface ITasksService extends ICommonService{
    ResponseEntity<IRunnerResponse> createTaskForHbl(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> retrieveTask(CommonRequestModel commonRequestModel);
    CompletableFuture<ResponseEntity<IRunnerResponse>> updateTask(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> createTask(CommonRequestModel commonRequestModel);
}