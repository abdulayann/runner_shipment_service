package com.dpw.runner.booking.services.service.interfaces;

import com.dpw.runner.booking.services.commons.requests.CommonRequestModel;
import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import org.springframework.http.ResponseEntity;

public interface ITasksService extends ICommonService{
    ResponseEntity<IRunnerResponse> createTaskForHbl(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> retrieveTask(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> updateTask(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> createTask(CommonRequestModel commonRequestModel);
}