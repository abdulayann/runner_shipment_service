package com.dpw.runner.booking.services.service.impl;

import com.dpw.runner.booking.services.commons.requests.CommonGetRequest;
import com.dpw.runner.booking.services.commons.requests.CommonRequestModel;
import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import com.dpw.runner.booking.services.dto.v1.request.TaskCreateRequest;
import com.dpw.runner.booking.services.dto.v1.request.TaskUpdateRequest;
import com.dpw.runner.booking.services.dto.v1.request.V1RetrieveRequest;
import com.dpw.runner.booking.services.dto.v1.request.V1SaveRequest;
import com.dpw.runner.booking.services.dto.v1.response.TaskResponse;
import com.dpw.runner.booking.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.booking.services.entity.enums.TaskStatus;
import com.dpw.runner.booking.services.helpers.JsonHelper;
import com.dpw.runner.booking.services.helpers.ResponseHelper;
import com.dpw.runner.booking.services.service.interfaces.ITasksService;
import com.dpw.runner.booking.services.service.v1.IV1Service;
import com.dpw.runner.booking.services.utils.StringUtility;
import lombok.NoArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.concurrent.CompletableFuture;

@Service
@NoArgsConstructor
public class TasksService implements ITasksService {

    private IV1Service iv1Service;
    private JsonHelper jsonHelper;

    @Autowired
    public TasksService(IV1Service iv1Service, JsonHelper jsonHelper) {
        this.jsonHelper = jsonHelper;
        this.iv1Service = iv1Service;
    }


    @Override
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> createTaskForHbl(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildSuccessResponse(iv1Service.createTaskforHBL(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveTask(CommonRequestModel commonRequestModel) {
        var request = (CommonGetRequest) commonRequestModel.getData();
        V1RetrieveResponse v1RetrieveResponse = iv1Service.retrieveTask(V1RetrieveRequest.builder().EntityId(StringUtility.convertToString(request.getId())).build());
        TaskResponse taskResponse = jsonHelper.convertValue(v1RetrieveResponse.getEntity(), TaskResponse.class);
        return ResponseHelper.buildSuccessResponse(taskResponse);
    }

    @Override
    public ResponseEntity<IRunnerResponse> updateTask(CommonRequestModel commonRequestModel) {
        var request = (TaskUpdateRequest) commonRequestModel.getData();
        return ResponseHelper.buildSuccessResponse(iv1Service.updateTask(V1SaveRequest.builder().entity(request).entityId(request.getId()).build()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> createTask(CommonRequestModel commonRequestModel) {
        var request = (TaskCreateRequest) commonRequestModel.getData();
        request.setTaskStatus(TaskStatus.PENDING_ACTION.getDescription());
        return ResponseHelper.buildSuccessResponse(iv1Service.createTask(request));
    }
}
