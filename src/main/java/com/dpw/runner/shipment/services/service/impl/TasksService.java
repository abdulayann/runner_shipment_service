package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.v1.request.TaskCreateRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TaskUpdateRequest;
import com.dpw.runner.shipment.services.dto.v1.request.V1RetrieveRequest;
import com.dpw.runner.shipment.services.dto.v1.request.V1SaveRequest;
import com.dpw.runner.shipment.services.entity.enums.TaskStatus;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITasksService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.concurrent.CompletableFuture;

@Service
public class TasksService implements ITasksService {

    @Autowired
    private IV1Service iv1Service;


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
        return ResponseHelper.buildSuccessResponse(iv1Service.retrieveTask(V1RetrieveRequest.builder().EntityId(StringUtility.convertToString(request.getId())).build()));
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
