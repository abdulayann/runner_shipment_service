package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITasksService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.concurrent.CompletableFuture;

@Service
public class TasksService implements ITasksService {

    @Autowired
    private IV1Service iv1Service;


    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> createTask(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildSuccessResponse(iv1Service.createTaskforHBL(commonRequestModel.getDependentData()));
    }
}
