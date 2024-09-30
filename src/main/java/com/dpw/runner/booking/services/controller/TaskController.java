package com.dpw.runner.booking.services.controller;

import com.dpw.runner.booking.services.commons.constants.ApiConstants;
import com.dpw.runner.booking.services.commons.constants.Constants;
import com.dpw.runner.booking.services.commons.constants.DaoConstants;
import com.dpw.runner.booking.services.commons.constants.TaskConstants;
import com.dpw.runner.booking.services.commons.constants.*;
import com.dpw.runner.booking.services.commons.requests.CommonGetRequest;
import com.dpw.runner.booking.services.commons.requests.CommonRequestModel;
import com.dpw.runner.booking.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import com.dpw.runner.booking.services.commons.responses.RunnerResponse;
import com.dpw.runner.booking.services.helpers.ResponseHelper;
import com.dpw.runner.booking.services.service.interfaces.ITasksService;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@RestController
@RequestMapping(TaskConstants.TASK_API_HANDLE)
@Slf4j
public class TaskController {

    private final ITasksService tasksService;
    private static class MyResponseClass extends RunnerResponse<DependentServiceResponse> {}
    @Autowired
    public TaskController(ITasksService tasksService) {
        this.tasksService = tasksService;
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = TaskConstants.TASK_CREATION_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(TaskConstants.TASK_CREATION)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return tasksService.createTaskForHbl(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = TaskConstants.TASK_CREATION_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieve(@ApiParam(value = TaskConstants.ID, required = true) @RequestParam Long id) {
        String responseMsg;
        try {
            return tasksService.retrieveTask(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(id).build()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
