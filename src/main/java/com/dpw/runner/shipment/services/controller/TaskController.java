package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.TaskConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITasksService;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

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
            @ApiResponse(responseCode = "200", description = TaskConstants.TASK_CREATION_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = DependentServiceResponse.class)))
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
            @ApiResponse(responseCode = "200", description = TaskConstants.TASK_CREATION_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = DependentServiceResponse.class)))
    })
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieve(@Parameter(description = TaskConstants.ID, required = true) @RequestParam Long id) {
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

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = TaskConstants.TASK_CREATION_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = DependentServiceResponse.class)))
    })
    @GetMapping(ApiConstants.API_RETRIEVE_BY_UUID)
    public ResponseEntity<IRunnerResponse> retrieveMDMTask( @Parameter(description = TaskConstants.UUID, required = false)
                                                                @RequestParam(name = "uuid", required = false) String uuid,

                                                            @Parameter(description = TaskConstants.ID_SMALL, required = false)
                                                                @RequestParam(name = "id", required = false) Long id) {
        String responseMsg;
        try {
            return tasksService.retrieveMDMTask(uuid, id);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
