package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.CommonErrorLogsConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.response.CommonErrorLogsResponse;
import com.dpw.runner.shipment.services.service.interfaces.ICommonErrorLogsService;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = CommonErrorLogsConstants.COMMON_ERROR_LOGS_API_HANDLE)
public class CommonErrorLogsController {
    private ICommonErrorLogsService commonErrorLogsService;

    private class MyResponseClass extends RunnerResponse<CommonErrorLogsResponse> {}
    @Autowired
    public CommonErrorLogsController(ICommonErrorLogsService commonErrorLogsService) {
        this.commonErrorLogsService = commonErrorLogsService;
    }

    @ApiResponses(value = {
            @ApiResponse(
                    responseCode = "200",
                    description = CommonErrorLogsConstants.LIST_SUCCESSFUL,
                    content = @Content(
                            mediaType = "application/json",
                            array = @ArraySchema(schema = @Schema(implementation = CommonErrorLogsController.MyResponseClass.class))
                    )
            )
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return commonErrorLogsService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(
            mediaType = "application/json",
            array = @ArraySchema(schema = @Schema(implementation = CommonErrorLogsController.MyResponseClass.class))), description = CommonErrorLogsConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@Parameter(description = CommonErrorLogsConstants.COMMON_ERROR_LOGS_ID) @RequestParam Optional<Long> id, @Parameter(description = CommonErrorLogsConstants.COMMON_ERROR_LOGS_GUID) @RequestParam Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return commonErrorLogsService.retrieveById(CommonRequestModel.buildRequest(request));
    }

}
