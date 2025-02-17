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
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.Optional;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = CommonErrorLogsConstants.COMMON_ERROR_LOGS_API_HANDLE)
public class CommonErrorLogsController {
    private ICommonErrorLogsService commonErrorLogsService;

    @Autowired
    public CommonErrorLogsController(ICommonErrorLogsService commonErrorLogsService) {
        this.commonErrorLogsService = commonErrorLogsService;
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = CommonErrorLogsController.MyResponseClass.class, message = CommonErrorLogsConstants.LIST_SUCCESSFUL, responseContainer = CommonErrorLogsConstants.RESPONSE_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return commonErrorLogsService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = CommonErrorLogsController.MyResponseClass.class, message = CommonErrorLogsConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = CommonErrorLogsConstants.COMMON_ERROR_LOGS_ID) @RequestParam Optional<Long> id, @ApiParam(value = CommonErrorLogsConstants.COMMON_ERROR_LOGS_GUID) @RequestParam Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return commonErrorLogsService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    private class MyResponseClass extends RunnerResponse<CommonErrorLogsResponse> {
    }

}
