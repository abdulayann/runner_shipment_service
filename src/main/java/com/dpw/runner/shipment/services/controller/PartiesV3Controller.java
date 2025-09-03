package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IPartiesV3Service;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

@Slf4j
@RestController
@RequestMapping(value = PartiesConstants.PARTIES_API_HANDLE_V3)
public class PartiesV3Controller {
    private final IPartiesV3Service partiesService;
    private final JsonHelper jsonHelper;


    @Autowired
    public PartiesV3Controller(IPartiesV3Service partiesService, JsonHelper jsonHelper) {
        this.partiesService = partiesService;
        this.jsonHelper = jsonHelper;
    }

    private static class MyResponseClass extends RunnerResponse<PartiesResponse> {}
    private static class MyListResponseClass extends RunnerListResponse<PartiesResponse> {}

    @PostMapping(ApiConstants.API_CREATE)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = PartiesConstants.PARTIES_CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid @NonNull PartiesRequest partiesRequest) {
        log.info("Received Party Create request with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(partiesRequest));
        return ResponseHelper.buildSuccessResponse(partiesService.create(partiesRequest));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = PartiesConstants.PARTIES_UPDATE_SUCCESSFUL , response = MyResponseClass.class)
    })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid @NonNull PartiesRequest partiesRequest) {
        log.info("Received Party Update request with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(partiesRequest));
        return ResponseHelper.buildSuccessResponse(partiesService.update(partiesRequest));

    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PartiesConstants.PARTIES_DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid PartiesRequest partiesRequest) {
        log.info("Received Party Delete request with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(partiesRequest));
        return ResponseHelper.buildSuccessResponse(partiesService.delete(partiesRequest));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyListResponseClass.class, message = PartiesConstants.PARTIES_LIST_SUCCESSFUL, responseContainer = PartiesConstants.PARTIES_LIST_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @NonNull @Valid ListCommonRequest listCommonRequest) {
        List<PartiesResponse> partiesList = partiesService.list(listCommonRequest);
        List<IRunnerResponse> responseList = partiesList.stream().map(p -> (IRunnerResponse) p).toList();
        return ResponseHelper.buildListSuccessResponse(responseList);
    }

}
