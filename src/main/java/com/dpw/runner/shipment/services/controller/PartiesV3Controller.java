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
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
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
            @ApiResponse(responseCode = "200", description = PartiesConstants.PARTIES_CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid @NonNull PartiesRequest partiesRequest) {
        log.info("Received Party Create request with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(partiesRequest));
        return ResponseHelper.buildSuccessResponse(partiesService.create(partiesRequest));
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = PartiesConstants.PARTIES_UPDATE_SUCCESSFUL , content = @Content(schema = @Schema(implementation = MyResponseClass.class)))
    })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid @NonNull PartiesRequest partiesRequest) {
        log.info("Received Party Update request with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(partiesRequest));
        return ResponseHelper.buildSuccessResponse(partiesService.update(partiesRequest));

    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = PartiesConstants.PARTIES_DELETE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
    @PostMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid PartiesRequest partiesRequest) {
        log.info("Received Party Delete request with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(partiesRequest));
        return ResponseHelper.buildSuccessResponse(partiesService.delete(partiesRequest));
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", content = @Content( array = @ArraySchema(schema = @Schema(implementation = MyListResponseClass.class))), description = PartiesConstants.PARTIES_LIST_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @NonNull @Valid ListCommonRequest listCommonRequest) {
        List<PartiesResponse> partiesList = partiesService.list(listCommonRequest);
        List<IRunnerResponse> responseList = partiesList.stream().map(p -> (IRunnerResponse) p).toList();
        return ResponseHelper.buildListSuccessResponse(responseList);
    }

}
