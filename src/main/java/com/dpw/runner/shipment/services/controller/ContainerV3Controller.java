package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.ContainerConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.response.BulkContainerUpdateResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import java.util.List;
import javax.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(ContainerConstants.CONTAINER_V3_API_HANDLE)
@Slf4j
public class ContainerV3Controller {

    private final JsonHelper jsonHelper;
    private final IContainerV3Service containerV3Service;

    public ContainerV3Controller(JsonHelper jsonHelper, IContainerV3Service containerV3Service) {
        this.jsonHelper = jsonHelper;
        this.containerV3Service = containerV3Service;
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_CREATE_SUCCESSFUL, response = ConsolidationDetailsResponse.class),
            @ApiResponse(code = 404, message = ContainerConstants.NO_DATA, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@Valid @RequestBody ContainerRequest containerRequest) {
        log.info("Received Container Create request with RequestId: {} and payload : {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(containerRequest));
        return ResponseHelper.buildSuccessResponse(containerV3Service.create(containerRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CONTAINER_UPDATE_SUCCESSFUL, response = BulkContainerUpdateResponse.class)})
    @PutMapping(value = ApiConstants.API_UPDATE_BULK)
    public ResponseEntity<IRunnerResponse> updateBulk(@RequestBody List<ContainerRequest> request) {
        return ResponseHelper.buildSuccessResponse(containerV3Service.updateBulk(request));
    }


}
