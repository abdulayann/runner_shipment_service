package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.SailingScheduleRequest;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISailingScheduleService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import java.util.List;


@RestController
@RequestMapping(ShipmentConstants.SHIPMENT_API_HANDLE)
@Slf4j
public class SailingScheduleController {

    ISailingScheduleService sailingScheduleService;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    public SailingScheduleController(ISailingScheduleService sailingScheduleService) {
        this.sailingScheduleService = sailingScheduleService;
    }

    public static final String CREATE_SUCCESSFUL = "Successful Sailing Schedules Data Creation";

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CREATE_SUCCESSFUL, response = RunnerResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.SAILING_SCHEDULE_API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid List<SailingScheduleRequest> requestList) {
        log.info("Received Shipment create request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requestList));
        String responseMsg;
        try {
            return sailingScheduleService.create(requestList);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
