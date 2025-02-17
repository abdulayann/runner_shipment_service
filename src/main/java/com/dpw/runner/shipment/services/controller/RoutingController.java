package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.RoutingConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.RoutingsUpdateRequest;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

import javax.validation.Valid;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@SuppressWarnings("ALL")
@RestController
@RequestMapping(RoutingConstants.ROUTING_API_HANDLE)
@Slf4j
public class RoutingController {

    @Autowired
    private IRoutingsService routingsService;

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = RoutingConstants.ROUTINGS_UPDATE_SUCCESS, response = MyListResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping("/update-routings")
    public ResponseEntity<IRunnerResponse> updateRoutings(@RequestBody @Valid RoutingsUpdateRequest routingsUpdateRequest) {
        return routingsService.updateRoutings(routingsUpdateRequest);
    }

    private class MyListResponseClass extends RunnerListResponse<RoutingsResponse> {
    }

}
