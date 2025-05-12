package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.TrackingEventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.impl.ApiKeyAuthenticationService;
import com.dpw.runner.shipment.services.service.interfaces.IEventsV3Service;
import com.dpw.runner.shipment.services.syncing.interfaces.IEventsSync;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

@SuppressWarnings("ALL")
@RestController
@RequestMapping(EventConstants.EVENT_V3_API_HANDLE)
@Slf4j
public class EventsV3Controller {
    private final IEventsV3Service eventService;
    private final ApiKeyAuthenticationService authenticationService;
    private final IEventsSync eventsSync;
    private class MyResponseClass extends RunnerResponse<EventsResponse> {}
    private class MyListResponseClass extends RunnerListResponse<EventsResponse> {}

    @Autowired
    public EventsV3Controller(IEventsV3Service eventService, IEventsSync eventsSync, ApiKeyAuthenticationService authenticationService) {
        this.eventService = eventService;
        this.eventsSync = eventsSync;
        this.authenticationService = authenticationService;
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EventConstants.EVENT_LIST_SUCCESS, response = EventsV3Controller.MyListResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(EventConstants.LIST_EVENT_DETAILS_V2)
    public ResponseEntity<IRunnerResponse> listEventsV2(@RequestBody @Valid TrackingEventsRequest request,
                                                        @RequestHeader(value = "x-source", required = false) String xSource) {
        String responseMsg;
        try {
            List<EventsResponse> eventsResponseList =  eventService.listV2(CommonRequestModel.buildRequest(request), xSource);
            return ResponseHelper.buildSuccessResponse(eventsResponseList);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : EventConstants.ERROR_FETCHING_EVENTS_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

}
