package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.TrackingEventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.impl.ApiKeyAuthenticationService;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.syncing.Entity.EventsRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IEventsSync;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
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

import java.util.List;
import java.util.Optional;

@SuppressWarnings("ALL")
@RestController
@RequestMapping(EventConstants.EVENT_API_HANDLE)
@Slf4j
public class EventsController {
    private final IEventService eventService;
    private final ApiKeyAuthenticationService authenticationService;
    private final IEventsSync eventsSync;
    private class MyEventsResponseClass extends RunnerResponse<EventsResponse> {}
    private class MyEventsListResponseClass extends RunnerListResponse<EventsResponse> {}

    @Autowired
    public EventsController(IEventService eventService, IEventsSync eventsSync, ApiKeyAuthenticationService authenticationService) {
        this.eventService = eventService;
        this.eventsSync = eventsSync;
        this.authenticationService = authenticationService;
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = EventConstants.EVENT_CREATE_SUCCESS, content = @Content(schema = @Schema(implementation = MyEventsResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createBookingCarriageData(@RequestBody @Valid EventsRequest request) {
        String responseMsg;
        try {
            return eventService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = EventConstants.EVENT_DELETE_SUCCESS, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return eventService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = EventConstants.EVENT_LIST_SUCCESS, content = @Content(schema = @Schema(implementation = MyEventsListResponseClass.class)))})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return eventService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = EventConstants.EVENTS_RETRIEVE_BY_ID_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyEventsResponseClass.class)))})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@Parameter(description = EventConstants.EVENT_ID, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return eventService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = EventConstants.EVENT_UPDATE_SUCCESS, content = @Content(schema = @Schema(implementation = MyEventsResponseClass.class)))})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid EventsRequest request) {
        String responseMsg;
        try {
            return eventService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = ShipmentConstants.SHIPMENT_SYNC_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(ApiConstants.SYNC)
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> syncEventsToService(@RequestBody @Valid EventsRequestV2 request, @RequestParam(required = false, defaultValue = "true") boolean checkForSync) {
        return ResponseHelper.buildSuccessResponse();
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = EventConstants.TRACK_EVENTS_FETCH_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyEventsListResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @GetMapping(EventConstants.TRACK_EVENT_DETAILS)
    public ResponseEntity<IRunnerResponse> trackEventDetails(@RequestParam(name = "shipmentId") Optional<Long> id, @RequestParam(name = "consolidationId") Optional<Long> consolidationId) {
        String responseMsg;
        try {
            TrackingEventsRequest request = new TrackingEventsRequest();
            request.setShipmentId(id.orElse(null));
            request.setConsolidationId(consolidationId.orElse(null));
            return eventService.trackEvents(request);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : EventConstants.ERROR_FETCHING_EVENTS_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = EventConstants.TRACK_EVENTS_FETCH_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyEventsListResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(EventConstants.TRACK_EVENT_DETAILS_V2)
    public ResponseEntity<IRunnerResponse> trackEventDetailsV2(@RequestBody @Valid TrackingEventsRequest request) {
        String responseMsg;
        try {
            return eventService.trackEvents(request);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : EventConstants.ERROR_FETCHING_EVENTS_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Push Tracking Events", content = @Content(schema = @Schema(implementation = MyEventsListResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping("/push-tracking-events")
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> pushTrackingEvents(@RequestHeader(ApiConstants.X_API_KEY) String xApiKey,
            @RequestBody @Valid TrackingServiceApiResponse.Container request) {
        String responseMsg;
        try {
            authenticationService.authenticate(Constants.TRACKING_PUSH_API, xApiKey);
            return eventService.pushTrackingEvents(request);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : EventConstants.ERROR_FETCHING_EVENTS_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = EventConstants.EVENT_LIST_SUCCESS, content = @Content(schema = @Schema(implementation = MyEventsListResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(EventConstants.LIST_EVENT_DETAILS_V2)
    public ResponseEntity<IRunnerResponse> listEventsV2(@RequestBody @Valid TrackingEventsRequest request) {
        String responseMsg;
        try {
            return ResponseHelper.buildSuccessResponse(eventService.listWithoutTenantFilter(request, null));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : EventConstants.ERROR_FETCHING_EVENTS_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(ApiConstants.API_SYNC_EVENTS)
    public ResponseEntity<IRunnerResponse> getEvents(@RequestBody @Valid List<Events> request) {
        String responseMsg;
        try {
            return eventsSync.sync(request);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
