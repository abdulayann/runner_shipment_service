package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.TrackingEventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.impl.ApiKeyAuthenticationService;
import com.dpw.runner.shipment.services.service.interfaces.IEventsV3Service;
import com.dpw.runner.shipment.services.syncing.Entity.EventsRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IEventsSync;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import java.util.List;
import java.util.Optional;
import javax.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@SuppressWarnings("ALL")
@RestController
@RequestMapping(EventConstants.EVENT_V3_API_HANDLE)
@Slf4j
public class EventsV3Controller {

    private final IEventsV3Service eventService;
    private final ApiKeyAuthenticationService authenticationService;
    private final IEventsSync eventsSync;

    @Autowired
    public EventsV3Controller(IEventsV3Service eventService, ApiKeyAuthenticationService authenticationService, IEventsSync eventsSync) {
        this.eventService = eventService;
        this.authenticationService = authenticationService;
        this.eventsSync = eventsSync;
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EventConstants.EVENT_LIST_SUCCESS),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(EventConstants.LIST_EVENT_DETAILS_V2)
    public ResponseEntity<IRunnerResponse> listEventsV2(@RequestBody @Valid TrackingEventsRequest request,
            @RequestHeader(value = "x-source", required = false) String xSource) {
        String responseMsg;
        try {
            List<EventsResponse> eventsResponseList = eventService.listV2(CommonRequestModel.buildRequest(request), xSource);
            return ResponseHelper.buildSuccessResponse(eventsResponseList);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : EventConstants.ERROR_FETCHING_EVENTS_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EventConstants.EVENT_CREATE_SUCCESS),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
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

    @ApiResponses(value = {@ApiResponse(code = 200, message = EventConstants.EVENT_DELETE_SUCCESS)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return eventService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = EventConstants.EVENT_LIST_SUCCESS)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return eventService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = EventConstants.EVENTS_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = EventConstants.EVENT_ID, required = true) @RequestParam Long id,
            @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return eventService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = EventConstants.EVENT_UPDATE_SUCCESS)})
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
            @ApiResponse(code = 200, message = ShipmentConstants.SHIPMENT_SYNC_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.SYNC)
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> syncEventsToService(@RequestBody @Valid EventsRequestV2 request,
            @RequestParam(required = false, defaultValue = "true") boolean checkForSync) {
        return ResponseHelper.buildSuccessResponse();
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = EventConstants.TRACK_EVENTS_FETCH_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @GetMapping(EventConstants.TRACK_EVENT_DETAILS)
    public ResponseEntity<IRunnerResponse> trackEventDetails(@RequestParam(name = "shipmentId") Optional<Long> id,
            @RequestParam(name = "consolidationId") Optional<Long> consolidationId) {
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
            @ApiResponse(code = 200, message = EventConstants.TRACK_EVENTS_FETCH_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
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
