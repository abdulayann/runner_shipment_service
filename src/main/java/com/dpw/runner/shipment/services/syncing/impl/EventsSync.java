package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.dto.v1.response.V1DataSyncResponse;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.EventsRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.V1DataSyncRequest;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IEventsSync;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Component
@Slf4j
public class EventsSync implements IEventsSync {

    @Autowired
    SyncEntityConversionService syncEntityConversionService;

    @Autowired
    JsonHelper jsonHelper;

    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private EmailServiceUtility emailServiceUtility;

    @Override
    public ResponseEntity<?> sync(List<Events> eventsList) {
        List<EventsRequestV2> eventsRequestV2List = new ArrayList<>();
        if(eventsList != null && eventsList.size() > 0) {
            eventsRequestV2List = syncEntityConversionService.eventsV2ToV1(eventsList);
            String json = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(eventsRequestV2List).module(SyncingConstants.EVENTS).build());

            String idsString = eventsList.stream()
                    .map(Events::getId)
                    .map(String::valueOf)
                    .collect(Collectors.joining(","));

            String guidsString = eventsList.stream()
                    .map(Events::getGuid)
                    .map(UUID::toString)
                    .collect(Collectors.joining(","));

            callSync(json, idsString, guidsString);
            return ResponseHelper.buildSuccessResponse(eventsList);
        }
        return null;
    }

    @Async
    private void callSync(String json, String ids, String guids) {
        retryTemplate.execute(ctx -> {
            log.info("Current retry : {}", ctx.getRetryCount());
            if (ctx.getLastThrowable() != null) {
                log.error("V1 error -> {}", ctx.getLastThrowable().getMessage());
            }

            V1DataSyncResponse response_ = v1Service.v1DataSync(json, null);
            if (!response_.getIsSuccess()) {
                try {
                    emailServiceUtility.sendEmailForSyncEntity(ids, guids,
                            "Events", response_.getError().toString());
                } catch (Exception ex) {
                    log.error("Not able to send email for sync failure for Events: " + ex.getMessage());
                }
            }
            return ResponseHelper.buildSuccessResponse(response_);
        });
    }

}
