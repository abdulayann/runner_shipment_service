package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.syncing.Entity.EventsRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.V1DataSyncRequest;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IEventsSync;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

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


    @Autowired
    private ISyncService syncService;

    @Override
    public ResponseEntity<?> sync(List<Events> eventsList) {
        if (!Boolean.TRUE.equals(SyncingContext.getContext()))
            return ResponseHelper.buildSuccessResponse();
        List<EventsRequestV2> eventsRequestV2List;
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

            syncService.pushToKafka(json, idsString, guidsString, "Events", UUID.randomUUID().toString());
            return ResponseHelper.buildSuccessResponse(eventsList);
        }
        return null;
    }


}
