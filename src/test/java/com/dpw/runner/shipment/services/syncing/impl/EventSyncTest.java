package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.syncing.Entity.EventsRequestV2;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class EventSyncTest {
    @InjectMocks
    private EventsSync eventsSync;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private SyncEntityConversionService syncEntityConversionService;
    @Mock
    private ISyncService syncService;

    @BeforeEach
    void setUp() {
        SyncingContext.setContext(Boolean.TRUE);
    }

    /**
     * Method under test: {@link EventsSync#sync(List)}
     */
    @Test
    void sync() {
        var inputEvent = new Events();
        inputEvent.setGuid(UUID.randomUUID());

        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(100));
        when(syncEntityConversionService.eventsV2ToV1(any())).thenReturn(List.of(new EventsRequestV2()));

        var responseEntity = eventsSync.sync(List.of(inputEvent));

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

    }

    @Test
    void sync2() {
        var responseEntity = eventsSync.sync(List.of());
        assertNull(responseEntity);
    }

    @Test
    void sync3() {
        var responseEntity = eventsSync.sync(null);
        assertNull(responseEntity);
    }




}
