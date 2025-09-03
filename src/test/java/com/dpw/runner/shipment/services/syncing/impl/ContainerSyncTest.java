package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.syncing.Entity.ContainerRequestV2;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ContainerSyncTest {
    @InjectMocks
    private ContainerSync containerSync;
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
     * Method under test: {@link ContainerSync#sync(List, Long, Long)}
     */
    @Test
    void sync() {
        var input = new Containers();
        input.setGuid(UUID.randomUUID());
        input.setId(11L);

        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(100));
        when(syncEntityConversionService.containersV2ToV1(any())).thenReturn(List.of(new ContainerRequestV2()));

        assertDoesNotThrow(() ->containerSync.sync(List.of(input), 11L, 22L));
    }
}
