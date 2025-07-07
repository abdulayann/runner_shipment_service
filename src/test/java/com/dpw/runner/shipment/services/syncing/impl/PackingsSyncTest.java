package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class PackingsSyncTest {
    @InjectMocks
    private PackingsSync packingsSync;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IContainerDao containerDao;
    @Mock
    private SyncEntityConversionService syncEntityConversionService;
    @Mock
    private ISyncService syncService;

    @BeforeEach
    void setUp() {
        SyncingContext.setContext(Boolean.TRUE);
    }

    /**
     * Method under test: {@link PackingsSync#sync(List, String)}
     */
    @Test
    void sync() {
        var inputPacking = new Packing();
        inputPacking.setGuid(UUID.randomUUID());
        inputPacking.setContainerId(11L);

        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(new Containers())));
        when(syncEntityConversionService.packingsV2ToV1(any(), any(), any(), any())).thenReturn(List.of(new PackingRequestV2()));
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(100));

        var responseEntity = packingsSync.sync(List.of(inputPacking), UUID.randomUUID().toString());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void sync2() {
        var inputPacking = new Packing();
        inputPacking.setGuid(UUID.randomUUID());
        inputPacking.setContainerId(11L);

        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        when(syncEntityConversionService.packingsV2ToV1(any(), any(), any(), any())).thenReturn(List.of(new PackingRequestV2()));
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(100));

        var responseEntity = packingsSync.sync(List.of(inputPacking), UUID.randomUUID().toString());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void sync3() {
        var inputPacking = new Packing();
        inputPacking.setGuid(UUID.randomUUID());
        inputPacking.setContainerId(11L);

        when(containerDao.findAll(any(), any())).thenReturn(null);
        when(syncEntityConversionService.packingsV2ToV1(any(), any(), any(), any())).thenReturn(List.of(new PackingRequestV2()));
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(100));

        var responseEntity = packingsSync.sync(List.of(inputPacking), UUID.randomUUID().toString());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void sync4() {
        var responseEntity = packingsSync.sync(List.of(), UUID.randomUUID().toString());
        assertNull(responseEntity);
    }

    @Test
    void sync5() {
        var responseEntity = packingsSync.sync(null, UUID.randomUUID().toString());
        assertNull(responseEntity);
    }


}
