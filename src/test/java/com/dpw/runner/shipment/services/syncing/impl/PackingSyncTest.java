package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.commons.entity.Containers;
import com.dpw.runner.shipment.services.commons.entity.Packing;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class PackingSyncTest {
    @InjectMocks
    private PackingSync packingSync;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IContainerDao containerDao;
    @Mock
    private SyncEntityConversionService syncEntityConversionService;
    @Mock
    private ISyncService syncService;

    /**
     * Method under test: {@link PackingSync#sync(List, Long, Long)}
     */
    @Test
    void sync() {
        var inputPacking = new Packing();
        inputPacking.setGuid(UUID.randomUUID());

        when(containerDao.findByShipmentId(22L)).thenReturn(List.of(new Containers()));
        when(syncEntityConversionService.packingsV2ToV1(any(), any(), any(), any())).thenReturn(List.of(new PackingRequestV2()));
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(100));
        packingSync.sync(List.of(inputPacking), 11L, 22L);

        Mockito.verify(containerDao, times(1)).findByShipmentId(any());
    }


    @Test
    void sync2() {
        var inputPacking = new Packing();
        when(syncEntityConversionService.packingsV2ToV1(any(), any(), any(), any())).thenReturn(List.of(new PackingRequestV2()));

        packingSync.sync(List.of(inputPacking), 11L, null);
        Mockito.verify(syncEntityConversionService, times(1)).packingsV2ToV1(any(),any(),any(),any());
    }


}
