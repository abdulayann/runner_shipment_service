package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.http.HttpStatus;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class HblSyncTest {
    @InjectMocks
    private HblSync hblSync;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    ModelMapper modelMapper;

    @Mock
    private ISyncService syncService;

    @Mock
    private CommonUtils commonUtils;

    @BeforeEach
    void setUp() {
        SyncingContext.setContext(Boolean.TRUE);
    }

    /**
     * Method under test: {@link HblSync#sync(Hbl, String)}
     */
    @Test
    void sync() {
        var inputHbl = new Hbl();
        var guid = UUID.randomUUID();
        var hblContainer1 = new HblContainerDto();
        var hblContainer2 = new HblContainerDto();
        hblContainer2.setHazardous(0);

        inputHbl.setHblContainer(List.of(hblContainer1, hblContainer2));
        when(shipmentDao.findById(any())).thenReturn(Optional.of(new ShipmentDetails()));
        when(jsonHelper.convertValue(any(), eq(HblRequestV2.class))).thenReturn(new HblRequestV2());
        when(modelMapper.map(any(), eq(HblDataRequestV2.class))).thenReturn(new HblRequestV2());
        when(commonUtils.convertToClass(any(), eq(HblContainerRequestV2.class))).thenReturn(new HblContainerRequestV2());

        var responseEntity = hblSync.sync(inputHbl, StringUtility.convertToString(guid));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }



    @Test
    void sync2() {
        var inputHbl = new Hbl();
        var guid = UUID.randomUUID();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(new ShipmentDetails()));
        when(jsonHelper.convertValue(any(), eq(HblRequestV2.class))).thenReturn(new HblRequestV2());
        when(modelMapper.map(any(), eq(HblDataRequestV2.class))).thenReturn(new HblRequestV2());

        var responseEntity = hblSync.sync(inputHbl, StringUtility.convertToString(guid));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void sync3() {
        var inputHbl = new Hbl();
        var guid = UUID.randomUUID();

        inputHbl.setHblContainer(List.of());
        when(shipmentDao.findById(any())).thenReturn(Optional.of(new ShipmentDetails()));
        when(jsonHelper.convertValue(any(), eq(HblRequestV2.class))).thenReturn(new HblRequestV2());
        when(modelMapper.map(any(), eq(HblDataRequestV2.class))).thenReturn(new HblRequestV2());

        var responseEntity = hblSync.sync(inputHbl, StringUtility.convertToString(guid));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

}
