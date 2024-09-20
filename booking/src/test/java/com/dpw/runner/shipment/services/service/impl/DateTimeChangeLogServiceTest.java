package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.commons.constants.DateTimeChangeLogConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IDateTimeChangeLogDao;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.UpstreamDateUpdateRequest;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.DateTimeChangeLog;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.DateType;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class DateTimeChangeLogServiceTest extends CommonMocks {


    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IDateTimeChangeLogDao dateTimeChangeLogDao;

    @InjectMocks
    DateTimeChangeLogService dateTimeChangeLogService;


    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
    private static ShipmentDetails testShipment;


    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
    }

    @BeforeEach
    void setup() {
        var tenantSettings = new V1TenantSettingsResponse();
        TenantSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);
        testShipment = jsonTestUtility.getTestShipment();
    }

    @Test
    void createEntryFromShipment() {
        var tenantSettings = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettings.setEnableEstimateAndActualDateTimeUpdates(Boolean.TRUE);

        ShipmentRequest shipmentRequest = objectMapper.convertValue(testShipment, ShipmentRequest.class);

        UpstreamDateUpdateRequest dateUpdateRequest = new UpstreamDateUpdateRequest();
        UpstreamDateUpdateRequest.DateAndSource ds = new UpstreamDateUpdateRequest.DateAndSource();
        ds.setDateTime(LocalDateTime.now());
        ds.setSource(DateTimeChangeLogConstants.TRACKING_SERVICE_SOURCE);

        dateUpdateRequest.setAta(ds);
        dateUpdateRequest.setAtd(ds);
        dateUpdateRequest.setEta(ds);
        dateUpdateRequest.setEtd(ds);

        shipmentRequest.setDateUpdateRequest(dateUpdateRequest);
        mockTenantSettings();
        dateTimeChangeLogService.createEntryFromShipment(shipmentRequest, testShipment);

        verify(dateTimeChangeLogDao, atLeast(1)).create(any());

    }

    @Test
    void createEntryFromShipmentWhenCarrierInfoChanges() {
        var tenantSettings = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettings.setEnableEstimateAndActualDateTimeUpdates(Boolean.TRUE);

        ShipmentRequest shipmentRequest = objectMapper.convertValue(testShipment, ShipmentRequest.class);

        UpstreamDateUpdateRequest dateUpdateRequest = new UpstreamDateUpdateRequest();
        UpstreamDateUpdateRequest.DateAndSource ds = new UpstreamDateUpdateRequest.DateAndSource();
        LocalDateTime mockDateTime = LocalDateTime.now();
        ds.setDateTime(mockDateTime);
        ds.setSource(DateTimeChangeLogConstants.TRACKING_SERVICE_SOURCE);

        dateUpdateRequest.setAta(ds);
        dateUpdateRequest.setAtd(ds);
        dateUpdateRequest.setEta(ds);
        dateUpdateRequest.setEtd(ds);

        shipmentRequest.setDateUpdateRequest(dateUpdateRequest);
        shipmentRequest.getCarrierDetails().setShippingLine("new shipping line");
        shipmentRequest.setDateUpdateRequest(dateUpdateRequest);
        mockTenantSettings();
        dateTimeChangeLogService.createEntryFromShipment(shipmentRequest, testShipment);

        verify(dateTimeChangeLogDao, atLeast(1)).create(any());

    }

    @Test
    void getDateTimeChangeLog() {
        Long shipmentId = 1L;
        List<DateTimeChangeLog> dateTimeChangeLogs = Collections.EMPTY_LIST;
        when(dateTimeChangeLogDao.getLogsForShipmentId(shipmentId)).thenReturn(dateTimeChangeLogs);
        var res = dateTimeChangeLogService.getDateTimeChangeLog(shipmentId);

        assertEquals(dateTimeChangeLogs, res);
    }

    @Test
    void deleteDateTimeLogs() {
        Long shipmentId = 1L;
        List<DateTimeChangeLog> dateTimeChangeLogs = List.of(new DateTimeChangeLog());
        when(dateTimeChangeLogDao.getLogsForShipmentId(shipmentId)).thenReturn(dateTimeChangeLogs);

        dateTimeChangeLogService.deleteDateTimeLogs(shipmentId);

        verify(dateTimeChangeLogDao, times(1)).deleteAll(anyList());
    }

    @Test
    void saveDateTimeChangeLog() {
        dateTimeChangeLogService.saveDateTimeChangeLog(DateType.ATA, LocalDateTime.now(), 1L, DateTimeChangeLogConstants.EVENT_SOURCE);
        verify(dateTimeChangeLogDao, times(1)).create(any());
    }
}