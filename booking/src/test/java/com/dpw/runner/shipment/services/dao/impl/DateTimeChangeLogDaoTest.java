package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.DateTimeChangeLog;
import com.dpw.runner.shipment.services.repository.interfaces.IDateTimeChangeLogRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)

class DateTimeChangeLogDaoTest {


    @Mock
    IDateTimeChangeLogRepository dateTimeChangeLogRepository;

    @InjectMocks
    DateTimeChangeLogDao dateTimeChangeLogDao;


    @Test
    void create() {
        DateTimeChangeLog dateTimeChangeLog = new DateTimeChangeLog();
        when(dateTimeChangeLogRepository.save(dateTimeChangeLog)).thenReturn(dateTimeChangeLog);

        var res = dateTimeChangeLogDao.create(dateTimeChangeLog);
        assertEquals(dateTimeChangeLog, res);
    }

    @Test
    void delete() {
        DateTimeChangeLog dateTimeChangeLog = new DateTimeChangeLog();
        dateTimeChangeLogDao.delete(dateTimeChangeLog);

        verify(dateTimeChangeLogRepository, times(1)).delete(dateTimeChangeLog);
    }

    @Test
    void getLogsForShipmentId() {
        DateTimeChangeLog dateTimeChangeLog = new DateTimeChangeLog();
        Long shipmentId = 1L;
        when(dateTimeChangeLogRepository.findByShipmentId(shipmentId))
            .thenReturn(List.of(dateTimeChangeLog));

        var res = dateTimeChangeLogDao.getLogsForShipmentId(shipmentId);
        assertEquals(List.of(dateTimeChangeLog), res);
    }

    @Test
    void deleteAll() {
        DateTimeChangeLog dateTimeChangeLog = new DateTimeChangeLog();
        dateTimeChangeLogDao.deleteAll(List.of(dateTimeChangeLog));

        verify(dateTimeChangeLogRepository, times(1)).deleteAll(anyList());
    }

}