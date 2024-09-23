package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IDateTimeChangeLogDao;
import com.dpw.runner.shipment.services.entity.DateTimeChangeLog;
import com.dpw.runner.shipment.services.entity.enums.DateType;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;


@Service
public class DateTimeChangeLogService implements IDateTimeChangeLogService {

    private IDateTimeChangeLogDao dateTimeChangeLogDao;

    private CommonUtils commonUtils;

    @Autowired
    DateTimeChangeLogService(IDateTimeChangeLogDao dateTimeChangeLogDao, CommonUtils commonUtils) {
        this.dateTimeChangeLogDao = dateTimeChangeLogDao;
        this.commonUtils = commonUtils;
    }

    @Override
    public List<DateTimeChangeLog> getDateTimeChangeLog(Long shipmentId) {
        return new ArrayList<>(dateTimeChangeLogDao.getLogsForShipmentId(shipmentId));
    }

    @Override
    public void deleteDateTimeLogs(Long shipmentId) {
        var dateTimeChangeLogs = dateTimeChangeLogDao.getLogsForShipmentId(shipmentId);
        if(dateTimeChangeLogs != null && !dateTimeChangeLogs.isEmpty()) {
            dateTimeChangeLogDao.deleteAll(dateTimeChangeLogs);
        }
    }

    @Override
    public void saveDateTimeChangeLog(DateType dateType, LocalDateTime dateTime, Long shipmentId, String source) {
        var dateTimeChangeLog = DateTimeChangeLog.builder()
            .dateType(dateType)
            .currentValue(dateTime)
            .sourceOfUpdate(source)
            .shipmentId(shipmentId)
            .build();
        dateTimeChangeLogDao.create(dateTimeChangeLog);
    }



}
