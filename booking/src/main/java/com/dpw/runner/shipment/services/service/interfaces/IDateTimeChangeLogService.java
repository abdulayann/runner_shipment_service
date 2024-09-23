package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.entity.DateTimeChangeLog;
import com.dpw.runner.shipment.services.entity.enums.DateType;

import java.time.LocalDateTime;
import java.util.List;

public interface IDateTimeChangeLogService {
    List<DateTimeChangeLog> getDateTimeChangeLog(Long shipmentId);

    void deleteDateTimeLogs(Long shipmentId);
    void saveDateTimeChangeLog(DateType dateType, LocalDateTime dateTime, Long shipmentId, String source);
}
