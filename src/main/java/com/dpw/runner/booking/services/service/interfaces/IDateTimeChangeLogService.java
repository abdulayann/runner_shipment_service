package com.dpw.runner.booking.services.service.interfaces;

import com.dpw.runner.booking.services.entity.DateTimeChangeLog;
import com.dpw.runner.booking.services.entity.enums.DateType;

import java.time.LocalDateTime;
import java.util.List;

public interface IDateTimeChangeLogService {
    List<DateTimeChangeLog> getDateTimeChangeLog(Long shipmentId);

    void deleteDateTimeLogs(Long shipmentId);
    void saveDateTimeChangeLog(DateType dateType, LocalDateTime dateTime, Long shipmentId, String source);
}
