package com.dpw.runner.booking.services.dao.interfaces;

import com.dpw.runner.booking.services.entity.DateTimeChangeLog;

import java.util.List;

public interface IDateTimeChangeLogDao {

    DateTimeChangeLog create(DateTimeChangeLog dateTimeChangeLog);
    void delete(DateTimeChangeLog dateTimeChangeLog);
    List<DateTimeChangeLog> getLogsForShipmentId(Long shipmentId);
    void deleteAll(List<DateTimeChangeLog> dateTimeChangeLogs);
}
