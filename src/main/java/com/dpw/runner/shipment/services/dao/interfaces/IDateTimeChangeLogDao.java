package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.DateTimeChangeLog;

import java.util.List;

public interface IDateTimeChangeLogDao {

    DateTimeChangeLog create(DateTimeChangeLog dateTimeChangeLog);

    void delete(DateTimeChangeLog dateTimeChangeLog);

    List<DateTimeChangeLog> getLogsForShipmentId(Long shipmentId);

    void deleteAll(List<DateTimeChangeLog> dateTimeChangeLogs);
}
