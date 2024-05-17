package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.DateTimeChangeLog;

public interface IDateTimeChangeLogDao {

    DateTimeChangeLog create(DateTimeChangeLog dateTimeChangeLog);
    void delete(DateTimeChangeLog dateTimeChangeLog);
}
