package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IDateTimeChangeLogDao;
import com.dpw.runner.shipment.services.entity.DateTimeChangeLog;
import com.dpw.runner.shipment.services.repository.interfaces.IDateTimeChangeLogRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class DateTimeChangeLogDao implements IDateTimeChangeLogDao {

    private IDateTimeChangeLogRepository dateTimeChangeLogRepository;

    @Autowired
    DateTimeChangeLogDao(IDateTimeChangeLogRepository dateTimeChangeLogRepository) {
        this.dateTimeChangeLogRepository = dateTimeChangeLogRepository;
    }

    @Override
    public DateTimeChangeLog create(DateTimeChangeLog dateTimeChangeLog) {
        var res = dateTimeChangeLogRepository.save(dateTimeChangeLog);
        return res;
    }

    @Override
    public void delete(DateTimeChangeLog dateTimeChangeLog) {
        dateTimeChangeLogRepository.delete(dateTimeChangeLog);
    }
}
