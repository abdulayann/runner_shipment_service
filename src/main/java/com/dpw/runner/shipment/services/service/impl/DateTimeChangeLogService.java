package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IDateTimeChangeLogDao;
import com.dpw.runner.shipment.services.dto.response.DateTimeChangeLogResponse;
import com.dpw.runner.shipment.services.dto.response.UpstreamDateUpdateResponse;
import com.dpw.runner.shipment.services.entity.DateTimeChangeLog;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.DateType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;


@Service
public class DateTimeChangeLogService implements IDateTimeChangeLogService {

    private JsonHelper jsonHelper;
    private IDateTimeChangeLogDao dateTimeChangeLogDao;

    @Autowired
    DateTimeChangeLogService(JsonHelper jsonHelper, IDateTimeChangeLogDao dateTimeChangeLogDao) {
        this.jsonHelper = jsonHelper;
        this.dateTimeChangeLogDao = dateTimeChangeLogDao;
    }


    @Override
    public DateTimeChangeLog create(ShipmentDetails shipmentDetails, UpstreamDateUpdateResponse upstreamDates) {

        DateTimeChangeLog entity = new DateTimeChangeLog();

        // if upstream dates are equal then the source is tracking
        // otherwise manual by User

        dateTimeChangeLogDao.create(entity);

        return entity;
    }

    @Override
    public List<DateTimeChangeLog> getDateTimeChangeLog(Long shipmentId) {
        List<DateTimeChangeLog> res = new ArrayList<>();

        DateTimeChangeLog ataDateTimeChangeLog = DateTimeChangeLog.builder()
            .dateType(DateType.ATA)
            .currentValue(LocalDateTime.now())
            .sourceOfUpdate("Tracking Service")
            .shipmentId(1L)
            .build();
        ataDateTimeChangeLog.setUpdatedAt(LocalDateTime.now().minusDays(3));

        res.add(ataDateTimeChangeLog);

        res.addAll(dateTimeChangeLogDao.getLogsForShipmentId(shipmentId));

        return res;
    }

    @Override
    public void deleteDateTimeLogs(Long shipmentId) {
        var dateTimeChangeLogs = dateTimeChangeLogDao.getLogsForShipmentId(shipmentId);
        if(dateTimeChangeLogs != null && !dateTimeChangeLogs.isEmpty()) {
            dateTimeChangeLogDao.deleteAll(dateTimeChangeLogs);
        }
    }


}
