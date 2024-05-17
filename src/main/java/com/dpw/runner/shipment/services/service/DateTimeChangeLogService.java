package com.dpw.runner.shipment.services.service;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IDateTimeChangeLogDao;
import com.dpw.runner.shipment.services.dto.response.DateTimeChangeLogResponse;
import com.dpw.runner.shipment.services.entity.DateTimeChangeLog;
import com.dpw.runner.shipment.services.entity.enums.DateType;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;


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
    public ResponseEntity<IRunnerResponse> getDateTimeChangeLog(Long shipmentId) {
        DateTimeChangeLog dateTimeChangeLog = DateTimeChangeLog.builder()
            .dateType(DateType.ATA)
            .currentValue(LocalDateTime.now())
            .sourceOfUpdate("Tracking Service")
            .shipmentId(1L)
            .build();

        dateTimeChangeLog.setUpdatedAt(LocalDateTime.now().minusDays(3));

        var response = jsonHelper.convertValue(dateTimeChangeLog, DateTimeChangeLogResponse.class);
        return ResponseHelper.buildListSuccessResponse(List.of(response));
    }

    @Override
    public void deleteChangeLogs(Long shipmentId) {

    }
}
