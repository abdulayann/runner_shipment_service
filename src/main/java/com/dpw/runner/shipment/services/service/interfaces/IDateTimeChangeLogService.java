package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.entity.DateTimeChangeLog;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.DateType;

import java.time.LocalDateTime;
import java.util.List;

public interface IDateTimeChangeLogService {

    void createEntryFromShipment(ShipmentRequest entity, ShipmentDetails oldEntity);

    List<DateTimeChangeLog> getDateTimeChangeLog(Long shipmentId);

    void deleteDateTimeLogs(Long shipmentId);
    void saveDateTimeChangeLog(DateType dateType, LocalDateTime dateTime, Long shipmentId, String source);
}
