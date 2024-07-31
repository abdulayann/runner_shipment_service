package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.commons.entity.DateTimeChangeLog;
import com.dpw.runner.shipment.services.commons.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.commons.entity.enums.DateType;

import java.time.LocalDateTime;
import java.util.List;

public interface IDateTimeChangeLogService {

    void createEntryFromShipment(ShipmentRequest entity, ShipmentDetails oldEntity);

    List<DateTimeChangeLog> getDateTimeChangeLog(Long shipmentId);

    void deleteDateTimeLogs(Long shipmentId);
    void saveDateTimeChangeLog(DateType dateType, LocalDateTime dateTime, Long shipmentId, String source);
}
