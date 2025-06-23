package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IDateTimeChangeLogDao;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.UpstreamDateUpdateRequest;
import com.dpw.runner.shipment.services.entity.DateTimeChangeLog;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.DateType;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;


@Service
public class DateTimeChangeLogService implements IDateTimeChangeLogService {

    private IDateTimeChangeLogDao dateTimeChangeLogDao;

    private CommonUtils commonUtils;

    @Autowired
    DateTimeChangeLogService(IDateTimeChangeLogDao dateTimeChangeLogDao, CommonUtils commonUtils) {
        this.dateTimeChangeLogDao = dateTimeChangeLogDao;
        this.commonUtils = commonUtils;
    }


    @Override
    public void createEntryFromShipment(ShipmentRequest entity, ShipmentDetails oldEntity) {

        var tenantSettings = commonUtils.getCurrentTenantSettings();
        // Only process if the feature is enabled
        if(Boolean.TRUE.equals(tenantSettings.getEnableEstimateAndActualDateTimeUpdates())) {
            // refresh all logs when carrier or vessel change
            if(checkIfCarrierOrVesselChanged(entity, oldEntity)) {
                deleteDateTimeLogs(oldEntity.getId());
                // generate default logs
            }

            if(entity.getDateUpdateRequest() == null)
                entity.setDateUpdateRequest(new UpstreamDateUpdateRequest());

            // create a new entry in case of changes in ata/atd/eta/etd
            if(entity.getDateUpdateRequest().getAta() != null) {
                var obj = entity.getDateUpdateRequest().getAta();
                saveDateTimeChangeLog(DateType.ATA, obj.getDateTime(), entity.getId(), obj.getSource());
            }
            if(entity.getDateUpdateRequest().getAtd() != null) {
                var obj = entity.getDateUpdateRequest().getAtd();
                saveDateTimeChangeLog(DateType.ATD, obj.getDateTime(), entity.getId(), obj.getSource());
            }
            if(entity.getDateUpdateRequest().getEta() != null) {
                var obj = entity.getDateUpdateRequest().getEta();
                saveDateTimeChangeLog(DateType.ETA, obj.getDateTime(), entity.getId(), obj.getSource());
            }
            if(entity.getDateUpdateRequest().getEtd() != null) {
                var obj = entity.getDateUpdateRequest().getEtd();
                saveDateTimeChangeLog(DateType.ETD, obj.getDateTime(), entity.getId(), obj.getSource());
            }
        }

    }

    @Override
    public List<DateTimeChangeLog> getDateTimeChangeLog(Long shipmentId) {
        return new ArrayList<>(dateTimeChangeLogDao.getLogsForShipmentId(shipmentId));
    }

    @Override
    public void deleteDateTimeLogs(Long shipmentId) {
        var dateTimeChangeLogs = dateTimeChangeLogDao.getLogsForShipmentId(shipmentId);
        if(dateTimeChangeLogs != null && !dateTimeChangeLogs.isEmpty()) {
            dateTimeChangeLogDao.deleteAll(dateTimeChangeLogs);
        }
    }

    private boolean checkIfCarrierOrVesselChanged(ShipmentRequest entity, ShipmentDetails oldEntity) {
        if(Objects.isNull(oldEntity))
            return false;
        return (!Objects.equals(entity.getCarrierDetails().getShippingLine(), oldEntity.getCarrierDetails().getShippingLine()) ||
            !Objects.equals(entity.getCarrierDetails().getVessel(), oldEntity.getCarrierDetails().getVessel()));

    }

    @Override
    public void saveDateTimeChangeLog(DateType dateType, LocalDateTime dateTime, Long shipmentId, String source) {
        var dateTimeChangeLog = DateTimeChangeLog.builder()
            .dateType(dateType)
            .currentValue(dateTime)
            .sourceOfUpdate(source)
            .shipmentId(shipmentId)
            .build();
        dateTimeChangeLogDao.create(dateTimeChangeLog);
    }



}
