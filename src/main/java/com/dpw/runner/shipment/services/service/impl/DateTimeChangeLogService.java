package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.commons.constants.DateTimeChangeLogConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IDateTimeChangeLogDao;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.entity.DateTimeChangeLog;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.DateType;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;


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
    public void createEntryFromShipment(ShipmentRequest entity, ShipmentDetails oldEntity) {

        var tenantSettings = TenantSettingsDetailsContext.getCurrentTenantSettings();
        DateTimeChangeLog dateTimeChangeLog = null;
        // Only process if the feature is enabled
        if(Boolean.TRUE.equals(tenantSettings.getEnableEstimateAndActualDateTimeUpdates())) {
            // refresh all logs when carrier or vessel change
            if(checkIfCarrierOrVesselChanged(entity, oldEntity)) {
                deleteDateTimeLogs(oldEntity.getId());
                // generate default logs
                generateDefaultLogs(entity);
            }

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
        List<DateTimeChangeLog> res = new ArrayList<>();
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

    private boolean checkIfCarrierOrVesselChanged(ShipmentRequest entity, ShipmentDetails oldEntity) {
        if(Objects.isNull(oldEntity))
            return false;
        if(!Objects.equals(entity.getCarrierDetails().getShippingLine(), oldEntity.getCarrierDetails().getShippingLine()) ||
            !Objects.equals(entity.getCarrierDetails().getVessel(), oldEntity.getCarrierDetails().getVessel()))
            return true;

        return false;
    }

    private void generateDefaultLogs(ShipmentRequest shipmentDetails) {
        if(Objects.isNull(shipmentDetails) || Objects.isNull(shipmentDetails.getCarrierDetails()))
            return;

        if(shipmentDetails.getCarrierDetails().getAta() != null) {
            saveDateTimeChangeLog(DateType.ATA, shipmentDetails.getCarrierDetails().getAta(), shipmentDetails.getId(), DateTimeChangeLogConstants.DEFAULT_SOURCE);
        }

        if(shipmentDetails.getCarrierDetails().getAtd() != null) {
            saveDateTimeChangeLog(DateType.ATD, shipmentDetails.getCarrierDetails().getAtd(), shipmentDetails.getId(), DateTimeChangeLogConstants.DEFAULT_SOURCE);
        }

        if(shipmentDetails.getCarrierDetails().getEta() != null) {
            saveDateTimeChangeLog(DateType.ETA, shipmentDetails.getCarrierDetails().getEta(), shipmentDetails.getId(), DateTimeChangeLogConstants.DEFAULT_SOURCE);
        }

        if(shipmentDetails.getCarrierDetails().getEtd() != null) {
            saveDateTimeChangeLog(DateType.ETD, shipmentDetails.getCarrierDetails().getEtd(), shipmentDetails.getId(), DateTimeChangeLogConstants.DEFAULT_SOURCE);
        }

    }

    @Override
    public void saveDateTimeChangeLog(DateType dateType, LocalDateTime dateTime, Long ShipmentId, String source) {
        var dateTimeChangeLog = DateTimeChangeLog.builder()
            .dateType(dateType)
            .currentValue(dateTime)
            .sourceOfUpdate(source)
            .shipmentId(ShipmentId)
            .build();
        dateTimeChangeLogDao.create(dateTimeChangeLog);
    }



}
