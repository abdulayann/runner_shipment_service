package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.SailingSchedule;

import java.util.List;

public interface ISailingScheduleDao {

    SailingSchedule save(SailingSchedule sailingSchedule);
    List<SailingSchedule> saveAll(List<SailingSchedule> sailingScheduleList);
    List<SailingSchedule> findByShipmentId(Long shipmentId);
}
