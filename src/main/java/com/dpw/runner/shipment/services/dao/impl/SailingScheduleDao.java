package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ISailingScheduleDao;
import com.dpw.runner.shipment.services.entity.SailingSchedule;
import com.dpw.runner.shipment.services.repository.interfaces.ISailingScheduleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;


@Repository
public class SailingScheduleDao implements ISailingScheduleDao {

    @Autowired
    private ISailingScheduleRepository sailingScheduleRepository;

    @Override
    public List<SailingSchedule> findByShipmentId(Long shipmentId) {
        return sailingScheduleRepository.findByShipmentId(shipmentId);
    }

    @Override
    public SailingSchedule save(SailingSchedule sailingSchedule) {
        return sailingScheduleRepository.save(sailingSchedule);
    }
    @Override
    public List<SailingSchedule> saveAll(List<SailingSchedule> sailingScheduleList) {
        return sailingScheduleRepository.saveAll(sailingScheduleList);
    }
}
