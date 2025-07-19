package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.SailingSchedule;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;


@Generated
public interface ISailingScheduleRepository extends JpaRepository<SailingSchedule, Long> {

    List<SailingSchedule> findByShipmentId(Long shipmentId);
}
