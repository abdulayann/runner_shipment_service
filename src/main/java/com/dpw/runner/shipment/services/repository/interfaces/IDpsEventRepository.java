package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.DpsEvent;
import org.springframework.data.jpa.repository.JpaRepository;

public interface IDpsEventRepository extends JpaRepository<DpsEvent, Long> {

}
