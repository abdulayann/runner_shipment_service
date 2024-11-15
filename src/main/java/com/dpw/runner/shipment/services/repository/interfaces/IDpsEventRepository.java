package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.DpsEvent;
import com.dpw.runner.shipment.services.entity.enums.DpsExecutionStatus;
import java.util.List;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface IDpsEventRepository extends JpaRepository<DpsEvent, Long> {

    @Query(value = "SELECT * FROM dps_event WHERE entity_id = ?1 AND status = ?2", nativeQuery = true)
    List<DpsEvent> findDpsEventByGuidAndExecutionState(String guid, DpsExecutionStatus dpsExecutionStatus);

    DpsEvent findByExecutionId(UUID executionId);
}
