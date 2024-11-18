package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.DpsEvent;
import com.dpw.runner.shipment.services.entity.enums.DpsExecutionStatus;
import java.util.List;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

public interface IDpsEventRepository extends JpaRepository<DpsEvent, Long> {

    @Query(value = "SELECT * FROM dps_event WHERE entity_id = ?1 AND status = ?2", nativeQuery = true)
    List<DpsEvent> findDpsEventByGuidAndExecutionState(String guid, DpsExecutionStatus dpsExecutionStatus);

    DpsEvent findByExecutionId(UUID executionId);

    @Modifying @Transactional
    @Query(value = "Update dps_event set status = ?2, warnings_approver = ?1 Where execution_id in ?3", nativeQuery = true)
    void updateRuleStatus(String username, DpsExecutionStatus dpsExecutionStatus, List<String> executionIds);
}
