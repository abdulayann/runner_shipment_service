package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.SyncQueue;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;


@org.springframework.stereotype.Repository
@Generated
public interface IGenericQueryRepository extends JpaRepository<SyncQueue, Long> {

    @Modifying
    @Transactional
    @Query(value = "Update shipment_details set job_status = ?2 , file_status = ?3 Where guid = ?1", nativeQuery = true)
    void saveJobStatusByGuid(UUID guid, String jobStatus, String fileStatus);
}
