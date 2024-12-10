package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.QuartzJobInfo;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface IQuartzJobInfoRepository extends JpaRepository<QuartzJobInfo, Long> {

    @Query(value = "SELECT * FROM quartz_job_info WHERE tenant_id = ?1 AND entity_id = ?2 AND entity_type = ?3", nativeQuery = true)
    Optional<QuartzJobInfo> findByJobFilters(Integer tenantId, Long entityId, String entityType);

    @Query(value = "SELECT * FROM quartz_job_info where id = ?1", nativeQuery = true)
    Optional<QuartzJobInfo> findByIdQuery(Long id);
}
