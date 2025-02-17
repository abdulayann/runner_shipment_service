package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.SyncQueue;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Generated
public interface ISyncQueueRepository extends MultiTenancyRepository<SyncQueue> {

    @Query(value = "SELECT * FROM sync_queue sd WHERE sd.sync_tenant_id IN ?1 AND sd.module_type = ?2 AND sd.is_deleted = false ORDER BY ID ASC", nativeQuery = true)
    List<SyncQueue> fetchByTenantIdsAndModuleType(List<Integer> tenantId, String moduleType);

    @Query(value = "SELECT * FROM sync_queue sd WHERE sd.sync_tenant_id IN ?1 AND sd.is_deleted = false ORDER BY ID ASC", nativeQuery = true)
    List<SyncQueue> fetchByTenantIds(List<Integer> tenantId);

    @Transactional
    @Modifying
    @Query(value = "UPDATE sync_queue SET is_deleted = true  WHERE module_type = ?1 AND module_id = ?2 AND sync_tenant_id = ?3 AND is_deleted = false", nativeQuery = true)
    void updateExistingDataInActive(String moduleType, String moduleId, Integer tenantId);

    @Transactional
    @Modifying
    @Query(value = "UPDATE sync_queue SET is_deleted = 'true' WHERE id IN ?1", nativeQuery = true)
    void updateDataInActiveByIds(List<Long> ids);
}
