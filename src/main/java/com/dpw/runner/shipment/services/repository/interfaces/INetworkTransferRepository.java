package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.utils.ExcludeTenantFilter;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.UUID;



@Repository
@Generated
public interface INetworkTransferRepository extends MultiTenancyRepository<NetworkTransfer> {

    List<NetworkTransfer> findAll();

    Page<NetworkTransfer> findAll(Specification<NetworkTransfer> spec, Pageable pageable);

    default Optional<NetworkTransfer> findByGuid(UUID id) {
        Specification<NetworkTransfer> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }

    @ExcludeTenantFilter
    @Query(value = "SELECT * FROM network_transfer WHERE id = ?1", nativeQuery = true)
    Optional<NetworkTransfer> findByIdWithQuery(Long id);

    @ExcludeTenantFilter
    @Query(value = "SELECT * FROM network_transfer WHERE tenant_id = ?1 AND entity_id = ?2 AND entity_type = ?3", nativeQuery = true)
    Optional<NetworkTransfer> findByTenantAndEntity(Integer tenantId, Long entityId, String entityType);

    @ExcludeTenantFilter
    @Query(value = "SELECT * FROM network_transfer WHERE tenant_id = ?1 AND entity_id = ?2 AND entity_type = ?3 AND job_type= ?4", nativeQuery = true)
    Optional<NetworkTransfer> findByTenantAndEntityAndJobType(Integer tenantId, Long entityId, String entityType, String jobType);

    @ExcludeTenantFilter
    @Query(value = "SELECT * FROM network_transfer WHERE entity_id = ?1 AND entity_type = ?2 AND tenant_id in ?3", nativeQuery = true)
    List<NetworkTransfer> findByEntityAndTenantList(Long entityId, String entityType, List<Integer> tenantIds);

    @ExcludeTenantFilter
    @Query(value = "SELECT * FROM network_transfer WHERE entity_id = ?1 AND entity_type = ?2", nativeQuery = true)
    List<NetworkTransfer> findByEntityNTList(Long entityId, String entityType);

    @ExcludeTenantFilter
    @Query(value = "SELECT * FROM network_transfer WHERE entity_id in ?1 AND entity_type = ?2 AND is_inter_branch_entity= true", nativeQuery = true)
    List<NetworkTransfer> getInterConsoleNTList(List<Long> entityIdList, String entityType);

    @Modifying
    @Transactional
    @Query(value = "Update network_transfer set status = ?2, created_entity_id = ?3 Where id = ?1", nativeQuery = true)
    void updateStatusAndCreatedEntityId(Long id, String status, Long createdEntityId);

    @Modifying
    @Transactional
    @Query(value = "Update network_transfer set status = ?2 Where id = ?1", nativeQuery = true)
    void updateStatus(Long id, String status);

    @ExcludeTenantFilter
    @Query(value = "SELECT * FROM network_transfer WHERE entity_id IN (?1) AND entity_type = ?2 AND is_inter_branch_entity = ?3 AND status IN (?4) And job_type != ?5", nativeQuery = true)
    List<NetworkTransfer> findByEntityIdAndEntityTypeAndIsInterBranchEntity(List<Long> entityIds, String entityType, Boolean isInterBranchEntity, List<String> status, String jobType);

    @ExcludeTenantFilter
    @Query(value = "SELECT status FROM network_transfer WHERE entity_id = ?1 AND entity_type = ?2 AND tenant_id = ?3", nativeQuery = true)
    String findStatusByEntityIdAndEntityTypeAndTenantId(Long entityId, String entityType, Integer tenantId);

    @ExcludeTenantFilter
    @Query(value = "SELECT status FROM network_transfer WHERE entity_guid = ?1 AND tenant_id = ?2", nativeQuery = true)
    String findByEntityGuidAndTenantId(UUID guid, Integer tenantId);

    @ExcludeTenantFilter
    @Query(value = "SELECT * FROM network_transfer WHERE tenant_id = ?2 AND status IN ('TRANSFERRED', 'RETRANSFERRED') AND migration_status IN (?1)", nativeQuery = true)
    List<NetworkTransfer> findNteForMigrationStatuses(List<String> migrationStatuses, Integer tenantId);

    @ExcludeTenantFilter
    @Query(value = "SELECT * FROM network_transfer WHERE entity_guid in (?1)", nativeQuery = true)
    List<NetworkTransfer> findByEntityGuids(List<UUID> guid);
}
