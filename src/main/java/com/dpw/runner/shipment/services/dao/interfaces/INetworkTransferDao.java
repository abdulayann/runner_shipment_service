package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface INetworkTransferDao {
    NetworkTransfer save(NetworkTransfer networkTransfer);
    List<NetworkTransfer> saveAll(List<NetworkTransfer> networkTransferEntityList);

    Page<NetworkTransfer> findAll(Specification<NetworkTransfer> spec, Pageable pageable);

    Optional<NetworkTransfer> findById(Long id);

    Optional<NetworkTransfer> findByGuid(UUID id);

    Optional<NetworkTransfer> findByTenantAndEntity(Integer tenantId, Long entityId, String entityType);

    Optional<NetworkTransfer> findByTenantAndEntityAndJobType(Integer tenantId, Long entityId, String entityType, String jobType);

    List<NetworkTransfer> findByEntityAndTenantList(Long entityId, String entityType, List<Integer> tenantIds);

    List<NetworkTransfer> findByEntityNTList(Long entityId, String entityType);

    List<NetworkTransfer> getInterConsoleNTList(List<Long> entityId, String entityType);

    void delete(NetworkTransfer networkTransfer);

    void deleteAndLog(NetworkTransfer networkTransferEntity, String entityType);

    void deleteByIdsAndLog(List<Long> networkTransferEntityIds);

    void updateStatusAndCreatedEntityId(Long id, String status, Long createdEntityId);

    void updateStatus(Long id, String status);

    List<NetworkTransfer> findByEntityIdAndEntityTypeAndIsInterBranchEntity(List<Long> entityIds, String entityType, Boolean isInterBranchEntity, List<String> status, String jobType);

    String findStatusByEntityIdAndEntityTypeAndTenantId(Long entityId, String entityType, Integer tenantId);
    String findByEntityGuidAndTenantId(UUID guid, Integer tenantId);
    List<NetworkTransfer> findByEntityGuids(List<UUID> guid);
}
