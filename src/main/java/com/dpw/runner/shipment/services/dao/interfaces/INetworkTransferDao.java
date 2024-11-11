package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;
import java.util.UUID;

public interface INetworkTransferDao {
    NetworkTransfer save(NetworkTransfer networkTransfer);

    Page<NetworkTransfer> findAll(Specification<NetworkTransfer> spec, Pageable pageable);

    Optional<NetworkTransfer> findById(Long id);

    Optional<NetworkTransfer> findByGuid(UUID id);

    Optional<NetworkTransfer> findByTenantAndEntity(Integer id, Long entityId, String entityType);

    void delete(NetworkTransfer networkTransfer);

    void deleteAndLog(NetworkTransfer networkTransferEntity, String entityType, Long entityId);
    void updateStatusAndCreatedEntityId(Long id, NetworkTransferStatus status, Long createdEntityId);
}
