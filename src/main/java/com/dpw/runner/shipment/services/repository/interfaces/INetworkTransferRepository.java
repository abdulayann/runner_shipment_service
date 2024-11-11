package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.utils.ExcludeTenantFilter;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

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
    @Query(value = "SELECT * FROM network_transfer WHERE tenant_id = ?1 AND entity_id = ?2 AND entity_type = ?3", nativeQuery = true)
    Optional<NetworkTransfer> findByTenantAndEntity(Integer tenantId, Long entityId, String entityType);
}
