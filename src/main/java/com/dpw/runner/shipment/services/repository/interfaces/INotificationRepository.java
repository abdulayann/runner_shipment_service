package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Notification;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;


@Repository
@Generated
public interface INotificationRepository extends MultiTenancyRepository<Notification> {

    List<Notification> findAll();

    Page<Notification> findAll(Specification<Notification> spec, Pageable pageable);

    default Optional<Notification> findByGuid(UUID id) {
        Specification<Notification> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }

    List<Notification> findByEntityIdAndEntityType(Long entityId, String entityType);

    @Query(value = "SELECT entity_id, COUNT(*) FROM notification WHERE entity_id IN ?1 AND entity_type = ?2 group by entity_id", nativeQuery = true)
    List<Object[]> pendingNotificationCountBasedOnEntityIdsAndEntityType(List<Long> entityIds, String entityType);

    @Query(value = "SELECT * FROM notification " +
            "WHERE entity_id = ?1 AND entity_type = ?2 " +
            "AND (requested_branch_id = ?3 OR reassigned_to_branch_id = ?3) " +
            "AND request_type IN (?4)",
            nativeQuery = true)
    List<Notification> findNotificationBasedOnEntityIdAndEntityTypeAndBranchIdAndRequestTypes(Long entityId, String entityType, Integer branchId, List<String> requestTypes);

    @Query(value = "SELECT * FROM notification " +
            "WHERE entity_id IN (?1) AND entity_type = ?2 " +
            "AND (requested_branch_id = ?3 OR reassigned_to_branch_id = ?3) " +
            "AND request_type IN (?4)",
            nativeQuery = true)
    List<Notification> findNotificationBasedOnEntityIdsAndEntityTypeAndBranchIdAndRequestTypes(List<Long> entityIds, String entityType, Integer branchId, List<String> requestTypes);

    @Query("SELECT DISTINCT n.entityId FROM Notification n WHERE n.entityType = :entityType")
    List<Long> findEntityIdsByEntityType(@Param("entityType") String entityType);
}
