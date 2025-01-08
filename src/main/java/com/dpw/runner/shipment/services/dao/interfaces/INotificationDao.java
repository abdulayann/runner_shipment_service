package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Notification;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

public interface INotificationDao {
    Notification save(Notification notification);

    Page<Notification> findAll(Specification<Notification> spec, Pageable pageable);

    Optional<Notification> findById(Long id);

    Optional<Notification> findByGuid(UUID id);

    List<Notification> findByEntityIdAndEntityType(Long entityId, String entityType);

    void delete(Notification notification);

    Map<Long, Integer> pendingNotificationCountBasedOnEntityIdsAndEntityType(List<Long> entityIds, String entityType);

    List<Notification> findNotificationForEntityTransfer(Long entityId, String entityType, Integer branchId, String requestType);

    void deleteAll(List<Notification> notificationList);

    List<Long> findEntityIdsByEntityType(String entityType);
}
