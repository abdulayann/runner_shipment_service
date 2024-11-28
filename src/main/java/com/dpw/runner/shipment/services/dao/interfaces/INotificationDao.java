package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Notification;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface INotificationDao {
    Notification save(Notification notification);

    Page<Notification> findAll(Specification<Notification> spec, Pageable pageable);

    Optional<Notification> findById(Long id);

    Optional<Notification> findByGuid(UUID id);

    List<Notification> findByEntityIdAndEntityType(Long entityId, String entityType);

    void delete(Notification notification);
}
