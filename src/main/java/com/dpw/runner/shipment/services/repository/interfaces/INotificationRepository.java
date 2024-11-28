package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Notification;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
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
}
