package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.entity.Notification;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.INotificationRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.Map;
import java.util.HashMap;

@Repository
@Slf4j
public class NotificationDao implements INotificationDao {

    private final INotificationRepository notificationRepository;

    private final ValidatorUtility validatorUtility;

    private final JsonHelper jsonHelper;

    @Autowired
    public NotificationDao(INotificationRepository notificationRepository, ValidatorUtility validatorUtility,
                           JsonHelper jsonHelper) {
        this.notificationRepository = notificationRepository;
        this.validatorUtility = validatorUtility;
        this.jsonHelper = jsonHelper;
    }


    @Override
    public Notification save(Notification notification) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(notification) , Constants.NOTIFICATION_ENTITY, LifecycleHooks.ON_CREATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        if (notification.getId() != null) {
            Optional<Notification> oldEntity = findById(notification.getId());
            if (oldEntity.isEmpty()) {
                log.debug("Notification is null for Id {}", notification.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
        }
        return notificationRepository.save(notification);
    }


    @Override
    public Page<Notification> findAll(Specification<Notification> spec, Pageable pageable) {
        return notificationRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Notification> findById(Long id) {
        return notificationRepository.findById(id);
    }

    @Override
    public Optional<Notification> findByGuid(UUID id) {
        return notificationRepository.findByGuid(id);
    }

    @Override
    public void delete(Notification notification) {
        notificationRepository.delete(notification);
    }

    @Override
    public List<Notification> findByEntityIdAndEntityType(Long entityId, String entityType) {
        return notificationRepository.findByEntityIdAndEntityType(entityId, entityType);
    }

    public List<Notification> saveAll(List<Notification> notificationEntityList) {
        for(var notificationEntity : notificationEntityList){
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(notificationEntity), Constants.NOTIFICATION_ENTITY, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(String.join(",", errors));
        }
        return notificationRepository.saveAll(notificationEntityList);
    }

    @Override
    public Map<Long, Integer> pendingNotificationCountBasedOnEntityIdsAndEntityType(List<Long> entityIds, String entityType) {
        List<Object[]> results = notificationRepository.pendingNotificationCountBasedOnEntityIdsAndEntityType(entityIds, entityType);
        return this.convertResponseToMap(results);
    }

    private Map<Long, Integer> convertResponseToMap(List<Object[]> results) {
        Map<Long, Integer> responseMap = new HashMap<>();

        for (Object[] result : results) {
            Long key = ((Number) result[0]).longValue();
            int count = ((Number) result[1]).intValue();
            responseMap.put(key, count);
        }

        return responseMap;
    }

    @Override
    public List<Notification> findNotificationForEntityTransfer(Long entityId, String entityType, Integer branchId, List<String> requestTypes) {
        return notificationRepository.findNotificationBasedOnEntityIdAndEntityTypeAndBranchIdAndRequestTypes(entityId, entityType, branchId, requestTypes);
    }

    @Override
    public List<Notification> findNotificationByEntityIdsForEntityTransfer(List<Long> entityIds, String entityType, Integer branchId, List<String> requestTypes) {
        return notificationRepository.findNotificationBasedOnEntityIdsAndEntityTypeAndBranchIdAndRequestTypes(entityIds, entityType, branchId, requestTypes);
    }

    @Override
    public void deleteAll(List<Notification> notificationList) {
        notificationRepository.deleteAll(notificationList);
    }

    @Override
    public List<Long> findEntityIdsByEntityType(String entityType) {
        return notificationRepository.findEntityIdsByEntityType(entityType);
    }

    public Integer findAllPendingNotificationCount(String entityType, Integer branchId) {
        return notificationRepository.findAllPendingNotificationCount(entityType, branchId);
    }
}
