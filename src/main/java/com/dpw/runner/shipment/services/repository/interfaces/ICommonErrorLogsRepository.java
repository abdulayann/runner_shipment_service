package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.CommonErrorLogs;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
@Generated
public interface ICommonErrorLogsRepository extends MultiTenancyRepository<CommonErrorLogs> {

    default Optional<CommonErrorLogs> findByGuid(UUID id) {
        Specification<CommonErrorLogs> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }

    @Query(value = "SELECT * FROM common_error_logs WHERE entity_id = ?1 AND entity_type = ?2 AND error_type = ?3", nativeQuery = true)
    List<CommonErrorLogs> findByEntityIdAndEntityTypeAndErrorType(Long entityId, String entityType, String errorType);

    @Query(value = "SELECT * FROM common_error_logs WHERE entity_id IN ?1 AND entity_type = ?2 AND error_type = ?3", nativeQuery = true)
    List<CommonErrorLogs> findByEntityIdListAndEntityTypeAndErrorType(List<Long> entityId, String entityType, String errorType);

}
