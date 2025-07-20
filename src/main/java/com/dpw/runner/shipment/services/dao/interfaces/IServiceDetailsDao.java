package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

public interface IServiceDetailsDao {
    ServiceDetails save(ServiceDetails serviceDetails);
    List<ServiceDetails> saveAll(List<ServiceDetails> serviceDetailsList);
    Page<ServiceDetails> findAll(Specification<ServiceDetails> spec, Pageable pageable);
    Optional<ServiceDetails> findById(Long id);
    void delete(ServiceDetails serviceDetails);
    List<ServiceDetails> updateEntityFromShipment(List<ServiceDetails> serviceDetailsList, Long shipmentId) throws RunnerException;
    List<ServiceDetails> saveEntityFromShipment(List<ServiceDetails> serviceDetailsRequests, Long shipmentId);
    List<ServiceDetails> saveEntityFromShipment(List<ServiceDetails> serviceDetailsRequests, Long shipmentId, Map<Long, ServiceDetails> oldEntityMap);
    List<ServiceDetails> updateEntityFromShipment(List<ServiceDetails> serviceDetailsList, Long shipmentId, List<ServiceDetails> oldEntityList) throws RunnerException;
    Optional<ServiceDetails> findByGuid(UUID id);
    List<ServiceDetails> findByIdIn(List<Long> ids);
    void deleteByIdIn(List<Long> deleteIds);
    Optional<ServiceDetails> findByIdWithQuery(Long id);
    Optional<ServiceDetails> findByGuidWithQuery(UUID guid);
    Page<ServiceDetails> findAllWithoutTenantFilter(Specification<ServiceDetails> spec, Pageable pageable);

    void deleteAdditionalServiceDetailsByShipmentId(List<Long> serviceDetailsIds, Long shipmentId);

    void revertSoftDeleteByServiceDetailsIdsAndShipmentId(List<Long> serviceDetailsIds, Long shipmentId);
}
