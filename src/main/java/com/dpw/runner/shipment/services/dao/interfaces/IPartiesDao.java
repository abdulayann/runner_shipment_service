package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Map;
import java.util.Optional;

public interface IPartiesDao {
    List<Parties> saveAll(List<Parties> parties);
    Parties save(Parties parties);
    Page<Parties> findAll(Specification<Parties> spec, Pageable pageable);
    Optional<Parties> findById(Long id);
    void delete(Parties parties);
    List<Parties> findByEntityIdAndEntityType(Long entityId, String entityType);
    List<Parties> updateEntityFromOtherEntity(List<Parties> partiesList, Long entityId, String entityType) throws RunnerException;
    List<Parties> saveEntityFromOtherEntity(List<Parties> partiesRequests, Long entityId, String entityType);
    List<Parties> saveEntityFromOtherEntity(List<Parties> partiesRequests, Long entityId, String entityType, Map<Long, Parties> oldEntityMap);
    List<Parties> updateEntityFromOtherEntity(List<Parties> partiesList, Long entityId, String entityType, List<Parties> oldEntityList) throws RunnerException;
    List<Parties> findByIds(List<Long> id);
    void deleteAdditionalDataByPartiesIdsEntityIdAndEntityType(List<Long> consolidationAddressIds, Long consolidationId, String consolidationAddresses);
    void revertSoftDeleteByPartiesIds(List<Long> partiesIds);

    void deleteAdditionalPartiesInPickupDeliveryDetailsByShipmentId(List<Long> partiesIds, List<Long> pickupDeliveryDetailsIds, Long shipmentId, String pickupDelivery);
}
