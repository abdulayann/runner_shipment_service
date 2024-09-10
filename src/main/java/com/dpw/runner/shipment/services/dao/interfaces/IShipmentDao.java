package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.projection.ShipmentDetailsProjection;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

public interface IShipmentDao {
    ShipmentDetails save(ShipmentDetails shipmentDetails, boolean fromV1Sync) throws RunnerException;
    ShipmentDetails update(ShipmentDetails shipmentDetails, boolean fromV1Sync);
    Page<ShipmentDetails> findAll(Specification<ShipmentDetails> spec, Pageable pageable);
    Optional<ShipmentDetails> findById(Long id);
    void delete(ShipmentDetails shipmentDetails);
    List<ShipmentDetails> saveAll(List<ShipmentDetails> shipments) throws RunnerException;
    Optional<ShipmentDetails> findByGuid(UUID id);
    List<ShipmentDetails> findByHouseBill(String hbl, Integer tenantId);
    List<ShipmentDetails> findByBookingReference(String Hbl);
    Long findMaxId();
    void saveJobStatus(Long id, String jobStatus);
    void saveCreatedDateAndUser(Long id, String createdBy, LocalDateTime createdDate);
    List<ShipmentDetails> getShipmentNumberFromId(List<Long> shipmentIds);
    void saveEntityTransfer(Long id, Boolean entityTransfer);
    List<ShipmentDetails> findShipmentsByGuids(Set<UUID> guids);
    List<ShipmentDetails> findShipmentsBySourceGuids(Set<UUID> sourceGuid);
    List<ShipmentDetails> findShipmentBySourceGuidAndTenantId(UUID sourceGuid, Integer tenantId);
    List<ShipmentDetails> findShipmentsByIds(Set<Long> ids);
    void entityDetach(List<ShipmentDetails> shipmentDetails);
    List<ShipmentDetails> findBySourceGuid(UUID guid);
    Page<Long> getIdWithPendingActions(ShipmentRequestedType shipmentRequestedType, Pageable pageable);

    List<ShipmentDetailsProjection> findHblNumberInAllBranches(String hblNumber);
}
