package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentOrder;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import javax.transaction.Transactional;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
@Generated
public interface IShipmentOrderRepository extends JpaRepository<ShipmentOrder, Long> {

    List<ShipmentOrder> findByShipmentId(Long shipmentId);

    Optional<ShipmentOrder> findByShipmentIdAndOrderGuid(Long shipmentId, UUID orderGuid);

    Page<ShipmentOrder> findAll(Specification<ShipmentOrder> spec, Pageable pageable);

    @Modifying
    @Transactional
    @Query(value = "DELETE FROM shipment_order so WHERE so.id NOT IN ?1 AND so.shipment_id = ?2", nativeQuery = true)
    void deleteByIdNotInAndShipmentId(List<Long> incomingIds, Long shipmentId);

    @Transactional
    void deleteAllByShipmentId(Long shipmentId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE shipment_order SET is_deleted = true WHERE id NOT IN (?1) and shipment_id = ?2", nativeQuery = true)
    void deleteAdditionalShipmentOrderByShipmentId(List<Long> shipmentOrderIds, Long shipmentId);

    @Modifying
    @Query(value = "UPDATE shipment_order SET is_deleted = false WHERE id IN (?1) and shipment_id = ?2", nativeQuery = true)
    void revertSoftDeleteByshipmentOrderIdsAndShipmentId(List<Long> shipmentOrderIds, Long shipmentId);
}
