package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentOrder;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

public interface IShipmentOrderService {

    ShipmentOrder save(ShipmentOrder shipmentOrder);

    Page<ShipmentOrder> findAll(Specification<ShipmentOrder> spec, Pageable pageable);

    Optional<ShipmentOrder> findById(Long id);

    void delete(ShipmentOrder shipmentOrder);

    List<ShipmentOrder> findByShipmentId(Long shipmentId);

    Optional<ShipmentOrder> findByShipmentIdAndOrderGuid(Long shipmentId, UUID orderGuid);

    List<ShipmentOrder> updateEntityFromShipment(List<ShipmentOrder> shipmentOrders, Long shipmentId);

    void deleteAdditionalShipmentOrderByShipmentId(List<Long> shipmentOrderIds, Long shipmentId);

    void revertSoftDeleteByshipmentOrderIdsAndShipmentId(List<Long> shipmentOrderIds, Long shipmentId);
}
