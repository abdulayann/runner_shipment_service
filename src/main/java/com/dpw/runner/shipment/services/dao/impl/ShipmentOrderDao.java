package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IShipmentOrderDao;
import com.dpw.runner.shipment.services.entity.ShipmentOrder;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentOrderRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

@Repository
public class ShipmentOrderDao implements IShipmentOrderDao {

    @Autowired
    private IShipmentOrderRepository shipmentOrderRepository;
    @Override
    public ShipmentOrder save(ShipmentOrder shipmentOrder) {
        return shipmentOrderRepository.save(shipmentOrder);
    }

    @Override
    public Page<ShipmentOrder> findAll(Specification<ShipmentOrder> spec, Pageable pageable) {
        return shipmentOrderRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ShipmentOrder> findById(Long id) {
        return shipmentOrderRepository.findById(id);
    }

    @Override
    public void delete(ShipmentOrder shipmentOrder) {
        shipmentOrderRepository.delete(shipmentOrder);
    }

    @Override
    public List<ShipmentOrder> findByShipmentId(Long shipmentId) {
        return shipmentOrderRepository.findByShipmentId(shipmentId);
    }

    @Override
    public Optional<ShipmentOrder> findByShipmentIdAndOrderGuid(Long shipmentId, UUID orderGuid) {
        return shipmentOrderRepository.findByShipmentIdAndOrderGuid(shipmentId, orderGuid);
    }

    @Override
    public List<ShipmentOrder> updateEntityFromShipment(List<ShipmentOrder> shipmentOrders, Long shipmentId) {
        List<Long> incomingIds = shipmentOrders.stream()
                .map(ShipmentOrder::getId)
                .filter(Objects::nonNull)
                .toList();

        if (!incomingIds.isEmpty()) {
            shipmentOrderRepository.deleteByIdNotInAndShipmentId(incomingIds, shipmentId);
        }
        else {
            shipmentOrderRepository.deleteAllByShipmentId(shipmentId);
        }

        for(ShipmentOrder shipmentOrder : shipmentOrders){
            shipmentOrder.setShipmentId(shipmentId);
        }

        return shipmentOrderRepository.saveAll(shipmentOrders);
    }

    @Override
    public void deleteAdditionalShipmentOrderByShipmentId(List<Long> shipmentOrderIds, Long shipmentId) {
        shipmentOrderRepository.deleteAdditionalShipmentOrderByShipmentId(shipmentOrderIds, shipmentId);
    }

    @Override
    public void revertSoftDeleteByshipmentOrderIdsAndShipmentId(List<Long> shipmentOrderIds, Long shipmentId) {
        shipmentOrderRepository.revertSoftDeleteByshipmentOrderIdsAndShipmentId(shipmentOrderIds, shipmentId);
    }
}
