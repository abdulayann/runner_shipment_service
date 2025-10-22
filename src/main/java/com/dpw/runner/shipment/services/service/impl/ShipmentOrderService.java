package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.dao.interfaces.IShipmentOrderDao;
import com.dpw.runner.shipment.services.entity.ShipmentOrder;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentOrderService;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ShipmentOrderService implements IShipmentOrderService {

    @Autowired
    private IShipmentOrderDao shipmentOrderDao;

    @Override
    public ShipmentOrder save(ShipmentOrder shipmentOrder) {
        return shipmentOrderDao.save(shipmentOrder);
    }

    @Override
    public Page<ShipmentOrder> findAll(Specification<ShipmentOrder> spec, Pageable pageable) {
        return shipmentOrderDao.findAll(spec, pageable);
    }

    @Override
    public Optional<ShipmentOrder> findById(Long id) {
        return shipmentOrderDao.findById(id);
    }

    @Override
    public void delete(ShipmentOrder shipmentOrder) {
        shipmentOrderDao.delete(shipmentOrder);
    }

    @Override
    public List<ShipmentOrder> findByShipmentId(Long shipmentId) {
        return shipmentOrderDao.findByShipmentId(shipmentId);
    }

    @Override
    public List<ShipmentOrder> findByOrderGuidIn(List<UUID> orderGuidList) {
        return shipmentOrderDao.findByOrderGuidIn(orderGuidList);
    }

    @Override
    public Optional<ShipmentOrder> findByShipmentIdAndOrderGuid(Long shipmentId, UUID orderGuid) {
        return shipmentOrderDao.findByShipmentIdAndOrderGuid(shipmentId, orderGuid);
    }

    @Override
    public List<ShipmentOrder> updateEntityFromShipment(List<ShipmentOrder> shipmentOrders, Long shipmentId) {
        return shipmentOrderDao.updateEntityFromShipment(shipmentOrders, shipmentId);
    }

    @Override
    public void deleteAdditionalShipmentOrderByShipmentId(List<Long> shipmentOrderIds, Long shipmentId) {
        shipmentOrderDao.deleteAdditionalShipmentOrderByShipmentId(shipmentOrderIds, shipmentId);
    }

    @Override
    public void revertSoftDeleteByshipmentOrderIdsAndShipmentId(List<Long> shipmentOrderIds, Long shipmentId) {
        shipmentOrderDao.revertSoftDeleteByshipmentOrderIdsAndShipmentId(shipmentOrderIds, shipmentId);
    }
}
