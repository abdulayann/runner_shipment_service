package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.NetworkShipmentsMapping;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;

import java.util.List;
import java.util.Optional;

public interface INetworkShipmentsMappingDao {
    Optional<NetworkShipmentsMapping> findByNetworkTransferAndShipmentNumber(NetworkTransfer networkTransfer, String shipmentNumber);

    NetworkShipmentsMapping save(NetworkShipmentsMapping networkShipmentsMapping);
    List<NetworkShipmentsMapping> saveAll(List<NetworkShipmentsMapping> networkShipmentsMappingList);

    void deleteByNetworkTransfer(NetworkTransfer networkTransfer);
}
