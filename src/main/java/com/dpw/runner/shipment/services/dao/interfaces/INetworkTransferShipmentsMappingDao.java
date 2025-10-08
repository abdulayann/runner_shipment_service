package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.NetworkTransferShipmentsMapping;

import java.util.List;

public interface INetworkTransferShipmentsMappingDao {
    NetworkTransferShipmentsMapping save(NetworkTransferShipmentsMapping networkShipmentsMapping);
    List<NetworkTransferShipmentsMapping> saveAll(List<NetworkTransferShipmentsMapping> networkShipmentsMappingList);

    void deleteByNetworkTransferId(Long networkTransferId);
    List<String> findShipmentNumbersByNetworkTransferId(Long networkTransferId);
    void deleteByNetworkTransferIdAndShipmentNumbers(Long networkTransferId, List<String> shipmentNumbers);
}
