package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferShipmentsMappingDao;
import com.dpw.runner.shipment.services.entity.NetworkTransferShipmentsMapping;
import com.dpw.runner.shipment.services.repository.interfaces.INetworkTransferShipmentsMappingRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@Slf4j
public class NetworkTransferShipmentsMappingDao implements INetworkTransferShipmentsMappingDao {
    private final INetworkTransferShipmentsMappingRepository networkTransferShipmentMappingRepository;

    @Autowired
    public NetworkTransferShipmentsMappingDao(INetworkTransferShipmentsMappingRepository networkTransferShipmentMappingRepository) {
        this.networkTransferShipmentMappingRepository = networkTransferShipmentMappingRepository;
    }

    @Override
    public NetworkTransferShipmentsMapping save(NetworkTransferShipmentsMapping networkShipmentsMapping) {
        return networkTransferShipmentMappingRepository.save(networkShipmentsMapping);
    }

    @Override
    public List<NetworkTransferShipmentsMapping> saveAll(List<NetworkTransferShipmentsMapping> networkShipmentsMappingList) {
        return networkTransferShipmentMappingRepository.saveAll(networkShipmentsMappingList);
    }

    @Override
    public void deleteByNetworkTransferId(Long networkTransferId) {
        networkTransferShipmentMappingRepository.deleteByNetworkTransferId(networkTransferId);
    }

    @Override
    public List<String> findShipmentNumbersByNetworkTransferId(Long networkTransferId) {
        return networkTransferShipmentMappingRepository.findShipmentNumbersByNetworkTransferId(networkTransferId);
    }

    @Override
    public void deleteByNetworkTransferIdAndShipmentNumbers(Long networkTransferId, List<String> shipmentNumbers) {
        networkTransferShipmentMappingRepository.deleteByNetworkTransferIdAndShipmentNumbers(networkTransferId, shipmentNumbers);
    }
}
