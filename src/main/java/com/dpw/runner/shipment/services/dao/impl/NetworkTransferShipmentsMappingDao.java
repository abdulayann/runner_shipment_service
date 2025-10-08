package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferShipmentsMappingDao;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.NetworkTransferShipmentsMapping;
import com.dpw.runner.shipment.services.repository.interfaces.INetworkTransferShipmentsMappingRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@Slf4j
public class NetworkTransferShipmentsMappingDao implements INetworkTransferShipmentsMappingDao {
    private final INetworkTransferShipmentsMappingRepository networkShipmentMappingRepository;

    @Autowired
    public NetworkTransferShipmentsMappingDao(INetworkTransferShipmentsMappingRepository networkShipmentMappingRepository) {
        this.networkShipmentMappingRepository = networkShipmentMappingRepository;
    }

    @Override
    public NetworkTransferShipmentsMapping save(NetworkTransferShipmentsMapping networkShipmentsMapping) {
        return networkShipmentMappingRepository.save(networkShipmentsMapping);
    }

    @Override
    public List<NetworkTransferShipmentsMapping> saveAll(List<NetworkTransferShipmentsMapping> networkShipmentsMappingList) {
        return networkShipmentMappingRepository.saveAll(networkShipmentsMappingList);
    }

    @Override
    public void deleteByNetworkTransferId(Long networkTransferId) {
        networkShipmentMappingRepository.deleteByNetworkTransferId(networkTransferId);
    }

    @Override
    public List<String> findShipmentNumbersByNetworkTransferId(Long networkTransferId) {
        return networkShipmentMappingRepository.findShipmentNumbersByNetworkTransferId(networkTransferId);
    }

    @Override
    public void deleteByNetworkTransferIdAndShipmentNumbers(Long networkTransferId, List<String> shipmentNumbers) {
        networkShipmentMappingRepository.deleteByNetworkTransferIdAndShipmentNumbers(networkTransferId, shipmentNumbers);
    }
}
