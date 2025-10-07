package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.INetworkShipmentsMappingDao;
import com.dpw.runner.shipment.services.entity.NetworkShipmentsMapping;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.repository.interfaces.INetworkShipmentsMappingRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@Slf4j
public class NetworkShipmentsMappingDaoImpl implements INetworkShipmentsMappingDao {
    private final INetworkShipmentsMappingRepository networkShipmentMappingRepository;

    @Autowired
    public NetworkShipmentsMappingDaoImpl(INetworkShipmentsMappingRepository networkShipmentMappingRepository) {
        this.networkShipmentMappingRepository = networkShipmentMappingRepository;
    }

    @Override
    public Optional<NetworkShipmentsMapping> findByNetworkTransferAndShipmentNumber(NetworkTransfer networkTransfer, String shipmentNumber) {
        return networkShipmentMappingRepository.findByNetworkTransferAndShipmentNumber(networkTransfer, shipmentNumber);
    }

    @Override
    public NetworkShipmentsMapping save(NetworkShipmentsMapping networkShipmentsMapping) {
        return networkShipmentMappingRepository.save(networkShipmentsMapping);
    }

    @Override
    public List<NetworkShipmentsMapping> saveAll(List<NetworkShipmentsMapping> networkShipmentsMappingList) {
        return networkShipmentMappingRepository.saveAll(networkShipmentsMappingList);
    }

    @Override
    public void deleteByNetworkTransfer(NetworkTransfer networkTransfer) {
        networkShipmentMappingRepository.deleteByNetworkTransfer(networkTransfer);
    }
}
