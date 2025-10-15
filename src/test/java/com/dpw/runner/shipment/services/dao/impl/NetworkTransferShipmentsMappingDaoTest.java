package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.NetworkTransferShipmentsMapping;
import com.dpw.runner.shipment.services.repository.interfaces.INetworkTransferShipmentsMappingRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class NetworkTransferShipmentsMappingDaoTest {

    @Mock
    private INetworkTransferShipmentsMappingRepository networkTransferShipmentsMappingRepository;

    @InjectMocks
    private NetworkTransferShipmentsMappingDao networkTransferShipmentsMappingDao;

    @Test
    void testSave() {
        NetworkTransferShipmentsMapping entity = new NetworkTransferShipmentsMapping();
        when(networkTransferShipmentsMappingRepository.save(any(NetworkTransferShipmentsMapping.class)))
                .thenReturn(entity);

        NetworkTransferShipmentsMapping saved = networkTransferShipmentsMappingDao.save(entity);

        assertNotNull(saved);
        verify(networkTransferShipmentsMappingRepository, times(1)).save(entity);
    }

    @Test
    void testSaveAll() {
        List<NetworkTransferShipmentsMapping> entities =
                Arrays.asList(new NetworkTransferShipmentsMapping(), new NetworkTransferShipmentsMapping());
        when(networkTransferShipmentsMappingRepository.saveAll(anyList())).thenReturn(entities);

        List<NetworkTransferShipmentsMapping> savedList = networkTransferShipmentsMappingDao.saveAll(entities);

        assertEquals(2, savedList.size());
        verify(networkTransferShipmentsMappingRepository, times(1)).saveAll(entities);
    }

    @Test
    void testDeleteByNetworkTransferId() {
        Long id = 1L;
        networkTransferShipmentsMappingDao.deleteByNetworkTransferId(id);

        verify(networkTransferShipmentsMappingRepository, times(1)).deleteByNetworkTransferId(id);
    }

    @Test
    void testFindShipmentNumbersByNetworkTransferId() {
        Long id = 1L;
        List<String> shipmentNumbers = Arrays.asList("S1", "S2");
        when(networkTransferShipmentsMappingRepository.findShipmentNumbersByNetworkTransferId(id))
                .thenReturn(shipmentNumbers);

        List<String> result = networkTransferShipmentsMappingDao.findShipmentNumbersByNetworkTransferId(id);

        assertNotNull(result);
        assertEquals(shipmentNumbers, result);
        verify(networkTransferShipmentsMappingRepository, times(1)).findShipmentNumbersByNetworkTransferId(id);
    }

    @Test
    void testDeleteByNetworkTransferIdAndShipmentNumbers() {
        Long id = 1L;
        List<String> shipmentNumbers = Arrays.asList("S1", "S2");

        networkTransferShipmentsMappingDao.deleteByNetworkTransferIdAndShipmentNumbers(id, shipmentNumbers);

        verify(networkTransferShipmentsMappingRepository, times(1))
                .deleteByNetworkTransferIdAndShipmentNumbers(id, shipmentNumbers);
    }
}
