
package com.dpw.runner.shipment.services.service;

import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.service.impl.ShipmentService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@SpringBootTest
public class ShipmentServiceTests {

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private ICarrierDao carrierDao;

    @Mock
    private IPartiesDao partiesDao;

    @InjectMocks
    private ShipmentService shipmentService;

    @Test
    public void testCreateTestShipment() {
        int count = 1;
        List<ShipmentDetails> expectedResponse = createDummyShipmentDetails(count);
        when(shipmentDao.save(any(ShipmentDetails.class))).thenReturn(expectedResponse.get(0));
        when(carrierDao.save(any(CarrierDetails.class))).thenReturn(expectedResponse.get(0).getCarrierDetails());

        List<ShipmentDetails> actualResponse = shipmentService.createTestShipment(count);

        assertEquals(count, actualResponse.size());
        for (int i = 0; i < count; i++) {
            ShipmentDetails expectedShipment = expectedResponse.get(i);
            ShipmentDetails actualShipment = actualResponse.get(i);
            assertEquals(expectedShipment.getCarrierDetails(), actualShipment.getCarrierDetails());
        }

        verify(shipmentDao, times(count)).save(any(ShipmentDetails.class));
        verify(carrierDao, times(count)).save(any(CarrierDetails.class));
        verify(partiesDao, times(count)).saveAll(anyList());
    }

    private List<ShipmentDetails> createDummyShipmentDetails(int count) {
        return Arrays.asList(new ShipmentDetails());
    }

}
