
package com.dpw.runner.shipment.services.service;

import com.dpw.runner.shipment.services.entity.BlDetails;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.MeasurementDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.repository.*;
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
    private IBlDetailsDao blDetailsDao;

    @Mock
    private IMeasurementDao measurementDao;

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
        when(blDetailsDao.save(any(BlDetails.class))).thenReturn(expectedResponse.get(0).getBlDetails());
        when(measurementDao.save(any(MeasurementDetails.class))).thenReturn(expectedResponse.get(0).getMeasurementDetails());
        when(carrierDao.save(any(CarrierDetails.class))).thenReturn(expectedResponse.get(0).getCarrierDetails());
        when(partiesDao.saveAll(anyList())).thenReturn(expectedResponse.get(0).getParties());

        List<ShipmentDetails> actualResponse = shipmentService.createTestShipment(count);

        assertEquals(count, actualResponse.size());
        for (int i = 0; i < count; i++) {
            ShipmentDetails expectedShipment = expectedResponse.get(i);
            ShipmentDetails actualShipment = actualResponse.get(i);
            assertEquals(expectedShipment.getBlDetails(), actualShipment.getBlDetails());
            assertEquals(expectedShipment.getMeasurementDetails(), actualShipment.getMeasurementDetails());
            assertEquals(expectedShipment.getCarrierDetails(), actualShipment.getCarrierDetails());
            assertEquals(expectedShipment.getParties(), actualShipment.getParties());
        }

        verify(shipmentDao, times(count)).save(any(ShipmentDetails.class));
        verify(blDetailsDao, times(count)).save(any(BlDetails.class));
        verify(measurementDao, times(count)).save(any(MeasurementDetails.class));
        verify(carrierDao, times(count)).save(any(CarrierDetails.class));
        verify(partiesDao, times(count)).saveAll(anyList());
    }

    private List<ShipmentDetails> createDummyShipmentDetails(int count) {
        return Arrays.asList(new ShipmentDetails());
    }

}
