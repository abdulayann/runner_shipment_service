
package com.dpw.runner.shipment.services.service;

import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.repository.interfaces.*;
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
    private IShipmentRepository shipmentRepository;

    @Mock
    private ICarrierRepository carrierRepository;

    @Mock
    private IPartiesRepository partiesRepository;

    @InjectMocks
    private ShipmentService shipmentService;

    @Test
    public void testCreateTestShipment() {
        int count = 1;
        List<ShipmentDetails> expectedResponse = createDummyShipmentDetails(count);
        when(shipmentRepository.save(any(ShipmentDetails.class))).thenReturn(expectedResponse.get(0));
        when(carrierRepository.save(any(CarrierDetails.class))).thenReturn(expectedResponse.get(0).getCarrierDetails());
        when(partiesRepository.saveAll(anyList())).thenReturn(expectedResponse.get(0).getParties());

        List<ShipmentDetails> actualResponse = shipmentService.createTestShipment(count);

        assertEquals(count, actualResponse.size());
        for (int i = 0; i < count; i++) {
            ShipmentDetails expectedShipment = expectedResponse.get(i);
            ShipmentDetails actualShipment = actualResponse.get(i);
            assertEquals(expectedShipment.getCarrierDetails(), actualShipment.getCarrierDetails());
            assertEquals(expectedShipment.getParties(), actualShipment.getParties());
        }

        verify(shipmentRepository, times(count)).save(any(ShipmentDetails.class));
        verify(carrierRepository, times(count)).save(any(CarrierDetails.class));
        verify(partiesRepository, times(count)).saveAll(anyList());
    }

    private List<ShipmentDetails> createDummyShipmentDetails(int count) {
        return Arrays.asList(new ShipmentDetails());
    }

}
