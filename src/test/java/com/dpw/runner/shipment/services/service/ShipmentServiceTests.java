
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
    }

    private List<ShipmentDetails> createDummyShipmentDetails(int count) {
        return Arrays.asList(new ShipmentDetails());
    }

}
