package com.dpw.runner.shipment.services.reportingservice.Reports;

import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertNull;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentTagsForExteranlServicesTest {

    @InjectMocks
    private ShipmentTagsForExteranlServices shipmentTagsForExteranlServices;

    @Test
    void populateDictionary() {
        assertNull(shipmentTagsForExteranlServices.populateDictionary(null));
    }

    @Test
    void getDocumentModel() throws RunnerException {
        assertNull(shipmentTagsForExteranlServices.getDocumentModel(null));
    }

    @Test
    void getData() throws RunnerException {
        assertNull(shipmentTagsForExteranlServices.getData(null));
    }
}
