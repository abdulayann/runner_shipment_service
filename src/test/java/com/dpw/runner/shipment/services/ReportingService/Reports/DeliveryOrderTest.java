package com.dpw.runner.shipment.services.ReportingService.Reports;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class DeliveryOrderTest {
    @InjectMocks
    private DeliveryOrder deliveryOrder;

    @Mock
    private HblReport hblReport;

    @Test
    void populateDictionary() {
        assertNotNull(deliveryOrder.populateDictionary(null));
    }

    @Test
    void getDocumentModel() {
        assertNull(deliveryOrder.getDocumentModel(null));
    }

    @Test
    void getData() {
        when(hblReport.getData(any())).thenReturn(new HashMap<>());
        assertNotNull(deliveryOrder.getData(null));
    }
}
