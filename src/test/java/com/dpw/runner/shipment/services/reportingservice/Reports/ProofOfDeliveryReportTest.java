package com.dpw.runner.shipment.services.reportingservice.Reports;

import com.dpw.runner.shipment.services.reportingservice.Models.HblModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ProofOfDeliveryModel;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ProofOfDeliveryReportTest {

    @InjectMocks
    private ProofOfDeliveryReport proofOfDeliveryReport;

    @Mock
    private HblReport hblReport;

    @Test
    void populateDictionary() {
        ProofOfDeliveryModel proofOfDeliveryModel = new ProofOfDeliveryModel();
        proofOfDeliveryModel.hblModel = new HblModel();
        assertNotNull(proofOfDeliveryReport.populateDictionary(proofOfDeliveryModel));
    }

    @Test
    void getDocumentModel() {
        assertNotNull(proofOfDeliveryReport.getDocumentModel(null));
    }

    @Test
    void getData() {
        ProofOfDeliveryModel proofOfDeliveryModel = new ProofOfDeliveryModel();
        HblModel model = new HblModel();
        proofOfDeliveryModel.hblModel = model;
        when(hblReport.getDocumentModel(any())).thenReturn(model);
        assertNotNull(proofOfDeliveryReport.getData(null));
    }
}
