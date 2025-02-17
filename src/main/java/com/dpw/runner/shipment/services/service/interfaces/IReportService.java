package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.itextpdf.text.DocumentException;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.util.concurrent.ExecutionException;

public interface IReportService {
    byte[] getDocumentData(CommonRequestModel request)
            throws DocumentException, IOException, RunnerException, ExecutionException, InterruptedException;

    ResponseEntity<IRunnerResponse> createDocumentTagsForShipment(CommonRequestModel request) throws RunnerException;
}
