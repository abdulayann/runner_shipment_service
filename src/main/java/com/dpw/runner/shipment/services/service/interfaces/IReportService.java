package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.EmailBodyResponse;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.DefaultEmailTemplateRequest;
import com.dpw.runner.shipment.services.dto.request.ReportRequest;
import com.dpw.runner.shipment.services.dto.response.HouseBillValidationResponse;
import com.dpw.runner.shipment.services.dto.response.ReportResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.itextpdf.text.DocumentException;
import java.io.IOException;
import java.util.concurrent.ExecutionException;
import org.springframework.http.ResponseEntity;

public interface IReportService {
    ReportResponse getDocumentData(CommonRequestModel request)
        throws DocumentException, IOException, RunnerException, ExecutionException, InterruptedException;
    ResponseEntity<IRunnerResponse> createDocumentTagsForShipment(CommonRequestModel request) throws RunnerException;
    EmailBodyResponse getPreAlertEmailTemplateData(Long shipmentId, Long emailTemplateId) throws RunnerException;
    EmailBodyResponse getDefaultEmailTemplateData(DefaultEmailTemplateRequest defaultEmailTemplateRequest) throws RunnerException;

    HouseBillValidationResponse validateHouseBill(ReportRequest request);
}
