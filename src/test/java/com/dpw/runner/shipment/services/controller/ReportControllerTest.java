package com.dpw.runner.shipment.services.controller;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.EmailBodyResponse;
import com.dpw.runner.shipment.services.dto.request.ReportRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.TranslationException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IReportService;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.itextpdf.text.DocumentException;
import java.io.IOException;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.transaction.UnexpectedRollbackException;

@ContextConfiguration(classes = {ReportController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ReportControllerTest {

    @Mock
    private IReportService reportService;
    @InjectMocks
    private ReportController reportController;

    @Test
    void createReport()
        throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        // Mock
        when(reportService.getDocumentData(any())).thenReturn(StringUtility.getRandomString(100).getBytes());
        // Test
        var responseEntity = reportController.createReport(new ReportRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createReport2()
        throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        // Mock
        when(reportService.getDocumentData(any())).thenThrow(new TranslationException("TranslationException"));
        // Test
        var responseEntity = reportController.createReport(new ReportRequest());
        // Assert
        assertEquals(HttpStatus.PRECONDITION_REQUIRED, responseEntity.getStatusCode());
    }

    @Test
    void createReport3()
        throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        // Mock
        when(reportService.getDocumentData(any())).thenThrow(new RunnerException("RunnerException"));
        // Test
        var responseEntity = reportController.createReport(new ReportRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createReport2_1()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        // Mock
        when(reportService.getDocumentData(any())).thenThrow(new UnexpectedRollbackException("UnexpectedRollbackException"));
        // Test
        var responseEntity = reportController.createReport(new ReportRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createReport4()
        throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        // Mock
        when(reportService.getDocumentData(any())).thenThrow(new RunnerException(""));
        // Test
        var responseEntity = reportController.createReport(new ReportRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createDocumentTagsForShipment() throws RunnerException {
        // Mock
        when(reportService.createDocumentTagsForShipment(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = reportController.createDocumentTagsForShipment(Optional.of(123L), Optional.of(UUID.randomUUID().toString()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createDocumentTagsForShipment2() throws RunnerException {
        // Mock
        when(reportService.createDocumentTagsForShipment(any())).thenThrow(new TranslationException("TranslationException"));
        // Test
        var responseEntity = reportController.createDocumentTagsForShipment(Optional.of(123L), Optional.of(UUID.randomUUID().toString()));
        // Assert
        assertEquals(HttpStatus.PRECONDITION_REQUIRED, responseEntity.getStatusCode());
    }

    @Test
    void createDocumentTagsForShipment3() throws RunnerException {
        // Mock
        when(reportService.createDocumentTagsForShipment(any())).thenThrow(new RunnerException("RunnerException"));
        // Test
        var responseEntity = reportController.createDocumentTagsForShipment(Optional.of(123L), Optional.of(UUID.randomUUID().toString()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createDocumentTagsForShipment4() throws RunnerException {
        // Mock
        when(reportService.createDocumentTagsForShipment(any())).thenThrow(new RunnerException(""));
        // Test
        var responseEntity = reportController.createDocumentTagsForShipment(Optional.of(123L), Optional.of(UUID.randomUUID().toString()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createAuditLog() throws Exception {
        when(reportService.getPreAlertEmailTemplateData(any(), any())).thenReturn(new EmailBodyResponse());
        var response = reportController.getPreAlertEmailTemplateData(1L, 2L);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void createAuditLog1() throws Exception {
        when(reportService.getPreAlertEmailTemplateData(any(), any())).thenThrow(new RunnerException());
        var response = reportController.getPreAlertEmailTemplateData(1L, 2L);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }


}
