package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.ConsolidationDao;
import com.dpw.runner.shipment.services.dao.impl.ShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICommonErrorLogsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IQuartzJobInfoDao;
import com.dpw.runner.shipment.services.document.config.DocumentManagerRestClient;
import com.dpw.runner.shipment.services.document.response.DocumentManagerEntityFileResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerListResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.QuartzJobInfo;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.TriangulationPartner;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendConsoleValidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendConsolidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendShipmentResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendShipmentValidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.service.interfaces.IEntityTransferService;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.JobKey;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import java.io.IOException;
import java.util.*;

import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class ShipmentJobExecutorServiceTest {

    @InjectMocks
    private ShipmentJobExecutorService shipmentJobExecutorService;

    @Mock
    private IEntityTransferService entityTransferService;

    @Mock
    private IQuartzJobInfoDao quartzJobInfoDao;

    @Mock
    private IV1Service v1Service;

    @Mock
    private ShipmentDao shipmentDao;

    @Mock
    private ConsolidationDao consolidationDao;

    @Mock
    private ICommonErrorLogsDao commonErrorLogsDao;

    @Mock
    private DocumentManagerRestClient documentManagerRestClient;

    @Mock
    private JobExecutionContext jobExecutionContext;

    private static JsonTestUtility jsonTestUtility;

    private ShipmentDetails shipmentDetails;

    private ConsolidationDetails consolidationDetails;

    private QuartzJobInfo quartzJobInfo;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
    }

    @BeforeEach
    void setUp() {
        TriangulationPartner triangulationPartner = TriangulationPartner.builder().triangulationPartner(66L).build();
        shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setTenantId(1);
        shipmentDetails.setTriangulationPartnerList(List.of(triangulationPartner));
        consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTenantId(1);
        consolidationDetails.setTriangulationPartnerList(List.of(triangulationPartner));
        quartzJobInfo = new QuartzJobInfo();
        quartzJobInfo.setId(1L);
        quartzJobInfo.setGuid(UUID.randomUUID());
        quartzJobInfo.setEntityId(100L);
        quartzJobInfo.setEntityType(Constants.SHIPMENT);
    }

    @Test
    void testExecuteJob_EmptyQuartzJobInfo() {
        String jobId = "1";
        JobDetail jobDetail = mock(JobDetail.class);
        SendShipmentValidationResponse sendShipmentValidationResponse = new SendShipmentValidationResponse();
        sendShipmentValidationResponse.setIsError(false);

        when(jobDetail.getKey()).thenReturn(JobKey.jobKey(jobId));
        when(jobExecutionContext.getJobDetail()).thenReturn(jobDetail);

        when(quartzJobInfoDao.findByIdQuery(Long.parseLong(jobId))).thenReturn(Optional.empty());

        doNothing().when(v1Service).clearAuthContext();

        shipmentJobExecutorService.executeJob(jobExecutionContext);

        verify(v1Service).clearAuthContext();
        verify(quartzJobInfoDao, times(1)).findByIdQuery(anyLong());
    }

    @Test
    void testExecuteJob_QuartzJobInfo_EmptyShipment() {
        String jobId = "1";
        JobDetail jobDetail = mock(JobDetail.class);
        SendShipmentValidationResponse sendShipmentValidationResponse = new SendShipmentValidationResponse();
        sendShipmentValidationResponse.setIsError(false);

        when(jobDetail.getKey()).thenReturn(JobKey.jobKey(jobId));
        when(jobExecutionContext.getJobDetail()).thenReturn(jobDetail);

        when(quartzJobInfoDao.findByIdQuery(Long.parseLong(jobId))).thenReturn(Optional.of(quartzJobInfo));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.empty());

        doNothing().when(v1Service).setAuthContext();
        doNothing().when(v1Service).clearAuthContext();

        shipmentJobExecutorService.executeJob(jobExecutionContext);

        verify(v1Service).setAuthContext();
        verify(v1Service).clearAuthContext();
        verify(quartzJobInfoDao, times(1)).findByIdQuery(anyLong());
    }

    @Test
    void testExecuteJob_WhenQuartzJobInfoExistsAndIsShipment() {
        String jobId = "1";
        JobDetail jobDetail = mock(JobDetail.class);
        SendShipmentValidationResponse sendShipmentValidationResponse = new SendShipmentValidationResponse();
        sendShipmentValidationResponse.setIsError(false);

        when(jobDetail.getKey()).thenReturn(JobKey.jobKey(jobId));
        when(jobExecutionContext.getJobDetail()).thenReturn(jobDetail);

        when(quartzJobInfoDao.findByIdQuery(Long.parseLong(jobId))).thenReturn(Optional.of(quartzJobInfo));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(documentManagerRestClient.multipleEntityFilesWithTenant(any())).thenReturn(getDocumentResponse());
        when(entityTransferService.automaticTransferShipmentValidation(any())).thenReturn(sendShipmentValidationResponse);
        when(entityTransferService.sendShipment(any())).thenReturn(getResponse(new SendShipmentResponse(), HttpStatus.OK));
        doNothing().when(commonErrorLogsDao).deleteShipmentErrorsLogs(anyLong());
        when(quartzJobInfoDao.save(any(QuartzJobInfo.class))).thenReturn(quartzJobInfo);

        doNothing().when(v1Service).setAuthContext();
        doNothing().when(v1Service).clearAuthContext();

        shipmentJobExecutorService.executeJob(jobExecutionContext);

        verify(v1Service).setAuthContext();
        verify(v1Service).clearAuthContext();
        verify(quartzJobInfoDao, times(1)).findByIdQuery(anyLong());
        verify(shipmentDao, times(1)).findById(anyLong());
        verify(documentManagerRestClient, times(1)).multipleEntityFilesWithTenant(any());
        verify(entityTransferService, times(1)).automaticTransferShipmentValidation(any());
        verify(entityTransferService, times(1)).sendShipment(any());
        verify(commonErrorLogsDao, times(1)).deleteShipmentErrorsLogs(anyLong());
        verify(quartzJobInfoDao, times(1)).save(any());
    }

    @Test
    void testExecuteJob_QuartzJobInfoExists_SendShipment_BadRequest() {
        String jobId = "1";
        JobDetail jobDetail = mock(JobDetail.class);
        SendShipmentValidationResponse sendShipmentValidationResponse = new SendShipmentValidationResponse();
        sendShipmentValidationResponse.setIsError(false);

        when(jobDetail.getKey()).thenReturn(JobKey.jobKey(jobId));
        when(jobExecutionContext.getJobDetail()).thenReturn(jobDetail);

        when(quartzJobInfoDao.findByIdQuery(Long.parseLong(jobId))).thenReturn(Optional.of(quartzJobInfo));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(documentManagerRestClient.multipleEntityFilesWithTenant(any())).thenReturn(new DocumentManagerListResponse<>());
        when(entityTransferService.automaticTransferShipmentValidation(any())).thenReturn(sendShipmentValidationResponse);
        when(entityTransferService.sendShipment(any())).thenReturn(getResponse(new SendShipmentResponse(), HttpStatus.BAD_REQUEST));
        when(quartzJobInfoDao.save(any(QuartzJobInfo.class))).thenReturn(quartzJobInfo);

        doNothing().when(v1Service).setAuthContext();
        doNothing().when(v1Service).clearAuthContext();

        shipmentJobExecutorService.executeJob(jobExecutionContext);

        verify(v1Service).setAuthContext();
        verify(v1Service).clearAuthContext();
        verify(quartzJobInfoDao, times(1)).findByIdQuery(anyLong());
        verify(shipmentDao, times(1)).findById(anyLong());
        verify(documentManagerRestClient, times(1)).multipleEntityFilesWithTenant(any());
        verify(entityTransferService, times(1)).automaticTransferShipmentValidation(any());
        verify(entityTransferService, times(1)).sendShipment(any());
        verify(quartzJobInfoDao, times(1)).save(any());
    }

    @Test
    void testExecuteJob_WhenQuartzJobInfoExistsAndIsShipment_ValidationError() {
        String jobId = "1";
        JobDetail jobDetail = mock(JobDetail.class);
        SendShipmentValidationResponse sendShipmentValidationResponse = new SendShipmentValidationResponse();
        sendShipmentValidationResponse.setIsError(true);

        when(jobDetail.getKey()).thenReturn(JobKey.jobKey(jobId));
        when(jobExecutionContext.getJobDetail()).thenReturn(jobDetail);

        when(quartzJobInfoDao.findByIdQuery(Long.parseLong(jobId))).thenReturn(Optional.of(quartzJobInfo));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(documentManagerRestClient.multipleEntityFilesWithTenant(any())).thenReturn(getDocumentResponse());
        when(entityTransferService.automaticTransferShipmentValidation(any())).thenReturn(sendShipmentValidationResponse);
        doNothing().when(commonErrorLogsDao).logShipmentAutomaticTransferErrors(any(), anyLong());
        when(quartzJobInfoDao.save(any(QuartzJobInfo.class))).thenReturn(quartzJobInfo);

        doNothing().when(v1Service).setAuthContext();
        doNothing().when(v1Service).clearAuthContext();

        shipmentJobExecutorService.executeJob(jobExecutionContext);

        verify(v1Service).setAuthContext();
        verify(v1Service).clearAuthContext();
        verify(quartzJobInfoDao, times(1)).findByIdQuery(anyLong());
        verify(shipmentDao, times(1)).findById(anyLong());
        verify(documentManagerRestClient, times(1)).multipleEntityFilesWithTenant(any());
        verify(entityTransferService, times(1)).automaticTransferShipmentValidation(any());
        verify(commonErrorLogsDao, times(1)).logShipmentAutomaticTransferErrors(any(), anyLong());
        verify(quartzJobInfoDao, times(1)).save(any());
    }

    @Test
    void testExecuteJob_WhenQuartzJobInfoExistsAndIsShipment_ValidationThrowsException() {
        String jobId = "1";
        shipmentDetails.setReceivingBranch(null);
        shipmentDetails.setTriangulationPartnerList(null);
        JobDetail jobDetail = mock(JobDetail.class);

        when(jobDetail.getKey()).thenReturn(JobKey.jobKey(jobId));
        when(jobExecutionContext.getJobDetail()).thenReturn(jobDetail);

        when(quartzJobInfoDao.findByIdQuery(Long.parseLong(jobId))).thenReturn(Optional.of(quartzJobInfo));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(documentManagerRestClient.multipleEntityFilesWithTenant(any())).thenReturn(getDocumentResponse());
        when(entityTransferService.automaticTransferShipmentValidation(any())).thenThrow(new ValidationException("Error"));
        when(quartzJobInfoDao.save(any(QuartzJobInfo.class))).thenReturn(quartzJobInfo);

        doNothing().when(v1Service).setAuthContext();
        doNothing().when(v1Service).clearAuthContext();

        shipmentJobExecutorService.executeJob(jobExecutionContext);

        verify(v1Service).setAuthContext();
        verify(v1Service).clearAuthContext();
        verify(quartzJobInfoDao, times(1)).findByIdQuery(anyLong());
        verify(shipmentDao, times(1)).findById(anyLong());
        verify(documentManagerRestClient, times(1)).multipleEntityFilesWithTenant(any());
        verify(entityTransferService, times(1)).automaticTransferShipmentValidation(any());
        verify(quartzJobInfoDao, times(1)).save(any());
    }

    @Test
    void testExecuteJob_QuartzJobInfo_EmptyConsolidation() {
        String jobId = "1";
        quartzJobInfo.setEntityType(Constants.CONSOLIDATION);
        JobDetail jobDetail = mock(JobDetail.class);
        SendShipmentValidationResponse sendShipmentValidationResponse = new SendShipmentValidationResponse();
        sendShipmentValidationResponse.setIsError(false);

        when(jobDetail.getKey()).thenReturn(JobKey.jobKey(jobId));
        when(jobExecutionContext.getJobDetail()).thenReturn(jobDetail);

        when(quartzJobInfoDao.findByIdQuery(Long.parseLong(jobId))).thenReturn(Optional.of(quartzJobInfo));
        when(consolidationDao.findById(anyLong())).thenReturn(Optional.empty());

        doNothing().when(v1Service).setAuthContext();
        doNothing().when(v1Service).clearAuthContext();

        shipmentJobExecutorService.executeJob(jobExecutionContext);

        verify(v1Service).setAuthContext();
        verify(v1Service).clearAuthContext();
        verify(quartzJobInfoDao, times(1)).findByIdQuery(anyLong());
    }

    @Test
    void testExecuteJob_WhenQuartzJobInfoExistsAndIsConsolidation() {
        String jobId = "1";
        quartzJobInfo.setEntityType(Constants.CONSOLIDATION);
        consolidationDetails.getShipmentsList().iterator().next().setTenantId(1);
        JobDetail jobDetail = mock(JobDetail.class);
        SendConsoleValidationResponse sendConsoleValidationResponse = new SendConsoleValidationResponse();
        sendConsoleValidationResponse.setIsError(false);

        when(jobDetail.getKey()).thenReturn(JobKey.jobKey(jobId));
        when(jobExecutionContext.getJobDetail()).thenReturn(jobDetail);

        when(quartzJobInfoDao.findByIdQuery(Long.parseLong(jobId))).thenReturn(Optional.of(quartzJobInfo));
        when(consolidationDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(entityTransferService.automaticTransferConsoleValidation(any())).thenReturn(sendConsoleValidationResponse);
        when(documentManagerRestClient.multipleEntityFilesWithTenant(any())).thenReturn(getDocumentResponse());
        when(entityTransferService.sendConsolidation(any())).thenReturn(getResponse(new SendConsolidationResponse(), HttpStatus.OK));
        doNothing().when(commonErrorLogsDao).deleteAllConsoleAndShipmentErrorsLogs(anyLong(), anyList());
        when(quartzJobInfoDao.save(any(QuartzJobInfo.class))).thenReturn(quartzJobInfo);

        doNothing().when(v1Service).setAuthContext();
        doNothing().when(v1Service).clearAuthContext();

        shipmentJobExecutorService.executeJob(jobExecutionContext);

        verify(v1Service).setAuthContext();
        verify(v1Service).clearAuthContext();
        verify(quartzJobInfoDao, times(1)).findByIdQuery(anyLong());
        verify(consolidationDao, times(1)).findById(anyLong());
        verify(entityTransferService, times(1)).automaticTransferConsoleValidation(any());
        verify(documentManagerRestClient, times(1)).multipleEntityFilesWithTenant(any());
        verify(entityTransferService, times(1)).sendConsolidation(any());
        verify(commonErrorLogsDao, times(1)).deleteAllConsoleAndShipmentErrorsLogs(anyLong(), anyList());
        verify(quartzJobInfoDao, times(1)).save(any());
    }

    @Test
    void testExecuteJob_QuartzJobInfoExists_SendConsolidation_BadRequest() {
        String jobId = "1";
        quartzJobInfo.setEntityType(Constants.CONSOLIDATION);
        consolidationDetails.getShipmentsList().iterator().next().setTenantId(1);
        JobDetail jobDetail = mock(JobDetail.class);
        SendConsoleValidationResponse sendConsoleValidationResponse = new SendConsoleValidationResponse();
        sendConsoleValidationResponse.setIsError(false);

        when(jobDetail.getKey()).thenReturn(JobKey.jobKey(jobId));
        when(jobExecutionContext.getJobDetail()).thenReturn(jobDetail);

        when(quartzJobInfoDao.findByIdQuery(Long.parseLong(jobId))).thenReturn(Optional.of(quartzJobInfo));
        when(consolidationDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(entityTransferService.automaticTransferConsoleValidation(any())).thenReturn(sendConsoleValidationResponse);
        when(documentManagerRestClient.multipleEntityFilesWithTenant(any())).thenReturn(new DocumentManagerListResponse<>());
        when(entityTransferService.sendConsolidation(any())).thenReturn(getResponse(new SendConsolidationResponse(), HttpStatus.BAD_REQUEST));
        when(quartzJobInfoDao.save(any(QuartzJobInfo.class))).thenReturn(quartzJobInfo);

        doNothing().when(v1Service).setAuthContext();
        doNothing().when(v1Service).clearAuthContext();

        shipmentJobExecutorService.executeJob(jobExecutionContext);

        verify(v1Service).setAuthContext();
        verify(v1Service).clearAuthContext();
        verify(quartzJobInfoDao, times(1)).findByIdQuery(anyLong());
        verify(consolidationDao, times(1)).findById(anyLong());
        verify(entityTransferService, times(1)).automaticTransferConsoleValidation(any());
        verify(documentManagerRestClient, times(1)).multipleEntityFilesWithTenant(any());
        verify(entityTransferService, times(1)).sendConsolidation(any());
        verify(quartzJobInfoDao, times(1)).save(any());
    }

    @Test
    void testExecuteJob_WhenQuartzJobInfoExistsAndIsConsolidation_ValidationError() {
        String jobId = "1";
        quartzJobInfo.setEntityType(Constants.CONSOLIDATION);
        consolidationDetails.getShipmentsList().iterator().next().setTenantId(1);
        JobDetail jobDetail = mock(JobDetail.class);
        SendConsoleValidationResponse sendConsoleValidationResponse = new SendConsoleValidationResponse();
        sendConsoleValidationResponse.setIsError(true);

        when(jobDetail.getKey()).thenReturn(JobKey.jobKey(jobId));
        when(jobExecutionContext.getJobDetail()).thenReturn(jobDetail);

        when(quartzJobInfoDao.findByIdQuery(Long.parseLong(jobId))).thenReturn(Optional.of(quartzJobInfo));
        when(consolidationDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(entityTransferService.automaticTransferConsoleValidation(any())).thenReturn(sendConsoleValidationResponse);
        when(documentManagerRestClient.multipleEntityFilesWithTenant(any())).thenReturn(getDocumentResponse());
        doNothing().when(commonErrorLogsDao).logConsoleAutomaticTransferErrors(any(), anyLong(), anyList());
        when(quartzJobInfoDao.save(any(QuartzJobInfo.class))).thenReturn(quartzJobInfo);

        doNothing().when(v1Service).setAuthContext();
        doNothing().when(v1Service).clearAuthContext();

        shipmentJobExecutorService.executeJob(jobExecutionContext);

        verify(v1Service).setAuthContext();
        verify(v1Service).clearAuthContext();
        verify(quartzJobInfoDao, times(1)).findByIdQuery(anyLong());
        verify(consolidationDao, times(1)).findById(anyLong());
        verify(entityTransferService, times(1)).automaticTransferConsoleValidation(any());
        verify(documentManagerRestClient, times(1)).multipleEntityFilesWithTenant(any());
        verify(commonErrorLogsDao, times(1)).logConsoleAutomaticTransferErrors(any(), anyLong(), anyList());
        verify(quartzJobInfoDao, times(1)).save(any());
    }

    @Test
    void testExecuteJob_WhenQuartzJobInfoExistsAndIsConsolidation_ValidationThrowsException() {
        String jobId = "1";
        quartzJobInfo.setEntityType(Constants.CONSOLIDATION);
        consolidationDetails.getShipmentsList().iterator().next().setTenantId(1);
        consolidationDetails.setReceivingBranch(null);
        consolidationDetails.setTriangulationPartnerList(null);
        JobDetail jobDetail = mock(JobDetail.class);

        when(jobDetail.getKey()).thenReturn(JobKey.jobKey(jobId));
        when(jobExecutionContext.getJobDetail()).thenReturn(jobDetail);

        when(quartzJobInfoDao.findByIdQuery(Long.parseLong(jobId))).thenReturn(Optional.of(quartzJobInfo));
        when(consolidationDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(documentManagerRestClient.multipleEntityFilesWithTenant(any())).thenReturn(getDocumentResponse());
        when(entityTransferService.automaticTransferConsoleValidation(any())).thenThrow(new ValidationException("Error"));
        when(quartzJobInfoDao.save(any(QuartzJobInfo.class))).thenReturn(quartzJobInfo);

        doNothing().when(v1Service).setAuthContext();
        doNothing().when(v1Service).clearAuthContext();

        shipmentJobExecutorService.executeJob(jobExecutionContext);

        verify(v1Service).setAuthContext();
        verify(v1Service).clearAuthContext();
        verify(quartzJobInfoDao, times(1)).findByIdQuery(anyLong());
        verify(consolidationDao, times(1)).findById(anyLong());
        verify(documentManagerRestClient, times(1)).multipleEntityFilesWithTenant(any());
        verify(entityTransferService, times(1)).automaticTransferConsoleValidation(any());
        verify(quartzJobInfoDao, times(1)).save(any());
    }

    @Test
    void testExecuteJob_Error() {
        String jobId = "1";
        JobDetail jobDetail = mock(JobDetail.class);

        when(jobDetail.getKey()).thenReturn(JobKey.jobKey(jobId));
        when(jobExecutionContext.getJobDetail()).thenReturn(jobDetail);
        when(quartzJobInfoDao.findByIdQuery(any())).thenReturn(Optional.of(quartzJobInfo));
        when(shipmentDao.findById(anyLong())).thenThrow(new RuntimeException("Error"));

        doNothing().when(v1Service).setAuthContext();
        doNothing().when(v1Service).clearAuthContext();

        shipmentJobExecutorService.executeJob(jobExecutionContext);

        verify(v1Service).setAuthContext();
        verify(v1Service).clearAuthContext();
        verify(quartzJobInfoDao, times(1)).findByIdQuery(anyLong());
        verify(shipmentDao, times(1)).findById(anyLong());
        verify(quartzJobInfoDao, times(1)).save(any());
    }


    public DocumentManagerListResponse<DocumentManagerEntityFileResponse> getDocumentResponse() {
        DocumentManagerListResponse<DocumentManagerEntityFileResponse> documentManagerListResponse = new DocumentManagerListResponse<>();
        DocumentManagerEntityFileResponse documentManagerEntityFileResponse1 = new DocumentManagerEntityFileResponse();
        documentManagerEntityFileResponse1.setIsTransferEnabled(true);
        documentManagerEntityFileResponse1.setGuid(String.valueOf(UUID.randomUUID()));
        documentManagerEntityFileResponse1.setEntityType(Constants.SHIPMENTS_WITH_SQ_BRACKETS);

        DocumentManagerEntityFileResponse documentManagerEntityFileResponse2 = documentManagerEntityFileResponse1;
        documentManagerEntityFileResponse2.setEntityType(Constants.CONSOLIDATIONS_WITH_SQ_BRACKETS);

        documentManagerListResponse.setData(List.of(documentManagerEntityFileResponse1, documentManagerEntityFileResponse2));
        return documentManagerListResponse;
    }

    public ResponseEntity<IRunnerResponse> getResponse(IRunnerResponse data, HttpStatus status) {
        return new ResponseEntity<>(RunnerResponse.builder().success(true).data(data).build(), status);
    }

}
