package com.dpw.runner.shipment.services.service.impl;

import com.dpw.api.quartz.service.QuartzJobExecutorService;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.QuartzJobInfoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.impl.ConsolidationDao;
import com.dpw.runner.shipment.services.dao.impl.ShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICommonErrorLogsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IQuartzJobInfoDao;
import com.dpw.runner.shipment.services.document.config.DocumentManagerRestClient;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerEntityFileRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerMultipleEntityFileRequest;
import com.dpw.runner.shipment.services.document.response.DocumentManagerEntityFileResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.QuartzJobInfo;
import com.dpw.runner.shipment.services.entity.TriangulationPartner;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.JobState;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.SendConsolidationRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.SendShipmentRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.ValidateSendConsolidationRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.ValidateSendShipmentRequest;
import com.dpw.runner.shipment.services.entitytransfer.service.interfaces.IEntityTransferService;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.quartz.JobExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Component
public class ShipmentJobExecutorService implements QuartzJobExecutorService {

    private final IEntityTransferService entityTransferService;
    private final IQuartzJobInfoDao quartzJobInfoDao;
    private final IV1Service v1Service;
    private final ShipmentDao shipmentDao;
    private final ConsolidationDao consolidationDao;
    private final ICommonErrorLogsDao commonErrorLogsDao;
    private final DocumentManagerRestClient documentManagerRestClient;

    @Autowired
    ShipmentJobExecutorService (IEntityTransferService entityTransferService, IQuartzJobInfoDao quartzJobInfoDao, IV1Service v1Service, ShipmentDao shipmentDao, ConsolidationDao consolidationDao, ICommonErrorLogsDao commonErrorLogsDao, DocumentManagerRestClient documentManagerRestClient) {
        this.entityTransferService = entityTransferService;
        this.quartzJobInfoDao = quartzJobInfoDao;
        this.v1Service = v1Service;
        this.shipmentDao = shipmentDao;
        this.consolidationDao = consolidationDao;
        this.commonErrorLogsDao = commonErrorLogsDao;
        this.documentManagerRestClient = documentManagerRestClient;
    }
    @Transactional
    @Override
    public void executeJob(JobExecutionContext jobExecutionContext) {
        String jobId = jobExecutionContext.getJobDetail().getKey().getName();
        log.info("Executing Job {}", jobId);
        var quartzJobInfo = quartzJobInfoDao.findByIdQuery(Long.parseLong(jobId));
        if (quartzJobInfo.isPresent()) {
            try {
                var tenantId = quartzJobInfo.get().getTenantId();
                v1Service.setAuthContext();
                TenantContext.setCurrentTenant(tenantId);
                if (Objects.equals(quartzJobInfo.get().getEntityType(), Constants.SHIPMENT)) {
                    processSendShipment(quartzJobInfo.get());
                } else if (Objects.equals(quartzJobInfo.get().getEntityType(), Constants.CONSOLIDATION)) {
                    processSendConsolidation(quartzJobInfo.get());
                }
            } catch (Exception ex) {
                log.error("Job Execution failed: " + ex.getMessage(), ex);
                QuartzJobInfo quartzJob = quartzJobInfo.get();
                quartzJob.setJobStatus(JobState.ERROR);
                quartzJob.setErrorMessage(ex.getMessage());
                quartzJobInfoDao.save(quartzJob);

            }
        }

        if (TransactionSynchronizationManager.isSynchronizationActive()) {
            TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                @Override
                public void afterCompletion(int status) {
                    v1Service.clearAuthContext();
                    log.info("Job Finished: {}", jobId);
                }
            });
        } else {
            log.info("Transaction synchronization is not active. Clearing auth context manually.");
            v1Service.clearAuthContext();
            log.info("Job Finished: {}", jobId);
        }

    }


    public void processSendShipment(QuartzJobInfo quartzJobInfo) {
        var shipment = shipmentDao.findById(quartzJobInfo.getEntityId());
        if(shipment.isPresent()) {
            List<Long> sendToBranch = new ArrayList<>();
            if(shipment.get().getReceivingBranch() != null)
                sendToBranch.add(shipment.get().getReceivingBranch());
            if(!CommonUtils.listIsNullOrEmpty(shipment.get().getTriangulationPartnerList()))
                sendToBranch.addAll(shipment.get().getTriangulationPartnerList().stream().map(TriangulationPartner::getTriangulationPartner).toList());
            SendShipmentRequest sendShipmentRequest = SendShipmentRequest.builder()
                    .shipId(quartzJobInfo.getEntityId())
                    .sendToBranch(sendToBranch.stream().map(Long::intValue).toList())
                    .additionalDocs(fetchDocs(shipment.get().getGuid(), shipment.get().getTenantId(), Constants.Shipments))
                    .isAutomaticTransfer(Boolean.TRUE)
                    .build();
            ValidateSendShipmentRequest request = ValidateSendShipmentRequest.builder().shipId(quartzJobInfo.getEntityId()).build();
            try {
                var validationResponse = entityTransferService.automaticTransferShipmentValidation(CommonRequestModel.buildRequest(request));
                log.info("Completed Shipment Validation check.");
                if(!Boolean.TRUE.equals(validationResponse.getIsError())) {
                    var etResponse = entityTransferService.sendShipment(CommonRequestModel.buildRequest(sendShipmentRequest));
                    log.info("Completed Shipment transfer");
                    if(Objects.equals(etResponse.getStatusCode(), HttpStatus.OK)) {
                        quartzJobInfo.setJobStatus(JobState.COMPLETED);
                        quartzJobInfo.setErrorMessage("");
                        commonErrorLogsDao.deleteShipmentErrorsLogs(shipment.get().getId());
                    }
                } else {
                    quartzJobInfo.setJobStatus(JobState.ERROR);
                    quartzJobInfo.setErrorMessage(QuartzJobInfoConstants.AUTOMATIC_TRANSFER_FAILED + validationResponse.getShipmentErrorMessage());
                    commonErrorLogsDao.logShipmentAutomaticTransferErrors(validationResponse, shipment.get().getId());
                }
                quartzJobInfoDao.save(quartzJobInfo);
            } catch (ValidationException ex) {
                log.info(QuartzJobInfoConstants.VALIDATION_EXCEPTION_FOR_AUTOMATIC_TRANSFER + ex.getMessage());
                quartzJobInfo.setJobStatus(JobState.ERROR);
                quartzJobInfo.setErrorMessage(QuartzJobInfoConstants.AUTOMATIC_TRANSFER_FAILED + ex.getMessage());
                quartzJobInfoDao.save(quartzJobInfo);
            }
        }
    }

    private List<String> fetchDocs(UUID entityGuid, int tenantId, String entityType) {
        DocumentManagerMultipleEntityFileRequest multipleEntityFileRequest = new DocumentManagerMultipleEntityFileRequest();
        DocumentManagerEntityFileRequest documentManagerEntityFileRequest = DocumentManagerEntityFileRequest.builder()
                .entityKey(entityGuid.toString())
                .entityType(entityType)
                .tenantId((long) tenantId)
                .build();
        multipleEntityFileRequest.setEntities(List.of(documentManagerEntityFileRequest));
        var response = documentManagerRestClient.multipleEntityFilesWithTenant(multipleEntityFileRequest);
        if(!CommonUtils.listIsNullOrEmpty(response.getData())) {
            return response.getData().stream().filter(x-> Boolean.TRUE.equals(x.getIsTransferEnabled())).map(DocumentManagerEntityFileResponse::getGuid).toList();
        }
        return Collections.emptyList();
    }

    private void fetchConsoleAndShipmentDocs(ConsolidationDetails consolidationDetails, SendConsolidationRequest sendConsolidationRequest) {
        DocumentManagerMultipleEntityFileRequest multipleEntityFileRequest = new DocumentManagerMultipleEntityFileRequest();
        DocumentManagerEntityFileRequest documentManagerEntityFileRequest = DocumentManagerEntityFileRequest.builder()
                .entityKey(consolidationDetails.getGuid().toString())
                .entityType(Constants.Consolidations)
                .tenantId((long) consolidationDetails.getTenantId())
                .build();
        List<DocumentManagerEntityFileRequest> docListRequest = new ArrayList<>(Collections.singletonList(documentManagerEntityFileRequest));
        consolidationDetails.getShipmentsList().forEach(ship -> {
            DocumentManagerEntityFileRequest documentShipRequest = DocumentManagerEntityFileRequest.builder()
                    .entityKey(ship.getGuid().toString())
                    .entityType(Constants.Shipments)
                    .tenantId((long) ship.getTenantId())
                    .build();
            docListRequest.add(documentShipRequest);
        });
        multipleEntityFileRequest.setEntities(docListRequest);
        var response = documentManagerRestClient.multipleEntityFilesWithTenant(multipleEntityFileRequest);
        if(!CommonUtils.listIsNullOrEmpty(response.getData())) {
            sendConsolidationRequest.setAdditionalDocs(response.getData().stream()
                    .filter(x-> Boolean.TRUE.equals(x.getIsTransferEnabled()) && Objects.equals(x.getEntityType(), Constants.Consolidations)).map(DocumentManagerEntityFileResponse::getGuid).toList());
            Map<String, List<String>> shipDocs = response.getData().stream()
                    .filter(x-> Boolean.TRUE.equals(x.getIsTransferEnabled()) && Objects.equals(x.getEntityType(), Constants.Shipments))
                    .collect(Collectors.groupingBy(DocumentManagerEntityFileResponse::getEntityId, Collectors.mapping(DocumentManagerEntityFileResponse::getGuid, Collectors.toList())));
            sendConsolidationRequest.setShipAdditionalDocs(shipDocs);
        }
    }

    public void processSendConsolidation(QuartzJobInfo quartzJobInfo) {
        var consolidation = consolidationDao.findById(quartzJobInfo.getEntityId());
        if(consolidation.isPresent()) {
            List<Long> sendToBranch = new ArrayList<>();
            if(consolidation.get().getReceivingBranch() != null)
                sendToBranch.add(consolidation.get().getReceivingBranch());
            if(!CommonUtils.listIsNullOrEmpty(consolidation.get().getTriangulationPartnerList()))
                sendToBranch.addAll(consolidation.get().getTriangulationPartnerList().stream().map(TriangulationPartner::getTriangulationPartner).toList());

            SendConsolidationRequest sendConsolidationRequest = SendConsolidationRequest.builder()
                    .consolId(quartzJobInfo.getEntityId())
                    .sendToBranch(sendToBranch.stream().map(Long::intValue).toList())
                    .isAutomaticTransfer(Boolean.TRUE)
                    .build();
            fetchConsoleAndShipmentDocs(consolidation.get(), sendConsolidationRequest);
            ValidateSendConsolidationRequest request = ValidateSendConsolidationRequest.builder().consoleId(quartzJobInfo.getEntityId()).build();
            try {
                List<Long> shipmentIds = consolidation.get().getShipmentsList().stream().map(BaseEntity::getId).toList();
                var response = entityTransferService.automaticTransferConsoleValidation(CommonRequestModel.buildRequest(request));
                log.info("Completed Console Validation check.");
                if(!Boolean.TRUE.equals(response.getIsError())) {
                    var etResponse = entityTransferService.sendConsolidation(CommonRequestModel.buildRequest(sendConsolidationRequest));
                    log.info("Completed Console transfer");
                    if(Objects.equals(etResponse.getStatusCode(), HttpStatus.OK)) {
                        quartzJobInfo.setJobStatus(JobState.COMPLETED);
                        quartzJobInfo.setErrorMessage("");
                        commonErrorLogsDao.deleteAllConsoleAndShipmentErrorsLogs(consolidation.get().getId(), shipmentIds);
                    }
                }
                else{
                    quartzJobInfo.setJobStatus(JobState.ERROR);
                    quartzJobInfo.setErrorMessage(QuartzJobInfoConstants.AUTOMATIC_TRANSFER_FAILED + response.getConsoleErrorMessage());
                    commonErrorLogsDao.logConsoleAutomaticTransferErrors(response, consolidation.get().getId(), shipmentIds);
                }
                quartzJobInfoDao.save(quartzJobInfo);
            } catch (ValidationException ex) {
                log.info(QuartzJobInfoConstants.VALIDATION_EXCEPTION_FOR_AUTOMATIC_TRANSFER + ex.getMessage());
                quartzJobInfo.setJobStatus(JobState.ERROR);
                quartzJobInfo.setErrorMessage(QuartzJobInfoConstants.AUTOMATIC_TRANSFER_FAILED + ex.getMessage());
                quartzJobInfoDao.save(quartzJobInfo);
            }
        }
    }
}
