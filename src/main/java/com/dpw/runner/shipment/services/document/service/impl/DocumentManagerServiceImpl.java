package com.dpw.runner.shipment.services.document.service.impl;


import com.dpw.runner.shipment.services.ReportingService.Models.DocUploadRequest;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.document.config.DocumentManagerRestClient;
import com.dpw.runner.shipment.services.document.exception.BadRequestException;
import com.dpw.runner.shipment.services.document.request.documentmanager.*;
import com.dpw.runner.shipment.services.document.response.*;
import com.dpw.runner.shipment.services.document.service.IDocumentManagerService;
import com.dpw.runner.shipment.services.document.util.FileUtils;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.exception.exceptions.DocumentClientException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.formula.functions.T;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;

@Service
@Slf4j
public class DocumentManagerServiceImpl implements IDocumentManagerService {

    public static final String FILE_UPLOAD_TO_TEMP_FAILED = "File upload to temporary failed";
    public static final String FILE_UPLOAD_FAILED = "Error while uploading file to document service: Error: %s File request: %s";
    @Autowired
    private DocumentManagerRestClient restClient;

    @Autowired
    private HttpServletRequest httpServletRequest;

    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public DocumentManagerResponse<DocumentManagerDataResponse> temporaryFileUpload(MultipartFile file, String filename) {

        if (ObjectUtils.isEmpty(filename)) {
            throw new BadRequestException("fileName cannot be null or empty");
        }

        try {
            String originalFileName = filename;
            String base64EncodedFile = FileUtils.convertMultipartFileToBase64(file);
            String encodedFile = "data:@file/" + FileUtils.getFileExtenation(originalFileName) + ";base64," + base64EncodedFile;

            DocumentManagerTempFileUploadRequest request = DocumentManagerTempFileUploadRequest.builder()
                    .fileName(originalFileName)
                    .encodedfile(encodedFile)
                    .build();

            log.info("temporary upload file: {}", originalFileName);

            return restClient.temporaryFileUpload(request);

        } catch (Exception e) {
            log.error("something went wrong in temporaryFileUpload {}", e.toString());
            throw new IllegalArgumentException(e.getMessage());
        }
    }


    @Override
    public DocumentManagerResponse<DocumentManagerDataResponse> saveFile(DocumentManagerSaveFileRequest saveFileRequest) {
        log.info("save file Request: {}", saveFileRequest);
        return restClient.saveFile(saveFileRequest);
    }

    @Override
    public DocumentManagerResponse<DocumentManagerDataResponse> getFileAndRules(DocumentManagerFileAndRulesRequest fileAndRulesRequest) {
        String token = httpServletRequest.getHeader(HttpHeaders.AUTHORIZATION);
        log.info("fileAndRulesRequest: {}", fileAndRulesRequest);
        return restClient.getFileAndRules(token, fileAndRulesRequest);
    }

    @Override
    public DocumentManagerResponse<DocumentManagerDataResponse> getFileById(Long id) {
        String token = httpServletRequest.getHeader(HttpHeaders.AUTHORIZATION);
        log.info("get file by id {}", id);
        return restClient.getFileById(token, id);
    }

    @Override
    public DocumentManagerResponse<DocumentManagerBulkDownloadResponse> getBulkDownloadLink(DocumentManagerBulkDownloadRequest request) {
        String token = httpServletRequest.getHeader(HttpHeaders.AUTHORIZATION);
        log.info("fileAndRulesRequest: {}", request);
        return restClient.getBulkDownloadLink(token, request);
    }

    @Override
    public DocumentManagerListResponse<DocumentManagerEntityFileResponse> fetchMultipleFilesWithTenant(DocumentManagerMultipleEntityFileRequest request) {
        log.info("multipleEntityFilesWithTenantRequest: {}", request);
        return restClient.multipleEntityFilesWithTenant(request);
    }

    @Override
    public DocumentManagerResponse<T> updateFileEntities(DocumentManagerUpdateFileEntitiesRequest request) {
        log.info("CR-ID {} || updateFileEntities: {}", LoggerHelper.getRequestIdFromMDC(), request);
        return restClient.updateFileEntities(request);
    }

    @Override
    public ResponseEntity<IRunnerResponse> deleteFile(CommonRequestModel request) {
        var response = restClient.deleteFile(request.getDependentData());
        return ResponseHelper.buildDependentServiceResponse(response.getData(), response.getPageNo(), response.getPageSize());
    }

    @Override
    public ResponseEntity<IRunnerResponse> getFileHistory(CommonRequestModel commonRequestModel) {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        var response = restClient.getFileHistory(request.getId());
        return ResponseHelper.buildDependentServiceResponse(response.getData(), response.getPageNo(), response.getPageSize());
    }

    @Override
    public ResponseEntity<DocumentDownloadResponse> downloadDocument(CommonRequestModel commonRequestModel) {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        return restClient.downloadDocument(request.getId());
    }

    @Override
    public ResponseEntity<IRunnerResponse> bulkSave(CommonRequestModel request) {
        var response = restClient.bulkSaveFiles(request.getDependentData());
        return ResponseHelper.buildDependentServiceResponse(response.getData(), response.getPageNo(), response.getPageSize());
    }

    @Override
    public ResponseEntity<IRunnerResponse> temporaryUpload(CommonRequestModel request) {
        var response = restClient.temporaryUpload(request.getDependentData());
        return ResponseHelper.buildDependentServiceResponse(response.getData(), response.getPageNo(), response.getPageSize());
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel request, Long page, Long size) {
        var response = restClient.list(request.getDependentData(), page, size);
        return ResponseHelper.buildDependentServiceResponse(response.getData(), response.getPageNo(), response.getCount());
    }

    @Override
    public ResponseEntity<IRunnerResponse> listDocTypes(CommonRequestModel request) {
        var response = restClient.listDocTypes(request.getDependentData());
        return ResponseHelper.buildDependentServiceResponse(response.getData(), response.getPageNo(), response.getCount());
    }

    @Override
    public DocumentManagerResponse<DocumentManagerDataResponse> pushSystemGeneratedDocumentToDocMaster(MultipartFile file, String filename, DocUploadRequest uploadRequest) {
        try {
            long start = System.currentTimeMillis();
            log.info("{} | {} Processing setDocumentServiceParameters process for Doc request {}.... ", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.PUSH_DOCUMENT_TO_DOC_MASTER_VIA_REPORT_SERVICE, jsonHelper.convertToJson(uploadRequest));
            var uploadResponse = this.temporaryFileUpload(file, filename);
            if (Boolean.FALSE.equals(uploadResponse.getSuccess())) {
                log.error("{} | {} Processing temporaryFileUpload Failed for Doc Response: {}.... ", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.PUSH_DOCUMENT_TO_DOC_MASTER_VIA_REPORT_SERVICE, jsonHelper.convertToJson(uploadResponse));
                throw new IOException(FILE_UPLOAD_TO_TEMP_FAILED);
            }

            var response = this.saveFile(DocumentManagerSaveFileRequest.builder()
                    .fileName(filename)
                    .entityType(uploadRequest.getEntityType())
                    .entityKey(uploadRequest.getKey())
                    .entityId(uploadRequest.getKey())
                    .secureDownloadLink(uploadResponse.getData().getSecureDownloadLink())
                    .fileSize(uploadResponse.getData().getFileSize())
                    .fileType(uploadResponse.getData().getFileType())
                    .path(uploadResponse.getData().getPath())
                    .source(Constants.SYSTEM_GENERATED)
                    .docType(uploadRequest.getDocType())
                    .docName(uploadRequest.getDocType())
                    .childType(uploadRequest.getChildType())
                    .transportMode(uploadRequest.getTransportMode())
                    .shipmentType(uploadRequest.getShipmentType())
                    .consolidationType(uploadRequest.getConsolidationType())
                    .overrideRuleDocName(true)
                .build());
            log.info("Time take to pushSystemGeneratedDocumentToDocMaster: {}ms", System.currentTimeMillis() - start);
            return response;
        } catch (Exception ex) {
            log.error("{} | {} Failed temporaryFileUpload process for Doc Response: {}.... ", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.PUSH_DOCUMENT_TO_DOC_MASTER_VIA_REPORT_SERVICE, jsonHelper.convertToJson(ex.getLocalizedMessage()));
            throw new DocumentClientException(ex.getMessage());
        }
    }
}
