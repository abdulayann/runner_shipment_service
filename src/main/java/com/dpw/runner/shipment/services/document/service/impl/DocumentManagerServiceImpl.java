package com.dpw.runner.shipment.services.document.service.impl;


import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.document.config.DocumentManagerRestClient;
import com.dpw.runner.shipment.services.document.exception.BadRequestException;
import com.dpw.runner.shipment.services.document.request.documentmanager.*;
import com.dpw.runner.shipment.services.document.response.*;
import com.dpw.runner.shipment.services.document.service.IDocumentManagerService;
import com.dpw.runner.shipment.services.document.util.FileUtils;
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

@Service
@Slf4j
public class DocumentManagerServiceImpl implements IDocumentManagerService {

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

}
