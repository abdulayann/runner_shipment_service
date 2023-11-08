package com.dpw.runner.shipment.services.document.service.impl;


import com.dpw.runner.shipment.services.document.config.DocumentManagerRestClient;
import com.dpw.runner.shipment.services.document.exception.BadRequestException;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerBulkDownloadRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerFileAndRulesRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerSaveFileRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerTempFileUploadRequest;
import com.dpw.runner.shipment.services.document.response.DocumentManagerBulkDownloadResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerDataResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.service.IDocumentManagerService;
import com.dpw.runner.shipment.services.document.util.FileUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
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

    @Override
    public DocumentManagerResponse<DocumentManagerDataResponse> temporaryFileUpload(MultipartFile file, String filename) {

        if (ObjectUtils.isEmpty(filename)) {
            throw new BadRequestException("fileName cannot be null or empty");
        }

        String token = httpServletRequest.getHeader(HttpHeaders.AUTHORIZATION);
        try {
            String originalFileName = filename;
            String base64EncodedFile = FileUtils.convertMultipartFileToBase64(file);
            String encodedFile = "data:@file/" + FileUtils.getFileExtenation(originalFileName) + ";base64," + base64EncodedFile;

            DocumentManagerTempFileUploadRequest request = DocumentManagerTempFileUploadRequest.builder()
                    .fileName(originalFileName)
                    .encodedfile(encodedFile)
                    .build();

            log.info("temporary upload file: {}", originalFileName);

            return restClient.temporaryFileUpload(token, request);

        } catch (Exception e) {
            log.error("something went wrong in temporaryFileUpload {}", e.toString());
            throw new IllegalArgumentException(e.getMessage());
        }
    }


    @Override
    public DocumentManagerResponse<DocumentManagerDataResponse> saveFile(DocumentManagerSaveFileRequest saveFileRequest) {
        String token = httpServletRequest.getHeader(HttpHeaders.AUTHORIZATION);
        log.info("save file Request: {}", saveFileRequest);
        return restClient.saveFile(token, saveFileRequest);
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

}
