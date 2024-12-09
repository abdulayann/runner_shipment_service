package com.dpw.runner.shipment.services.document.service;

import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerBulkDownloadRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerFileAndRulesRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerMultipleEntityFileRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerSaveFileRequest;
import com.dpw.runner.shipment.services.document.response.DocumentManagerBulkDownloadResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerDataResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerEntityFileResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerListResponse;
import org.springframework.web.multipart.MultipartFile;

public interface IDocumentManagerService {
    DocumentManagerResponse<DocumentManagerDataResponse> temporaryFileUpload(MultipartFile file, String filename);

    DocumentManagerResponse<DocumentManagerDataResponse> saveFile(DocumentManagerSaveFileRequest saveFileRequest);

    DocumentManagerResponse<DocumentManagerDataResponse> getFileAndRules(DocumentManagerFileAndRulesRequest fileAndRulesRequest);

    DocumentManagerResponse<DocumentManagerDataResponse> getFileById(Long id);

    DocumentManagerResponse<DocumentManagerBulkDownloadResponse> getBulkDownloadLink(DocumentManagerBulkDownloadRequest request);

    DocumentManagerListResponse<DocumentManagerEntityFileResponse> fetchMultipleFilesWithTenant(DocumentManagerMultipleEntityFileRequest request);
}
