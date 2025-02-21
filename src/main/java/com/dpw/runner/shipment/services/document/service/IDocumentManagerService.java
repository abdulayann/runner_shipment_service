package com.dpw.runner.shipment.services.document.service;

import com.dpw.runner.shipment.services.document.request.documentmanager.*;
import com.dpw.runner.shipment.services.document.response.DocumentManagerBulkDownloadResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerDataResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerEntityFileResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerListResponse;
import org.apache.poi.ss.formula.functions.T;
import org.springframework.web.multipart.MultipartFile;

public interface IDocumentManagerService {
    DocumentManagerResponse<DocumentManagerDataResponse> temporaryFileUpload(MultipartFile file, String filename);

    DocumentManagerResponse<DocumentManagerDataResponse> saveFile(DocumentManagerSaveFileRequest saveFileRequest);

    DocumentManagerResponse<DocumentManagerDataResponse> getFileAndRules(DocumentManagerFileAndRulesRequest fileAndRulesRequest);

    DocumentManagerResponse<DocumentManagerDataResponse> getFileById(Long id);

    DocumentManagerResponse<DocumentManagerBulkDownloadResponse> getBulkDownloadLink(DocumentManagerBulkDownloadRequest request);

    DocumentManagerListResponse<DocumentManagerEntityFileResponse> fetchMultipleFilesWithTenant(DocumentManagerMultipleEntityFileRequest request);
    DocumentManagerResponse<T> updateFileEntities(DocumentManagerUpdateFileEntitiesRequest request);
}
