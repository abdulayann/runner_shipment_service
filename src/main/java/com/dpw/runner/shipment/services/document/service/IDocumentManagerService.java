package com.dpw.runner.shipment.services.document.service;

import com.dpw.runner.shipment.services.ReportingService.Models.DocUploadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.document.request.documentmanager.*;
import com.dpw.runner.shipment.services.document.response.*;
import org.apache.poi.ss.formula.functions.T;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

public interface IDocumentManagerService {
    DocumentManagerResponse<DocumentManagerDataResponse> temporaryFileUpload(MultipartFile file, String filename);

    DocumentManagerResponse<DocumentManagerDataResponse> saveFile(DocumentManagerSaveFileRequest saveFileRequest);

    DocumentManagerResponse<DocumentManagerDataResponse> getFileAndRules(DocumentManagerFileAndRulesRequest fileAndRulesRequest);

    DocumentManagerResponse<DocumentManagerDataResponse> getFileById(Long id);

    DocumentManagerResponse<DocumentManagerBulkDownloadResponse> getBulkDownloadLink(DocumentManagerBulkDownloadRequest request);

    DocumentManagerListResponse<DocumentManagerEntityFileResponse> fetchMultipleFilesWithTenant(DocumentManagerMultipleEntityFileRequest request);
    DocumentManagerResponse<T> updateFileEntities(DocumentManagerUpdateFileEntitiesRequest request);

    ResponseEntity<IRunnerResponse> deleteFile(CommonRequestModel request);
    ResponseEntity<IRunnerResponse> getFileHistory(CommonRequestModel request);
    ResponseEntity<DocumentDownloadResponse> downloadDocument(CommonRequestModel request);
    ResponseEntity<IRunnerResponse> bulkSave(CommonRequestModel request);
    ResponseEntity<IRunnerResponse> temporaryUpload(CommonRequestModel request);
    ResponseEntity<IRunnerResponse> list(CommonRequestModel request, Long page, Long size);
    ResponseEntity<IRunnerResponse> listDocTypes(CommonRequestModel request);
    void pushSystemGeneratedDocumentToDocMaster(MultipartFile file, String filename, DocUploadRequest uploadRequest);
}
