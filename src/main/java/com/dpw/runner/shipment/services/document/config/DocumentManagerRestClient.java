package com.dpw.runner.shipment.services.document.config;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.LoggingConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerBulkDownloadRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerFileAndRulesRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerSaveFileRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerTempFileUploadRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerMultipleEntityFileRequest;
import com.dpw.runner.shipment.services.document.response.DocumentManagerBulkDownloadResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerDataResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerListResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerEntityFileResponse;
import com.dpw.runner.shipment.services.dto.request.CopyDocumentsRequest;
import com.dpw.runner.shipment.services.exception.exceptions.DocumentClientException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;

import java.util.concurrent.CompletableFuture;

import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.*;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

@Component
@Generated
@Slf4j
public class DocumentManagerRestClient {

    @Value("${document-manager.baseUrl}")
    private String baseUrl;

    @Value("${document-manager.copy-file}")
    private String copyFileUrl;

    @Value("${document-manager.multipleEntityFilesWithTenant}")
    private String multipleEntityFilesWithTenantUrl;

    private final JsonHelper jsonHelper;

    private final RestTemplate restTemplate;

    @Autowired
    DocumentManagerRestClient(RestTemplate restTemplate, JsonHelper jsonHelper) {
        this.jsonHelper = jsonHelper;
        this.restTemplate = restTemplate;
    }

    public DocumentManagerResponse<DocumentManagerDataResponse> getFileAndRules(String token, DocumentManagerFileAndRulesRequest fileAndRulesRequest) {
        HttpHeaders headers = getHttpHeaders(token);

        HttpEntity<DocumentManagerFileAndRulesRequest> requestEntity = new HttpEntity<>(fileAndRulesRequest, headers);

        String url = baseUrl + "/document-rules/FilesNRules";

        ResponseEntity<DocumentManagerResponse<DocumentManagerDataResponse>> responseEntity = restTemplate.exchange(
                url,
                HttpMethod.POST,
                requestEntity,
                new ParameterizedTypeReference<>() {
                }
        );

        return responseEntity.getBody();
    }

    @NotNull HttpHeaders getHttpHeaders(String token) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.set("Authorization", token);
        headers.add(LoggingConstants.REQUEST_ID, LoggerHelper.getRequestIdFromMDC());
        headers.add(Constants.SOURCE_SERVICE_TYPE, LoggingConstants.SHIPMENT);
        return headers;
    }

    public DocumentManagerResponse<DocumentManagerDataResponse> temporaryFileUpload(DocumentManagerTempFileUploadRequest request) {

        HttpEntity<DocumentManagerTempFileUploadRequest> requestEntity = new HttpEntity<>(request, V1AuthHelper.getHeaders());

        String url = baseUrl + "/files-management/v2/addTemporaryFile";

        ResponseEntity<DocumentManagerResponse<DocumentManagerDataResponse>> responseEntity = restTemplate.exchange(
                url,
                HttpMethod.POST,
                requestEntity,
                new ParameterizedTypeReference<>() {
                }
        );

        return responseEntity.getBody();
    }

    public DocumentManagerResponse<DocumentManagerDataResponse> saveFile(DocumentManagerSaveFileRequest request) {

        HttpEntity<DocumentManagerSaveFileRequest> requestEntity = new HttpEntity<>(request, V1AuthHelper.getHeaders());

        String url = baseUrl + "/files-management/v2/saveFile";

        ResponseEntity<DocumentManagerResponse<DocumentManagerDataResponse>> responseEntity = restTemplate.exchange(
                url,
                HttpMethod.POST,
                requestEntity,
                new ParameterizedTypeReference<>() {
                }
        );

        return responseEntity.getBody();
    }

    public DocumentManagerResponse<DocumentManagerDataResponse> getFileById(String token, Long id) {
        HttpHeaders headers = getHttpHeaders(token);

        String url = baseUrl + "/files-management?id=" + id;

        ResponseEntity<DocumentManagerResponse<DocumentManagerDataResponse>> responseEntity = restTemplate.exchange(
                url,
                HttpMethod.GET,
                new HttpEntity<>(headers),
                new ParameterizedTypeReference<>() {
                }
        );

        return responseEntity.getBody();
    }

    public DocumentManagerResponse<DocumentManagerBulkDownloadResponse> getBulkDownloadLink(String token, DocumentManagerBulkDownloadRequest request) {
        HttpHeaders headers = getHttpHeaders(token);

        HttpEntity<DocumentManagerBulkDownloadRequest> requestEntity = new HttpEntity<>(request, headers);

        String url = baseUrl + "/files-management/bulk-download";

        ResponseEntity<DocumentManagerResponse<DocumentManagerBulkDownloadResponse>> responseEntity = restTemplate.exchange(
                url,
                HttpMethod.POST,
                requestEntity,
                new ParameterizedTypeReference<>() {
                }
        );

        return responseEntity.getBody();
    }

    @Async
    public CompletableFuture<ResponseEntity<Object>> copyDocuments(CommonRequestModel commonRequestModel, String authToken) {
        try {
            var request = (CopyDocumentsRequest) commonRequestModel.getData();
            log.info("Copy Document Request {}", jsonHelper.convertToJson(request));

            HttpHeaders headers = getHttpHeaders(authToken);
            HttpEntity<Object> httpEntity = new HttpEntity<>(request, headers);

            var response = restTemplate.postForEntity(baseUrl + copyFileUrl, httpEntity, Object.class);
            log.info("Copy Document Response {}", jsonHelper.convertToJson(response));

            return CompletableFuture.completedFuture(response);
        } catch (Exception ex) {
            log.error("Error in Copy document Api from Document Service: {}", ex.getMessage());
            // It's good practice to handle exceptions in async methods
            return CompletableFuture.failedFuture(ex);
        }
    }

    public DocumentManagerListResponse<DocumentManagerEntityFileResponse> multipleEntityFilesWithTenant(DocumentManagerMultipleEntityFileRequest request) {
        try {
            HttpHeaders headers = getHttpHeaders(RequestAuthContext.getAuthToken());
            HttpEntity<DocumentManagerMultipleEntityFileRequest> requestEntity = new HttpEntity<>(request, headers);
            String url = baseUrl + multipleEntityFilesWithTenantUrl;

            ResponseEntity<DocumentManagerListResponse<DocumentManagerEntityFileResponse>> responseEntity = restTemplate.exchange(
                    url,
                    HttpMethod.POST,
                    requestEntity,
                    new ParameterizedTypeReference<>() {
                    }
            );

            return responseEntity.getBody();
        } catch (Exception ex) {
            log.error("Error in MultipleEntityFilesWithTenant Api from Document Service: {}", ex.getMessage());
            throw new DocumentClientException(ex.getMessage());
        }
    }
}
