package com.dpw.runner.shipment.services.document.config;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.LoggingConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.document.request.documentmanager.*;
import com.dpw.runner.shipment.services.document.response.*;
import com.dpw.runner.shipment.services.dto.request.CopyDocumentsRequest;
import com.dpw.runner.shipment.services.exception.exceptions.DocumentClientException;
import com.dpw.runner.shipment.services.exception.exceptions.UnAuthorizedException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.formula.functions.T;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.*;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestTemplate;

import java.util.concurrent.CompletableFuture;

@Component
@Generated
@Slf4j
public class DocumentManagerRestClient {

    public static final String UN_AUTHORIZED_EXCEPTION_STRING = "UnAuthorizedException";

    @Value("${document-manager.baseUrl}")
    private String baseUrl;

    @Value("${document-manager.copy-file}")
    private String copyFileUrl;

    @Value("${document-manager.multipleEntityFilesWithTenant}")
    private String multipleEntityFilesWithTenantUrl;

    @Value("${document-manager.updateFileEntities}")
    private String updateFileEntitiesUrl;

    private JsonHelper jsonHelper;

    private RestTemplate restTemplate;

    @Value("${document-manager.baseUrl}${document-manager.delete}")
    private String documentDelete;

    @Value("${document-manager.baseUrl}${document-manager.file-history}")
    private String documentHistory;

    @Value("${document-manager.baseUrl}${document-manager.download}")
    private String documentDownload;

    @Value("${document-manager.baseUrl}${document-manager.bulk-save}")
    private String documentBulkSave;

    @Value("${document-manager.baseUrl}${document-manager.temporary-upload}")
    private String documentTemporaryUpload;

    @Value("${document-manager.baseUrl}${document-manager.list}")
    private String documentList;

    @Value("${document-manager.baseUrl}${document-manager.doc-type-list}")
    private String docTypeList;
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
        log.info("{} | URL: {} | downloadDocument request: {}", url, LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(requestEntity));

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

    public DocumentManagerResponse<T> updateFileEntities(DocumentManagerUpdateFileEntitiesRequest request) {
        try {
            HttpHeaders headers = getHttpHeaders(RequestAuthContext.getAuthToken());
            HttpEntity<DocumentManagerUpdateFileEntitiesRequest> requestEntity = new HttpEntity<>(request, headers);
            String url = baseUrl + updateFileEntitiesUrl;

            ResponseEntity<DocumentManagerResponse<T>> responseEntity = restTemplate.exchange(
                    url,
                    HttpMethod.PUT,
                    requestEntity,
                    new ParameterizedTypeReference<>() {
                    }
            );

            return responseEntity.getBody();
        } catch (Exception ex) {
            log.error("CR-ID {} || Error in updateFileEntities Api from Document Service: {} and request sent is: {}",
                    LoggerHelper.getRequestIdFromMDC(), ex.getMessage(), request);
            throw new DocumentClientException(ex.getMessage());
        }
    }

    public DocumentManagerResponse<T> deleteFile(Object object) {
        try {
            HttpHeaders headers = getHttpHeaders(RequestAuthContext.getAuthToken());
            HttpEntity<Object> httpEntity = new HttpEntity<>(object, headers);
            log.info("{} | URL: {} | deleteFile request: {}", this.documentDelete, LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(object));
            var response  = restTemplate.exchange(
                    this.documentDelete,
                    HttpMethod.PUT,
                    httpEntity,
                    new ParameterizedTypeReference<>() {}
            );
            return jsonHelper.convertValue(response.getBody(), DocumentManagerResponse.class);
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            if (ex.getStatusCode() == HttpStatus.UNAUTHORIZED)
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            throw new DocumentClientException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), DocumentManagerResponse.class).getErrorMessage());
        } catch (Exception var7) {
            throw new DocumentClientException(var7.getMessage());
        }

    }

    public DocumentManagerResponse<T> getFileHistory(Object object) {
        try {
            HttpHeaders headers = getHttpHeaders(RequestAuthContext.getAuthToken());
            HttpEntity<Object> httpEntity = new HttpEntity<>(object, headers);
            log.info("{} | URL: {} | getFileHistory request: {}", this.documentHistory + "/" + object, LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(object));
            var response  = restTemplate.exchange(
                    this.documentHistory + "/" + object,
                    HttpMethod.GET,
                    httpEntity,
                    new ParameterizedTypeReference<>() {}
            );
            return jsonHelper.convertValue(response.getBody(), DocumentManagerResponse.class);
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            if (ex.getStatusCode() == HttpStatus.UNAUTHORIZED)
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            throw new DocumentClientException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), DocumentManagerResponse.class).getErrorMessage());
        } catch (Exception var7) {
            throw new DocumentClientException(var7.getMessage());
        }

    }

    public ResponseEntity<byte[]> downloadDocument(Object object) {
        try {
            HttpHeaders headers = getHttpHeaders(RequestAuthContext.getAuthToken());
            HttpEntity<Object> httpEntity = new HttpEntity<>(object, headers);

            log.info("{} | URL: {} | downloadDocument request: {}", this.documentDownload + "?id=" + object, LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(object));
            var response  = restTemplate.exchange(
                    this.documentDownload + "?id=" + object,
                    HttpMethod.GET,
                    httpEntity,
                    byte[].class
            );
            return ResponseEntity.ok(response.getBody());
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            if (ex.getStatusCode() == HttpStatus.UNAUTHORIZED)
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            throw new DocumentClientException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), DocumentManagerResponse.class).getErrorMessage());
        } catch (Exception var7) {
            throw new DocumentClientException(var7.getMessage());
        }

    }

    public DocumentManagerResponse<T> bulkSaveFiles(Object object) {
        try {
            HttpHeaders headers = getHttpHeaders(RequestAuthContext.getAuthToken());
            HttpEntity<Object> httpEntity = new HttpEntity<>(object, headers);
            log.info("{} | URL: {} | bulkSaveFiles request: {}", this.documentBulkSave, LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(object));
            var response  = restTemplate.exchange(
                    this.documentBulkSave,
                    HttpMethod.POST,
                    httpEntity,
                    new ParameterizedTypeReference<>() {}
            );
            return jsonHelper.convertValue(response.getBody(), DocumentManagerResponse.class);
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            if (ex.getStatusCode() == HttpStatus.UNAUTHORIZED)
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            throw new DocumentClientException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), DocumentManagerResponse.class).getErrorMessage());
        } catch (Exception var7) {
            throw new DocumentClientException(var7.getMessage());
        }

    }

    public DocumentManagerResponse<T> temporaryUpload(Object object) {
        try {
            HttpHeaders headers = getHttpHeaders(RequestAuthContext.getAuthToken());
            HttpEntity<Object> httpEntity = new HttpEntity<>(object, headers);
            log.info("{} | URL: {} | temporaryUpload request: {}", this.documentTemporaryUpload, LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(object));
            var response  = restTemplate.exchange(
                    this.documentTemporaryUpload,
                    HttpMethod.POST,
                    httpEntity,
                    new ParameterizedTypeReference<>() {}
            );
            return jsonHelper.convertValue(response.getBody(), DocumentManagerResponse.class);
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            if (ex.getStatusCode() == HttpStatus.UNAUTHORIZED)
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            throw new DocumentClientException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), DocumentManagerResponse.class).getErrorMessage());
        } catch (Exception var7) {
            throw new DocumentClientException(var7.getMessage());
        }

    }

    public DocumentManagerResponse<T> list(Object object) {
        try {
            HttpHeaders headers = getHttpHeaders(RequestAuthContext.getAuthToken());
            HttpEntity<Object> httpEntity = new HttpEntity<>(object, headers);
            log.info("{} | URL: {} | list request: {}", this.documentList, LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(object));
            var response  = restTemplate.exchange(
                    this.documentList,
                    HttpMethod.POST,
                    httpEntity,
                    new ParameterizedTypeReference<>() {}
            );
            return jsonHelper.convertValue(response.getBody(), DocumentManagerResponse.class);
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            if (ex.getStatusCode() == HttpStatus.UNAUTHORIZED)
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            throw new DocumentClientException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), DocumentManagerResponse.class).getErrorMessage());
        } catch (Exception var7) {
            throw new DocumentClientException(var7.getMessage());
        }

    }

    public DocumentManagerResponse<T> listDocTypes(Object object) {
        try {
            HttpHeaders headers = getHttpHeaders(RequestAuthContext.getAuthToken());
            HttpEntity<Object> httpEntity = new HttpEntity<>(object, headers);
            log.info("{} | URL: {} | list request: {}", this.docTypeList, LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(object));
            var response  = restTemplate.exchange(
                    this.docTypeList,
                    HttpMethod.POST,
                    httpEntity,
                    new ParameterizedTypeReference<>() {}
            );
            return jsonHelper.convertValue(response.getBody(), DocumentManagerResponse.class);
        } catch (HttpClientErrorException | HttpServerErrorException ex) {
            if (ex.getStatusCode() == HttpStatus.UNAUTHORIZED)
                throw new UnAuthorizedException(UN_AUTHORIZED_EXCEPTION_STRING);
            throw new DocumentClientException(jsonHelper.readFromJson(ex.getResponseBodyAsString(), DocumentManagerResponse.class).getErrorMessage());
        } catch (Exception var7) {
            throw new DocumentClientException(var7.getMessage());
        }

    }
}
