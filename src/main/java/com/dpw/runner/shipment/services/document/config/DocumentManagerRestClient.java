package com.dpw.runner.shipment.services.document.config;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerBulkDownloadRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerFileAndRulesRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerSaveFileRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerTempFileUploadRequest;
import com.dpw.runner.shipment.services.document.response.DocumentManagerBulkDownloadResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerDataResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.dto.request.CopyDocumentsRequest;
import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.*;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

@Component
@Generated
public class DocumentManagerRestClient {

    @Value("${document-manager.baseUrl}")
    private String baseUrl;

    @Value("${document-manager.copy-file}")
    private String copyFileUrl;


    @Autowired
    private RestTemplate restTemplate;

    public DocumentManagerResponse<DocumentManagerDataResponse> getFileAndRules(String token, DocumentManagerFileAndRulesRequest fileAndRulesRequest) {
        HttpHeaders headers = getHttpHeaders(token);

        HttpEntity<DocumentManagerFileAndRulesRequest> requestEntity = new HttpEntity<>(fileAndRulesRequest, headers);

        String url = baseUrl + "/document-rules/FilesNRules";

        ResponseEntity<DocumentManagerResponse<DocumentManagerDataResponse>> responseEntity = restTemplate.exchange(
                url,
                HttpMethod.POST,
                requestEntity,
                new ParameterizedTypeReference<>() {}
        );

        return responseEntity.getBody();
    }

    @NotNull HttpHeaders getHttpHeaders(String token) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.set("Authorization", token);
        return headers;
    }

    public DocumentManagerResponse<DocumentManagerDataResponse> temporaryFileUpload(DocumentManagerTempFileUploadRequest request) {

        HttpEntity<DocumentManagerTempFileUploadRequest> requestEntity = new HttpEntity<>(request, V1AuthHelper.getHeaders());

        String url = baseUrl + "/files-management/v2/addTemporaryFile";

        ResponseEntity<DocumentManagerResponse<DocumentManagerDataResponse>> responseEntity = restTemplate.exchange(
                url,
                HttpMethod.POST,
                requestEntity,
                new ParameterizedTypeReference<>() {}
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
                new ParameterizedTypeReference<>() {}
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
                new ParameterizedTypeReference<>() {}
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
                new ParameterizedTypeReference<>() {}
        );

        return responseEntity.getBody();
    }

    public ResponseEntity<Object> copyDocuments(CommonRequestModel commonRequestModel) {
        var request = (CopyDocumentsRequest) commonRequestModel.getData();

        HttpHeaders headers = getHttpHeaders(RequestAuthContext.getAuthToken());

        String url = baseUrl + copyFileUrl;

        HttpEntity<Object> httpEntity = new HttpEntity<>(request, headers);

        return restTemplate.postForEntity(url, httpEntity, Object.class);
    }
}
