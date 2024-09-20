package com.dpw.runner.shipment.services.document.config;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerBulkDownloadRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerFileAndRulesRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerSaveFileRequest;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerTempFileUploadRequest;
import com.dpw.runner.shipment.services.document.response.DocumentManagerBulkDownloadResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerDataResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.dto.request.CopyDocumentsRequest;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.*;
import org.springframework.web.client.RestTemplate;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class DocumentManagerRestClientTest {

    @InjectMocks
    private DocumentManagerRestClient documentManagerRestClient;

    @Mock
    private RestTemplate restTemplate;
    @Mock
    private JsonHelper jsonHelper;

    @Value("${document-manager.baseUrl}")
    private String baseUrl;

    @BeforeEach
    void setup() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testGetFileAndRules() {
        String token = "dummy-token";
        DocumentManagerFileAndRulesRequest request = new DocumentManagerFileAndRulesRequest();
        DocumentManagerResponse<DocumentManagerDataResponse> response = new DocumentManagerResponse<>();
        ResponseEntity<DocumentManagerResponse<DocumentManagerDataResponse>> responseEntity = new ResponseEntity<>(response, HttpStatus.OK);

        when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        DocumentManagerResponse<DocumentManagerDataResponse> result = documentManagerRestClient.getFileAndRules(token, request);

        assertEquals(response, result);
        verify(restTemplate, times(1)).exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), any(ParameterizedTypeReference.class));
    }

    @Test
    void testTemporaryFileUpload() {
        DocumentManagerTempFileUploadRequest request = DocumentManagerTempFileUploadRequest.builder().build();
        DocumentManagerResponse<DocumentManagerDataResponse> response = new DocumentManagerResponse<>();
        ResponseEntity<DocumentManagerResponse<DocumentManagerDataResponse>> responseEntity = new ResponseEntity<>(response, HttpStatus.OK);

        when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        DocumentManagerResponse<DocumentManagerDataResponse> result = documentManagerRestClient.temporaryFileUpload(request);

        assertEquals(response, result);
        verify(restTemplate, times(1)).exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), any(ParameterizedTypeReference.class));
    }

    @Test
    void testSaveFile() {
        DocumentManagerSaveFileRequest request = DocumentManagerSaveFileRequest.builder().build();
        DocumentManagerResponse<DocumentManagerDataResponse> response = new DocumentManagerResponse<>();
        ResponseEntity<DocumentManagerResponse<DocumentManagerDataResponse>> responseEntity = new ResponseEntity<>(response, HttpStatus.OK);

        when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        DocumentManagerResponse<DocumentManagerDataResponse> result = documentManagerRestClient.saveFile(request);

        assertEquals(response, result);
        verify(restTemplate, times(1)).exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), any(ParameterizedTypeReference.class));
    }

    @Test
    void testGetFileById() {
        String token = "dummy-token";
        Long id = 1L;
        DocumentManagerResponse<DocumentManagerDataResponse> response = new DocumentManagerResponse<>();
        ResponseEntity<DocumentManagerResponse<DocumentManagerDataResponse>> responseEntity = new ResponseEntity<>(response, HttpStatus.OK);

        when(restTemplate.exchange(anyString(), eq(HttpMethod.GET), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        DocumentManagerResponse<DocumentManagerDataResponse> result = documentManagerRestClient.getFileById(token, id);

        assertEquals(response, result);
        verify(restTemplate, times(1)).exchange(anyString(), eq(HttpMethod.GET), any(HttpEntity.class), any(ParameterizedTypeReference.class));
    }

    @Test
    void testGetBulkDownloadLink() {
        String token = "dummy-token";
        DocumentManagerBulkDownloadRequest request = new DocumentManagerBulkDownloadRequest();
        DocumentManagerResponse<DocumentManagerBulkDownloadResponse> response = new DocumentManagerResponse<>();
        ResponseEntity<DocumentManagerResponse<DocumentManagerBulkDownloadResponse>> responseEntity = new ResponseEntity<>(response, HttpStatus.OK);

        when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), any(ParameterizedTypeReference.class)))
                .thenReturn(responseEntity);

        DocumentManagerResponse<DocumentManagerBulkDownloadResponse> result = documentManagerRestClient.getBulkDownloadLink(token, request);

        assertEquals(response, result);
        verify(restTemplate, times(1)).exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), any(ParameterizedTypeReference.class));
    }

    @Test
    void testGetHttpHeaders() {
        String token = "dummy-token";
        HttpHeaders headers = documentManagerRestClient.getHttpHeaders(token);

        assertNotNull(headers);
        assertEquals(MediaType.APPLICATION_JSON, headers.getContentType());
        assertEquals(token, headers.getFirst("Authorization"));
    }

    @Test
    void testCopyDocuments() {
        CopyDocumentsRequest copyDocumentsRequest = CopyDocumentsRequest.builder().build();
        when(restTemplate.postForEntity(anyString(), any(), eq(Object.class))).thenReturn(new ResponseEntity<>(new Object(), HttpStatus.OK));
        var response = documentManagerRestClient.copyDocuments(CommonRequestModel.buildRequest(copyDocumentsRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testCopyDocuments_Failure() {
        CopyDocumentsRequest copyDocumentsRequest = CopyDocumentsRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(copyDocumentsRequest);
        when(restTemplate.postForEntity(anyString(), any(), eq(Object.class))).thenThrow(new RuntimeException());
        assertThrows(RuntimeException.class, () -> documentManagerRestClient.copyDocuments(commonRequestModel));
    }
}
