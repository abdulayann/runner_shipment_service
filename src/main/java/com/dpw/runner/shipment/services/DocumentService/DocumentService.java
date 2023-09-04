package com.dpw.runner.shipment.services.DocumentService;

import com.dpw.runner.shipment.services.dto.request.TemplateUploadRequest;
import com.dpw.runner.shipment.services.dto.response.TemplateUploadResponse;
import com.dpw.runner.shipment.services.dto.response.UploadDocumentResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.http.*;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.util.UriBuilder;
import org.springframework.web.util.UriComponentsBuilder;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URL;

@Component
public class DocumentService {
    @Value("${DocumentService.BaseUrl}")
    private String BaseUrl;
    @Value("${DocumentService.UploadFileUrl}")
    private String UploadFileUrl;
    @Value("${DocumentService.DownloadFileUrl}")
    private String DownloadFileUrl;
    @Value("${DocumentService.xApiKey}")
    private String xApikey;
    @Value("${DocumentService.organizationId}")
    private String organizationId;
    @Value("${DocumentService.applicationId}")
    private String applicationId;
    @Value("${TemplateDocumentService.BaseUrl}")
    private String templateBaseUrl;
    @Value("${TemplateDocumentService.xApiKey}")
    private String templatexApiKey;
    @Value("${TemplateDocumentService.organizationId}")
    private String templateOrganizationId;
    @Value("${TemplateDocumentService.applicationId}")
    private String templateApplicationId;
    @Autowired
    private RestTemplate restTemplate;

    public ResponseEntity<UploadDocumentResponse> PostDocument(MultipartFile file, String path) throws Exception{

        String url = BaseUrl+UploadFileUrl;

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.MULTIPART_FORM_DATA);
        headers.add("x-api-key", xApikey);
        headers.add("X-DPW-ApplicationId", applicationId);

        MultiValueMap<String, Object> body = new LinkedMultiValueMap<>();
        body.set("organizationId", organizationId);
        body.set("file", file.getResource());
        body.set("path", path);

        HttpEntity<Object> request = new HttpEntity<Object>(body, headers);

        ResponseEntity<UploadDocumentResponse> response = restTemplate.postForEntity(url, request, UploadDocumentResponse.class);
        return response;
    }

    public ResponseEntity<?> DownloadDocument(String path) throws Exception{

        String url = BaseUrl+DownloadFileUrl;

        URI urlTemplate = UriComponentsBuilder.newInstance().fromUriString(url)
                .queryParam("organizationId", organizationId)
                .queryParam("path", path)
                .build()
                .toUri();

        HttpHeaders headers = new HttpHeaders();
        headers.add("x-api-key", xApikey);
        headers.add("X-DPW-ApplicationId", applicationId);

        HttpEntity<Object> request = new HttpEntity<Object>(headers);

        ResponseEntity<?> response = restTemplate.exchange(urlTemplate, HttpMethod.GET, request, byte[].class);

        return response;
    }

    public ResponseEntity<TemplateUploadResponse> CreateDocumentTemplate(TemplateUploadRequest templateRequest) throws Exception{
        String url = templateBaseUrl;

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.MULTIPART_FORM_DATA);
        headers.add("x-api-key", xApikey);

        MultiValueMap<String, Object> body = new LinkedMultiValueMap<>();
        body.set("file", templateRequest.getFile().getResource());
        body.set("organizationId", templateOrganizationId);
        body.set("applicationId", templateApplicationId);
        body.set("templateName", templateRequest.getFile().getOriginalFilename());
        body.set("metadata", "{\"exporterName\": \"Honda-UK\",\"bookingNumber\": \"DPW897890\"}");

        HttpEntity<Object> request = new HttpEntity<Object>(body, headers);

        ResponseEntity<TemplateUploadResponse> response = restTemplate.exchange(url, HttpMethod.POST, request, TemplateUploadResponse.class);
        return response;
    }
    public ResponseEntity<?> UpdateDocumentTemplate(TemplateUploadRequest templateRequest) throws Exception{
        String url = templateBaseUrl+templateRequest.getPreviousFileId();

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.MULTIPART_FORM_DATA);
        headers.add("x-api-key", templatexApiKey);
        headers.add("X-DPW-ApplicationId", templateApplicationId);

        MultiValueMap<String, Object> body = new LinkedMultiValueMap<>();
        body.set("file", templateRequest.getFile().getResource());
        body.set("organizationId", templateOrganizationId);
        body.set("applicationId", templateApplicationId);
        body.set("templateName", templateRequest.getFile().getOriginalFilename());
        body.set("metadata", "{\"exporterName\": \"Honda-UK\",\"bookingNumber\": \"DPW897890\"}");
        HttpEntity<Object> request = new HttpEntity<Object>(body, headers);

        ResponseEntity<?> response = restTemplate.exchange(url, HttpMethod.PUT, request, String.class);
        return response;
    }
    public ResponseEntity<?> DownloadDocumentTemplate(Object json, String templateId) throws Exception {
        // TODO Provide json object with proper format
        String url = templateBaseUrl+templateId+"/document";

        HttpHeaders headers = new HttpHeaders();
        headers.add("x-api-key", templatexApiKey);
        headers.setContentType(MediaType.APPLICATION_JSON);

        HttpEntity<Object> request = new HttpEntity<Object>(json,headers);

        ResponseEntity<?> response = restTemplate.exchange(url, HttpMethod.POST, request, byte[].class);

        return response;
    }

    public ResponseEntity<UploadDocumentResponse> PostDocument(ByteArrayResource file, String path) throws Exception{

        String url = BaseUrl+UploadFileUrl;

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.MULTIPART_FORM_DATA);
        headers.add("x-api-key", xApikey);
        headers.add("X-DPW-ApplicationId", applicationId);

        MultiValueMap<String, Object> body = new LinkedMultiValueMap<>();
        body.set("organizationId", organizationId);
        body.set("file", file);
        body.set("path", path);

        HttpEntity<Object> request = new HttpEntity<Object>(body, headers);

        ResponseEntity<UploadDocumentResponse> response = restTemplate.postForEntity(url, request, UploadDocumentResponse.class);
        return response;
    }
}
