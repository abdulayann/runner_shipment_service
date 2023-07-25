package com.dpw.runner.shipment.services.DocumentService;

import com.dpw.runner.shipment.services.dto.response.UploadDocumentResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
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
}
