package com.dpw.runner.shipment.services.DocumentService;

import com.dpw.runner.shipment.services.commons.constants.LoggingConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentSettingsConstants;
import com.dpw.runner.shipment.services.dto.request.TemplateUploadRequest;
import com.dpw.runner.shipment.services.dto.response.TemplateUploadResponse;
import com.dpw.runner.shipment.services.dto.response.UploadDocumentResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.http.*;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;
import java.util.Arrays;

@Component
@Generated
public class DocumentService {
    @Value("${DocumentService.BaseUrl}")
    private String baseUrl;
    @Value("${DocumentService.UploadFileUrl}")
    private String uploadFileUrl;
    @Value("${DocumentService.DownloadFileUrl}")
    private String downloadFileUrl;
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

    public static final String X_API_KEY= "x-api-key";
    public static final String X_DPW_APPLICATION_ID = "X-DPW-ApplicationId";
    public static final String ORGANIZATION_ID= "organizationId";
    public static final String FILE = "file";
    public static final String PATH = "path";
    public static final String APPLICATION_ID = "applicationId";
    public static final String TEMPLATE_NAME = "templateName";
    public static final String META_DATA = "metadata";


    public ResponseEntity<UploadDocumentResponse> postDocument(MultipartFile file, String path){

        String url = baseUrl+uploadFileUrl;

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.MULTIPART_FORM_DATA);
        headers.add(X_API_KEY, xApikey);
        headers.add(X_DPW_APPLICATION_ID, applicationId);

        MultiValueMap<String, Object> body = new LinkedMultiValueMap<>();
        body.set(ORGANIZATION_ID, organizationId);
        body.set(FILE, file.getResource());
        body.set(PATH, path);

        HttpEntity<Object> request = new HttpEntity<>(body, headers);

        return restTemplate.postForEntity(url, request, UploadDocumentResponse.class);
    }

    public ResponseEntity<byte[]> downloadDocument(String path){

        String url = baseUrl+downloadFileUrl;

        URI urlTemplate = UriComponentsBuilder.fromUriString(url)
                .queryParam(ORGANIZATION_ID, organizationId)
                .queryParam(PATH, path)
                .build()
                .toUri();

        HttpHeaders headers = new HttpHeaders();
        headers.add(X_API_KEY, xApikey);
        headers.add(X_DPW_APPLICATION_ID, applicationId);

        HttpEntity<Object> request = new HttpEntity<>(headers);

        return restTemplate.exchange(urlTemplate, HttpMethod.GET, request, byte[].class);
    }

    public ResponseEntity<TemplateUploadResponse> createDocumentTemplate(TemplateUploadRequest templateRequest){
        String url = templateBaseUrl;

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.MULTIPART_FORM_DATA);
        headers.add(X_API_KEY, xApikey);

        MultiValueMap<String, Object> body = new LinkedMultiValueMap<>();
        body.set(FILE, templateRequest.getFile().getResource());
        body.set(ORGANIZATION_ID, templateOrganizationId);
        body.set(APPLICATION_ID, templateApplicationId);
        body.set(TEMPLATE_NAME , templateRequest.getFile().getOriginalFilename());
        body.set(META_DATA, "{\"exporterName\": \"Honda-UK\",\"bookingNumber\": \"DPW897890\"}");

        HttpEntity<Object> request = new HttpEntity<>(body, headers);

        return restTemplate.exchange(url, HttpMethod.POST, request, TemplateUploadResponse.class);
    }
    public ResponseEntity<String> updateDocumentTemplate(TemplateUploadRequest templateRequest){
        String url = templateBaseUrl+templateRequest.getPreviousFileId();

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.MULTIPART_FORM_DATA);
        headers.add(X_API_KEY, templatexApiKey);
        headers.add(X_DPW_APPLICATION_ID, templateApplicationId);

        MultiValueMap<String, Object> body = new LinkedMultiValueMap<>();
        body.set(FILE, templateRequest.getFile().getResource());
        body.set(ORGANIZATION_ID, templateOrganizationId);
        body.set(APPLICATION_ID, templateApplicationId);
        body.set(TEMPLATE_NAME, templateRequest.getFile().getOriginalFilename());
        body.set(META_DATA, "{\"exporterName\": \"Honda-UK\",\"bookingNumber\": \"DPW897890\"}");
        HttpEntity<Object> request = new HttpEntity<>(body, headers);

        return restTemplate.exchange(url, HttpMethod.PUT, request, String.class);
    }
    public ResponseEntity<byte[]> downloadDocumentTemplate(Object json, String templateId){
        // Later: Provide json object with proper format
        String url = templateBaseUrl+templateId+"/document";

        HttpHeaders headers = new HttpHeaders();
        headers.add(X_API_KEY, templatexApiKey);
        headers.add(LoggingConstants.REQUEST_ID, LoggerHelper.getRequestIdFromMDC());
        headers.setContentType(MediaType.APPLICATION_JSON);

        HttpEntity<Object> request = new HttpEntity<>(json,headers);

        return restTemplate.exchange(url, HttpMethod.POST, request, byte[].class);
    }

    public byte[] downloadTemplate(String templateId) throws RunnerException {
        String url = templateBaseUrl+templateId+"/download";

        HttpHeaders headers = new HttpHeaders();
        headers.add(X_API_KEY, templatexApiKey);
        headers.setContentType(MediaType.TEXT_PLAIN);

        HttpEntity<Object> request = new HttpEntity<>(headers);

        ResponseEntity<byte[]> response = restTemplate.exchange(url, HttpMethod.GET, request, byte[].class);
        if(response.getStatusCode() != HttpStatus.OK){
            LoggerHelper.error("Error While Downloading Template From Document Service");
            String responseMsg = ShipmentSettingsConstants.DOWNLOAD_TEMPLATE_FAILED + " : " + Arrays.toString(response.getBody());
            throw new RunnerException(responseMsg);
        }
        return response.getBody();
    }

    public ResponseEntity<UploadDocumentResponse> postDocument(ByteArrayResource file, String path){

        String url = baseUrl+uploadFileUrl;

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.MULTIPART_FORM_DATA);
        headers.add(X_API_KEY, xApikey);
        headers.add(X_DPW_APPLICATION_ID, applicationId);

        MultiValueMap<String, Object> body = new LinkedMultiValueMap<>();
        body.set(ORGANIZATION_ID, organizationId);
        body.set(FILE, file);
        body.set(PATH, path);

        HttpEntity<Object> request = new HttpEntity<>(body, headers);

        return restTemplate.postForEntity(url, request, UploadDocumentResponse.class);
    }
}
