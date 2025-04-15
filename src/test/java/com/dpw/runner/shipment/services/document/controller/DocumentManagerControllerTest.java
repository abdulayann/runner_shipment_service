package com.dpw.runner.shipment.services.document.controller;

import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerSaveFileRequest;
import com.dpw.runner.shipment.services.document.response.DocumentManagerDataResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.service.IDocumentManagerService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class DocumentManagerControllerTest {
    @InjectMocks
    private DocumentManagerController documentManagerController;

    @Mock
    private IDocumentManagerService documentManagerService;

    @BeforeEach
    void setup() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testTemporaryFileUpload() {
        MultipartFile file = new MockMultipartFile(
                "file",
                "testfile.txt",
                "text/plain",
                "Test content".getBytes()
        );

        DocumentManagerResponse<DocumentManagerDataResponse> response = new DocumentManagerResponse<>();
        DocumentManagerDataResponse dataResponse = new DocumentManagerDataResponse();
        response.setData(dataResponse);

        when(documentManagerService.temporaryFileUpload(any(MultipartFile.class), anyString()))
                .thenReturn(response);

        ResponseEntity<DocumentManagerResponse<DocumentManagerDataResponse>> result =
                documentManagerController.temporaryFileUpload(file);

        assertEquals(HttpStatus.OK, result.getStatusCode());
        assertEquals(response, result.getBody());
    }

    @Test
    void testSaveFile() {
        DocumentManagerSaveFileRequest saveFileRequest = DocumentManagerSaveFileRequest.builder().build();
        DocumentManagerResponse<DocumentManagerDataResponse> response = new DocumentManagerResponse<>();
        DocumentManagerDataResponse dataResponse = new DocumentManagerDataResponse();
        response.setData(dataResponse);

        when(documentManagerService.saveFile(any(DocumentManagerSaveFileRequest.class)))
                .thenReturn(response);

        ResponseEntity<DocumentManagerResponse<DocumentManagerDataResponse>> result =
                documentManagerController.saveFile(saveFileRequest);

        assertEquals(HttpStatus.OK, result.getStatusCode());
        assertEquals(response, result.getBody());
    }
}
