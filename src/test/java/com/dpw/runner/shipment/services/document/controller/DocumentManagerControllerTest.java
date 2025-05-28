package com.dpw.runner.shipment.services.document.controller;

import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerSaveFileRequest;
import com.dpw.runner.shipment.services.document.response.DocumentDownloadResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerDataResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.service.IDocumentManagerService;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import java.util.Optional;

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

    @Test
    void testDeleteDocument() {
        // Mock
        when(documentManagerService.deleteFile(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = documentManagerController.deleteDocument(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDeleteDocument2() {
        // Mock
        when(documentManagerService.deleteFile(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = documentManagerController.deleteDocument(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDeleteDocument3() {
        // Mock
        when(documentManagerService.deleteFile(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = documentManagerController.deleteDocument(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testGetFileHistory() {
        // Mock
        when(documentManagerService.getFileHistory(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = documentManagerController.getFileHistory(123L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetFileHistory2() {
        // Mock
        when(documentManagerService.getFileHistory(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = documentManagerController.getFileHistory(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testGetFileHistory3() {
        // Mock
        when(documentManagerService.getFileHistory(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = documentManagerController.getFileHistory(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDownloadDocument() {
        // Mock
        when(documentManagerService.downloadDocument(any())).thenReturn(ResponseEntity.ok(DocumentDownloadResponse.builder().content(new byte[1024]).headers(new HttpHeaders()).build()));
        // Test
        var responseEntity = documentManagerController.downloadDocument(123L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDownloadDocument2() {
        // Mock
        when(documentManagerService.downloadDocument(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = documentManagerController.downloadDocument(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDownloadDocument3() {
        // Mock
        when(documentManagerService.downloadDocument(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = documentManagerController.downloadDocument(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }


    @Test
    void testListDocuments() {
        // Mock
        when(documentManagerService.list(any(), any(), any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = documentManagerController.listDocuments(new Object(), Optional.of(1L),  Optional.of(1L));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testListDocuments2() {
        // Mock
        when(documentManagerService.list(any(), any(), any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = documentManagerController.listDocuments(new Object(), Optional.of(1L), Optional.of(1L));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListDocuments3() {
        // Mock
        when(documentManagerService.list(any(), any(), any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = documentManagerController.listDocuments(new Object(), Optional.of(1L), Optional.of(1L));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testTemporaryUpload() {
        // Mock
        when(documentManagerService.temporaryUpload(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = documentManagerController.temporaryUpload(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testTemporaryUpload2() {
        // Mock
        when(documentManagerService.temporaryUpload(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = documentManagerController.temporaryUpload(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testTemporaryUpload3() {
        // Mock
        when(documentManagerService.temporaryUpload(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = documentManagerController.temporaryUpload(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testBulkSaveDocument() {
        // Mock
        when(documentManagerService.bulkSave(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = documentManagerController.bulkSave(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testBulkSaveDocument2() {
        // Mock
        when(documentManagerService.bulkSave(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = documentManagerController.bulkSave(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testBulkSaveDocument3() {
        // Mock
        when(documentManagerService.bulkSave(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = documentManagerController.bulkSave(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testGetDocTypesList() {
        // Mock
        when(documentManagerService.listDocTypes(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = documentManagerController.getDocTypesList(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetDocTypesList2() {
        // Mock
        when(documentManagerService.listDocTypes(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = documentManagerController.getDocTypesList(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testGetDocTypesList3() {
        // Mock
        when(documentManagerService.listDocTypes(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = documentManagerController.getDocTypesList(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }
}
