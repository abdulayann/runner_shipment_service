package com.dpw.runner.shipment.services.document.service.impl;

import com.dpw.runner.shipment.services.document.config.DocumentManagerRestClient;
import com.dpw.runner.shipment.services.document.request.documentmanager.*;
import com.dpw.runner.shipment.services.document.response.DocumentManagerBulkDownloadResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerDataResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.util.BASE64DecodedMultipartFile;
import org.apache.poi.ss.formula.functions.T;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import java.io.UnsupportedEncodingException;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class DocumentManagerServiceImplTest {
    @Mock
    private DocumentManagerRestClient documentManagerRestClient;

    @InjectMocks
    private DocumentManagerServiceImpl documentManagerServiceImpl;

    @Mock
    private HttpServletRequest httpServletRequest;

    /**
     * Method under test:
     * {@link DocumentManagerServiceImpl#temporaryFileUpload(MultipartFile, String)}
     */
    @Test
    void testTemporaryFileUpload() throws UnsupportedEncodingException {
        // Arrange
        DocumentManagerDataResponse documentManagerDataResponse = new DocumentManagerDataResponse();
        documentManagerDataResponse.setFileGuid("1234");


        DocumentManagerResponse<DocumentManagerDataResponse> documentManagerResponse = new DocumentManagerResponse<>();
        documentManagerResponse.setCount(3L);

        when(documentManagerRestClient.temporaryFileUpload(Mockito.<DocumentManagerTempFileUploadRequest>any()))
                .thenReturn(documentManagerResponse);

        // Act
        DocumentManagerResponse<DocumentManagerDataResponse> actualTemporaryFileUploadResult = documentManagerServiceImpl
                .temporaryFileUpload(new BASE64DecodedMultipartFile("AXAXAXAX".getBytes("UTF-8")), "foo.txt");

        // Assert
        verify(documentManagerRestClient).temporaryFileUpload(isA(DocumentManagerTempFileUploadRequest.class));
        assertSame(documentManagerResponse, actualTemporaryFileUploadResult);
    }

    /**
     * Method under test:
     * {@link DocumentManagerServiceImpl#temporaryFileUpload(MultipartFile, String)}
     */
    @Test
    void testTemporaryFileUpload2() {
        // Arrange
        DocumentManagerDataResponse documentManagerDataResponse = new DocumentManagerDataResponse();
        documentManagerDataResponse.setFileGuid("1234");

        DocumentManagerResponse<DocumentManagerDataResponse> documentManagerResponse = new DocumentManagerResponse<>();
        documentManagerResponse.setCount(3L);

        when(documentManagerRestClient.temporaryFileUpload(Mockito.<DocumentManagerTempFileUploadRequest>any()))
                .thenReturn(documentManagerResponse);

        // Act
        DocumentManagerResponse<DocumentManagerDataResponse> actualTemporaryFileUploadResult = documentManagerServiceImpl
                .temporaryFileUpload(new BASE64DecodedMultipartFile(new byte[]{-1, 'X', 'A', 'X', 'A', 'X', 'A', 'X'}),
                        "foo.txt");

        // Assert
        verify(documentManagerRestClient).temporaryFileUpload(isA(DocumentManagerTempFileUploadRequest.class));
        assertSame(documentManagerResponse, actualTemporaryFileUploadResult);
    }

    /**
     * Method under test:
     * {@link DocumentManagerServiceImpl#temporaryFileUpload(MultipartFile, String)}
     */
    @Test
    void testTemporaryFileUpload3() {
        // Arrange
        DocumentManagerDataResponse documentManagerDataResponse = new DocumentManagerDataResponse();
        documentManagerDataResponse.setFileGuid("1234");


        DocumentManagerResponse<DocumentManagerDataResponse> documentManagerResponse = new DocumentManagerResponse<>();
        documentManagerResponse.setCount(3L);
        documentManagerResponse.setData(documentManagerDataResponse);

        when(documentManagerRestClient.temporaryFileUpload(Mockito.<DocumentManagerTempFileUploadRequest>any()))
                .thenReturn(documentManagerResponse);

        // Act
        DocumentManagerResponse<DocumentManagerDataResponse> actualTemporaryFileUploadResult = documentManagerServiceImpl
                .temporaryFileUpload(new BASE64DecodedMultipartFile(new byte[]{}), "foo.txt");

        // Assert
        verify(documentManagerRestClient).temporaryFileUpload(isA(DocumentManagerTempFileUploadRequest.class));
        assertSame(documentManagerResponse, actualTemporaryFileUploadResult);
    }

    /**
     * Method under test:
     * {@link DocumentManagerServiceImpl#temporaryFileUpload(MultipartFile, String)}
     */
    @Test
    void testTemporaryFileUpload4() {
        // Arrange
        DocumentManagerDataResponse documentManagerDataResponse = new DocumentManagerDataResponse();
        documentManagerDataResponse.setFileGuid("1234");
        documentManagerDataResponse.setFileId("42");

        DocumentManagerResponse<DocumentManagerDataResponse> documentManagerResponse = new DocumentManagerResponse<>();
        documentManagerResponse.setCount(3L);
        documentManagerResponse.setData(documentManagerDataResponse);

        when(documentManagerRestClient.temporaryFileUpload(Mockito.<DocumentManagerTempFileUploadRequest>any()))
                .thenReturn(documentManagerResponse);

        // Act
        DocumentManagerResponse<DocumentManagerDataResponse> actualTemporaryFileUploadResult = documentManagerServiceImpl
                .temporaryFileUpload(new BASE64DecodedMultipartFile(
                        new byte[]{'A', -1, 'A', -1, 'A', -1, 'A', -1, 'A', -1, 'A', -1, 'A', -1, 'A', -1}), "foo.txt");

        // Assert
        verify(documentManagerRestClient).temporaryFileUpload(isA(DocumentManagerTempFileUploadRequest.class));
        assertSame(documentManagerResponse, actualTemporaryFileUploadResult);
    }

    /**
     * Method under test:
     * {@link DocumentManagerServiceImpl#temporaryFileUpload(MultipartFile, String)}
     */
    @Test
    void testTemporaryFileUpload5() {
        // Arrange
        DocumentManagerDataResponse documentManagerDataResponse = new DocumentManagerDataResponse();
        documentManagerDataResponse.setFileGuid("1234");
        documentManagerDataResponse.setFileId("42");


        DocumentManagerResponse<DocumentManagerDataResponse> documentManagerResponse = new DocumentManagerResponse<>();
        documentManagerResponse.setCount(3L);

        when(documentManagerRestClient.temporaryFileUpload(Mockito.<DocumentManagerTempFileUploadRequest>any()))
                .thenReturn(documentManagerResponse);

        // Act
        DocumentManagerResponse<DocumentManagerDataResponse> actualTemporaryFileUploadResult = documentManagerServiceImpl
                .temporaryFileUpload(new BASE64DecodedMultipartFile(new byte[]{'A', -1, 'A', -1, 'A', -1, 'A', -1, 'A', -1, 'A',
                        -1, 'A', -1, 'A', -1, 'A', -1, 'A', -1, 'A', -1, 'A', -1}), "foo.txt");

        // Assert
        verify(documentManagerRestClient).temporaryFileUpload(isA(DocumentManagerTempFileUploadRequest.class));
        assertSame(documentManagerResponse, actualTemporaryFileUploadResult);
    }

    /**
     * Method under test:
     * {@link DocumentManagerServiceImpl#temporaryFileUpload(MultipartFile, String)}
     */
    @Test
    void testTemporaryFileUpload6() {
        // Arrange, Act and Assert
        assertThrows(IllegalArgumentException.class, () -> documentManagerServiceImpl.temporaryFileUpload(null, "foo.txt"));
    }

    @Test
    void temporaryFileUpload() {
        var mockResponse = new DocumentManagerResponse<DocumentManagerDataResponse>();
        when(documentManagerRestClient.saveFile(any())).thenReturn(mockResponse);
        var responseEntity = documentManagerServiceImpl.saveFile(DocumentManagerSaveFileRequest.builder().build());
        assertNotNull(responseEntity);
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void getFileAndRules() {
        var mockResponse = new DocumentManagerResponse<DocumentManagerDataResponse>();
        when(documentManagerRestClient.getFileAndRules(any(), any())).thenReturn(mockResponse);
        var responseEntity = documentManagerServiceImpl.getFileAndRules(new DocumentManagerFileAndRulesRequest());
        assertNotNull(responseEntity);
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void getFileById() {
        var mockResponse = new DocumentManagerResponse<DocumentManagerDataResponse>();
        when(documentManagerRestClient.getFileById(any(), any())).thenReturn(mockResponse);
        var responseEntity = documentManagerServiceImpl.getFileById(11L);
        assertNotNull(responseEntity);
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void getBulkDownloadLink() {
        var mockResponse = new DocumentManagerResponse<DocumentManagerBulkDownloadResponse>();
        when(documentManagerRestClient.getBulkDownloadLink(any(), any())).thenReturn(mockResponse);
        var responseEntity = documentManagerServiceImpl.getBulkDownloadLink(new DocumentManagerBulkDownloadRequest());
        assertNotNull(responseEntity);
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void updateFileEntities() {
        var mockResponse = new DocumentManagerResponse<T>();
        when(documentManagerRestClient.updateFileEntities(any())).thenReturn(mockResponse);
        var responseEntity = documentManagerServiceImpl.updateFileEntities(new DocumentManagerUpdateFileEntitiesRequest());
        assertNotNull(responseEntity);
        assertEquals(mockResponse, responseEntity);
    }

}
