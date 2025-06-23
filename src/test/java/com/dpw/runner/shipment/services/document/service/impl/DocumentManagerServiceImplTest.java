package com.dpw.runner.shipment.services.document.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.DocUploadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.document.config.DocumentManagerRestClient;
import com.dpw.runner.shipment.services.document.request.documentmanager.*;
import com.dpw.runner.shipment.services.document.response.*;
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
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import java.io.UnsupportedEncodingException;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
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

    @Test
    void testDeleteFile() {
        var mockResponse = createMockResponse();
        when(documentManagerRestClient.deleteFile(any())).thenReturn(mockResponse);
        var responseEntity = documentManagerServiceImpl.deleteFile(CommonRequestModel.builder().dependentData(new DocumentManagerUpdateFileEntitiesRequest()).build());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.getBody() instanceof DependentServiceResponse);
        assertEquals(mockResponse.getData(), ((DependentServiceResponse) responseEntity.getBody()).getData());
    }

    @Test
    void testGetFileHistory() {
        var mockResponse = createMockResponse();
        when(documentManagerRestClient.getFileHistory(any())).thenReturn(mockResponse);
        var responseEntity = documentManagerServiceImpl.getFileHistory(CommonRequestModel.builder().data(CommonGetRequest.builder().id(11L).build()).build());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.getBody() instanceof DependentServiceResponse);
        assertEquals(mockResponse.getData(), ((DependentServiceResponse) responseEntity.getBody()).getData());
    }

    @Test
    void testDownloadDocument() {
        var mockResponse = ResponseEntity.ok(new byte[1024]);
        when(documentManagerRestClient.downloadDocument(any())).thenReturn(mockResponse);
        var responseEntity = documentManagerServiceImpl.downloadDocument(CommonRequestModel.builder().data(CommonGetRequest.builder().id(11L).build()).build());
        assertNotNull(responseEntity);
        assertEquals(mockResponse.getBody(), responseEntity);
    }

    @Test
    void testBulkSave() {
        var mockResponse = createMockResponse();
        when(documentManagerRestClient.bulkSaveFiles(any())).thenReturn(mockResponse);
        var responseEntity = documentManagerServiceImpl.bulkSave(CommonRequestModel.builder().id(11L).build());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.getBody() instanceof DependentServiceResponse);
        assertEquals(mockResponse.getData(), ((DependentServiceResponse) responseEntity.getBody()).getData());
    }

    @Test
    void testTemporaryUpload() {
        var mockResponse = createMockResponse();
        when(documentManagerRestClient.temporaryUpload(any())).thenReturn(mockResponse);
        var responseEntity = documentManagerServiceImpl.temporaryUpload(CommonRequestModel.builder().dependentData(new DocumentManagerUpdateFileEntitiesRequest()).build());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.getBody() instanceof DependentServiceResponse);
        assertEquals(mockResponse.getData(), ((DependentServiceResponse) responseEntity.getBody()).getData());
    }

    @Test
    void testList() {
        var mockResponse = createMockResponse();
        when(documentManagerRestClient.list(any(), any(), any())).thenReturn(mockResponse);
        var responseEntity = documentManagerServiceImpl.list(CommonRequestModel.builder().dependentData(new DocumentManagerUpdateFileEntitiesRequest()).build(), 1L, 1L);
        assertNotNull(responseEntity);
        assertTrue(responseEntity.getBody() instanceof DependentServiceResponse);
        assertEquals(mockResponse.getData(), ((DependentServiceResponse) responseEntity.getBody()).getData());
    }

    @Test
    void testFetchMultipleFilesWithTenant() {
        var mockResponse = new DocumentManagerListResponse<DocumentManagerEntityFileResponse>();
        when(documentManagerRestClient.multipleEntityFilesWithTenant(any())).thenReturn(mockResponse);
        var responseEntity = documentManagerServiceImpl.fetchMultipleFilesWithTenant(new DocumentManagerMultipleEntityFileRequest());
        assertNotNull(responseEntity);
        assertEquals(mockResponse, responseEntity);
    }

    @Test
    void testListDocTypes() {
        var mockResponse = createMockResponse();
        when(documentManagerRestClient.listDocTypes(any())).thenReturn(mockResponse);
        var responseEntity = documentManagerServiceImpl.listDocTypes(CommonRequestModel.builder().dependentData(new DocumentManagerUpdateFileEntitiesRequest()).build());
        assertNotNull(responseEntity);
        assertTrue(responseEntity.getBody() instanceof DependentServiceResponse);
        assertEquals(mockResponse.getData(), ((DependentServiceResponse) responseEntity.getBody()).getData());
    }


    @Test
    void testPushSystemGeneratedDocumentToDocMaster() {

        DocumentManagerResponse<DocumentManagerDataResponse> documentManagerResponse = new DocumentManagerResponse<>();
        documentManagerResponse.setCount(3L);
        documentManagerResponse.setData(new DocumentManagerDataResponse());
        when(documentManagerRestClient.temporaryFileUpload(Mockito.<DocumentManagerTempFileUploadRequest>any()))
                .thenReturn(documentManagerResponse);

        documentManagerServiceImpl.pushSystemGeneratedDocumentToDocMaster(new BASE64DecodedMultipartFile(new byte[]{'A', -1, 'A', -1, 'A', -1}), "test.txt", new DocUploadRequest());
        verify(documentManagerRestClient).temporaryFileUpload(isA(DocumentManagerTempFileUploadRequest.class));
    }

    @Test
    void testPushSystemGeneratedDocumentToDocMaster2() {

        DocumentManagerResponse<DocumentManagerDataResponse> documentManagerResponse = new DocumentManagerResponse<>();
        documentManagerResponse.setCount(3L);
        documentManagerResponse.setData(new DocumentManagerDataResponse());
        documentManagerResponse.setSuccess(Boolean.FALSE);
        when(documentManagerRestClient.temporaryFileUpload(Mockito.<DocumentManagerTempFileUploadRequest>any()))
                .thenReturn(documentManagerResponse);

        documentManagerServiceImpl.pushSystemGeneratedDocumentToDocMaster(new BASE64DecodedMultipartFile(new byte[]{'A', -1, 'A', -1, 'A', -1}), "test.txt", new DocUploadRequest());
        verify(documentManagerRestClient).temporaryFileUpload(isA(DocumentManagerTempFileUploadRequest.class));
    }





    private DocumentManagerResponse<T> createMockResponse() {
        var mockResponse = new DocumentManagerResponse<T>();
        mockResponse.setData(new T());
        mockResponse.setPageNo(1);
        mockResponse.setPageSize(2);
        mockResponse.setCount(2l);

        return mockResponse;
    }
}
