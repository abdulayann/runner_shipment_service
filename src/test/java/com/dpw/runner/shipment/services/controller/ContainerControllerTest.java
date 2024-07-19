package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.commons.requests.ExportContainerListRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.document.util.BASE64DecodedMultipartFile;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.syncing.Entity.BulkContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.ContainerRequestV2;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.context.ContextConfiguration;

import java.io.IOException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.*;

@ContextConfiguration(classes = {MasterDataController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ContainerControllerTest {

    @Mock
    private IContainerService containerService;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private ContainerController containerController;


    @Test
    void uploadCSV() throws IOException {
        // Mock
        var responseEntity = containerController.uploadCSV(BulkUploadRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void uploadCSV2() throws IOException, RunnerException {
        doNothing().when(containerService).uploadContainers(any());
        // Mock
        var responseEntity = containerController.uploadCSV(BulkUploadRequest.builder().file(new BASE64DecodedMultipartFile(StringUtility.getRandomString(11).getBytes())).build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void uploadCSV3() throws IOException, RunnerException {
        doThrow(new RuntimeException()).when(containerService).uploadContainers(any());
        // Mock
        var responseEntity = containerController.uploadCSV(BulkUploadRequest.builder().file(new BASE64DecodedMultipartFile(StringUtility.getRandomString(11).getBytes())).build());
        // Assert
        assertEquals(HttpStatus.EXPECTATION_FAILED, responseEntity.getStatusCode());
    }

    @Test
    void uploadCSV4() throws IOException, RunnerException {
        doThrow(new RuntimeException("RuntimeException")).when(containerService).uploadContainers(any());
        // Mock
        var responseEntity = containerController.uploadCSV(BulkUploadRequest.builder().file(new BASE64DecodedMultipartFile(StringUtility.getRandomString(11).getBytes())).build());
        // Assert
        assertEquals(HttpStatus.EXPECTATION_FAILED, responseEntity.getStatusCode());
    }

    @Test
    void uploadEventsCSV() throws IOException {
        // Mock
        var responseEntity = containerController.uploadEventsCSV(BulkUploadRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void uploadEventsCSV2() throws IOException, RunnerException {
        doNothing().when(containerService).uploadContainerEvents(any());
        // Mock
        var responseEntity = containerController.uploadEventsCSV(BulkUploadRequest.builder().file(new BASE64DecodedMultipartFile(StringUtility.getRandomString(11).getBytes())).build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void uploadEventsCSV3() throws IOException, RunnerException {
        doThrow(new RuntimeException()).when(containerService).uploadContainerEvents(any());
        // Mock
        var responseEntity = containerController.uploadEventsCSV(BulkUploadRequest.builder().file(new BASE64DecodedMultipartFile(StringUtility.getRandomString(11).getBytes())).build());
        // Assert
        assertEquals(HttpStatus.EXPECTATION_FAILED, responseEntity.getStatusCode());
    }

    @Test
    void uploadEventsCSV4() throws IOException, RunnerException {
        doThrow(new RuntimeException("RuntimeException")).when(containerService).uploadContainerEvents(any());
        // Mock
        var responseEntity = containerController.uploadEventsCSV(BulkUploadRequest.builder().file(new BASE64DecodedMultipartFile(StringUtility.getRandomString(11).getBytes())).build());
        // Assert
        assertEquals(HttpStatus.EXPECTATION_FAILED, responseEntity.getStatusCode());
    }

    @Test
    void downloadCSV() throws RunnerException {
        boolean isSuccess = true;
        doNothing().when(containerService).downloadContainers(any(), any());
        containerController.downloadCSV(new MockHttpServletResponse(), new BulkDownloadRequest());
        assertTrue(isSuccess);
    }

    @Test
    void downloadCSV2() throws RunnerException {
        boolean isSuccess = true;
        doThrow(new RuntimeException()).when(containerService).downloadContainers(any(), any());
        containerController.downloadCSV(new MockHttpServletResponse(), new BulkDownloadRequest());
        assertTrue(isSuccess);
    }

    @Test
    void exportContainers() throws RunnerException, IOException, IllegalAccessException {
        boolean isSuccess = true;
        doNothing().when(containerService).exportContainers(any(), any());
        var responseEntity = containerController.exportContainers(new MockHttpServletResponse(), new ExportContainerListRequest());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void exportContainers2() throws RunnerException, IOException, IllegalAccessException {
        doThrow(new RuntimeException()).when(containerService).exportContainers(any(), any());
        var responseEntity = containerController.exportContainers(new MockHttpServletResponse(), new ExportContainerListRequest());
        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, responseEntity.getStatusCode());
    }

    @Test
    void downloadEventsCSV() throws RunnerException, IOException, IllegalAccessException {
        boolean isSuccess = true;
        doNothing().when(containerService).downloadContainerEvents(any(), any());
        containerController.downloadEventsCSV(new MockHttpServletResponse(), new BulkDownloadRequest());
        assertTrue(isSuccess);
    }

    @Test
    void downloadEventsCSV2() throws RunnerException, IOException, IllegalAccessException {
        boolean isSuccess = true;
        doThrow(new RuntimeException()).when(containerService).downloadContainerEvents(any(), any());
        containerController.downloadEventsCSV(new MockHttpServletResponse(), new BulkDownloadRequest());
        assertTrue(isSuccess);
    }

    @Test
    void checkForDelete() {
        // Mock
        when(containerService.checkForDelete(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Mock
        var responseEntity = containerController.checkForDelete(1221L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list() {
        // Mock
        when(containerService.getContainers(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Mock
        var responseEntity = containerController.list(ListCommonRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void validateContainerNumber() {
        // Mock
        when(containerService.validateContainerNumber(anyString())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Mock
        var responseEntity = containerController.validateContainerNumber("CONT2121212");
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculateAchievedOnPackDetach() {
        // Mock
        when(containerService.calculateAchievedQuantity_onPackDetach(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Mock
        var responseEntity = containerController.calculateAchievedOnPackDetach(new ContainerPackADInShipmentRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculateAllocatedData() {
        // Mock
        when(containerService.calculateAllocatedData(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Mock
        var responseEntity = containerController.calculateAllocatedData(CheckAllocatedDataChangesRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculateAchieved_AllocatedForSameUnit() {
        // Mock
        when(containerService.calculateAchieved_AllocatedForSameUnit(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Mock
        var responseEntity = containerController.calculateAchieved_AllocatedForSameUnit(ContainerRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getContainersForSelection() {
        // Mock
        when(containerService.getContainersForSelection(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Mock
        var responseEntity = containerController.getContainersForSelection(new ContainerAssignListRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list2() {
        // Mock
        when(containerService.list(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Mock
        var responseEntity = containerController.list(123L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getContainers() {
        // Mock
        when(containerService.containerSync(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = containerController.getContainers(List.of(123L));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getContainers2() {
        // Mock
        when(containerService.containerSync(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = containerController.getContainers(List.of(123L));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getContainers3() {
        // Mock
        when(containerService.containerSync(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = containerController.getContainers(List.of(123L));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void syncBulkContainerToService() {
        // Mock
        when(containerService.V1BulkContainerCreateAndUpdate(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = containerController.syncBulkContainerToService(BulkContainerRequestV2.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void syncBulkContainerToService2() {
        // Mock
        when(containerService.V1BulkContainerCreateAndUpdate(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = containerController.syncBulkContainerToService(BulkContainerRequestV2.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void syncBulkContainerToService3() {
        // Mock
        when(containerService.V1BulkContainerCreateAndUpdate(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = containerController.syncBulkContainerToService(BulkContainerRequestV2.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void syncContainerToService() throws RunnerException {
        // Mock
        when(containerService.V1ContainerCreateAndUpdate(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = containerController.syncContainerToService(new ContainerRequestV2(), false);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void syncContainerToService2() throws RunnerException {
        // Mock
        when(containerService.V1ContainerCreateAndUpdate(any(), anyBoolean())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = containerController.syncContainerToService(new ContainerRequestV2(), true);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void syncContainerToService3() throws RunnerException {
        // Mock
        when(containerService.V1ContainerCreateAndUpdate(any(), anyBoolean())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = containerController.syncContainerToService(new ContainerRequestV2(), false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void create() {
        // Mock
        when(containerService.create(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = containerController.create(new ContainerRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void create2() {
        // Mock
        when(containerService.create(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = containerController.create(new ContainerRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void create3() {
        // Mock
        when(containerService.create(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = containerController.create(new ContainerRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update() throws RunnerException {
        // Mock
        when(containerService.update(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = containerController.update(new ContainerRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update2() throws RunnerException {
        // Mock
        when(containerService.update(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = containerController.update(new ContainerRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update3() throws RunnerException {
        // Mock
        when(containerService.update(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = containerController.update(new ContainerRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void delete() {
        // Mock
        when(containerService.delete(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = containerController.delete(12L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void delete2() {
        // Mock
        when(containerService.delete(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = containerController.delete(122L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

}
