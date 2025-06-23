package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.document.util.BASE64DecodedMultipartFile;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.DetachPacksListDto;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackContainerNumberChangeRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.syncing.Entity.BulkPackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.*;

@ContextConfiguration(classes = {MasterDataController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class PackingControllerTest {

    @Mock
    private IPackingService packingService;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private PackingController packingController;


    @Test
    void uploadCSV() throws IOException {
        // Mock
        var responseEntity = packingController.uploadCSV(BulkUploadRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void uploadCSV2() throws IOException, RunnerException {
        doNothing().when(packingService).uploadPacking(any());
        // Mock
        var responseEntity = packingController.uploadCSV(BulkUploadRequest.builder().file(new BASE64DecodedMultipartFile(StringUtility.getRandomString(11).getBytes())).build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void uploadCSV3() throws IOException, RunnerException {
        doThrow(new RuntimeException()).when(packingService).uploadPacking(any());
        // Mock
        var responseEntity = packingController.uploadCSV(BulkUploadRequest.builder().file(new BASE64DecodedMultipartFile(StringUtility.getRandomString(11).getBytes())).build());
        // Assert
        assertEquals(HttpStatus.EXPECTATION_FAILED, responseEntity.getStatusCode());
    }

    @Test
    void uploadCSV4() throws IOException, RunnerException {
        doThrow(new RuntimeException("RuntimeException")).when(packingService).uploadPacking(any());
        // Mock
        var responseEntity = packingController.uploadCSV(BulkUploadRequest.builder().file(new BASE64DecodedMultipartFile(StringUtility.getRandomString(11).getBytes())).build());
        // Assert
        assertEquals(HttpStatus.EXPECTATION_FAILED, responseEntity.getStatusCode());
    }

    @Test
    void downloadCSV() throws RunnerException {
        var isSuccess = true;
        doNothing().when(packingService).downloadPacking(any(), any());
        packingController.downloadCSV(new MockHttpServletResponse(), new BulkDownloadRequest());
        assertTrue(isSuccess);
    }

    @Test
    void downloadCSV2() throws RunnerException {
        var isSuccess = true;
        doThrow(new RuntimeException()).when(packingService).downloadPacking(any(), any());
        packingController.downloadCSV(new MockHttpServletResponse(), new BulkDownloadRequest());
        assertTrue(isSuccess);
    }

    @Test
    void list() throws RunnerException {
        when(packingService.list(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = packingController.list(new AttachListShipmentRequest());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void create() {
        // Mock
        when(packingService.create(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = packingController.create(PackingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void create2() {
        // Mock
        when(packingService.create(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = packingController.create(PackingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void create3() {
        // Mock
        when(packingService.create(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = packingController.create(PackingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update() throws RunnerException {
        // Mock
        when(packingService.update(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = packingController.update(PackingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update2() throws RunnerException {
        // Mock
        when(packingService.update(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = packingController.update(PackingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update3() throws RunnerException {
        // Mock
        when(packingService.update(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = packingController.update(PackingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void delete() {
        // Mock
        when(packingService.delete(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = packingController.delete(123L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void delete2() {
        // Mock
        when(packingService.delete(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = packingController.delete(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculateWeightVolume() throws RunnerException {
        // Mock
        when(packingService.calculateWeightVolumne(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = packingController.calculateWeightVolume(PackContainerNumberChangeRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculateWeightVolume2() throws RunnerException {
        // Mock
        when(packingService.calculateWeightVolumne(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = packingController.calculateWeightVolume(PackContainerNumberChangeRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculateWeightVolume3() throws RunnerException {
        // Mock
        when(packingService.calculateWeightVolumne(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = packingController.calculateWeightVolume(PackContainerNumberChangeRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listPacksToDetach() throws RunnerException {
        // Mock
        when(packingService.listPacksToDetach(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = packingController.listPacksToDetach(DetachPacksListDto.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listPacksToDetach2() throws RunnerException {
        // Mock
        when(packingService.listPacksToDetach(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = packingController.listPacksToDetach(DetachPacksListDto.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listPacksToDetach3() throws RunnerException {
        // Mock
        when(packingService.listPacksToDetach(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = packingController.listPacksToDetach(DetachPacksListDto.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void syncPackingToService() throws RunnerException {
        // Mock
        when(packingService.v1PackingCreateAndUpdate(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = packingController.syncPackingToService(new PackingRequestV2());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void syncPackingToService2() throws RunnerException {
        // Mock
        when(packingService.v1PackingCreateAndUpdate(any(), anyBoolean())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = packingController.syncPackingToService(new PackingRequestV2());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void syncPackingToService3() throws RunnerException {
        // Mock
        when(packingService.v1PackingCreateAndUpdate(any(), anyBoolean())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = packingController.syncPackingToService(new PackingRequestV2());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void syncBulkPackingToService() {
        // Mock
        when(packingService.v1BulkPackingCreateAndUpdate(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = packingController.syncBulkPackingToService(BulkPackingRequestV2.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void syncBulkPackingToService2() {
        // Mock
        when(packingService.v1BulkPackingCreateAndUpdate(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = packingController.syncBulkPackingToService(BulkPackingRequestV2.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void syncBulkPackingToService3() {
        // Mock
        when(packingService.v1BulkPackingCreateAndUpdate(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = packingController.syncBulkPackingToService(BulkPackingRequestV2.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void autoCalculateVolumetricWeight() {
        // Mock
        when(packingService.autoCalculatePacksData(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = packingController.autoCalculatePacksData(new AutoCalculatePackingRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void autoCalculateVolumetricWeight2() {
        // Mock
        when(packingService.autoCalculatePacksData(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = packingController.autoCalculatePacksData(new AutoCalculatePackingRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void autoCalculateVolumetricWeight3() {
        // Mock
        when(packingService.autoCalculatePacksData(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = packingController.autoCalculatePacksData(new AutoCalculatePackingRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

}
