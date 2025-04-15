package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.document.util.BASE64DecodedMultipartFile;
import com.dpw.runner.shipment.services.dto.patchrequest.ShipmentSettingsPatchRequest;
import com.dpw.runner.shipment.services.dto.request.ProductSequenceConfigRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentSettingRequest;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentSettingsService;
import com.dpw.runner.shipment.services.syncing.Entity.ShipmentSettingsSyncRequest;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsReverseSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsSync;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ContextConfiguration(classes = {MasterDataController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentSettingsControllerTest {

    @Mock
    private IShipmentSettingsService shipmentSettingsService;
    @Mock
    private IShipmentSettingsSync shipmentSettingsSync;
    @Mock
    IShipmentSettingsReverseSync shipmentSettingsReverseSync;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private ShipmentSettingsController shipmentSettingsController;

    @Test
    void create() {
        // Mock
        when(shipmentSettingsService.create(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentSettingsController.create(ShipmentSettingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void create2() {
        // Mock
        when(shipmentSettingsService.create(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentSettingsController.create(ShipmentSettingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void create3() {
        // Mock
        when(shipmentSettingsService.create(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentSettingsController.create(ShipmentSettingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByTenantId() {
        // Mock
        when(shipmentSettingsService.retrieveByTenantId(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentSettingsController.retrieveByTenantId(Optional.of(111L), false);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByTenantId2() {
        // Mock
        when(shipmentSettingsService.retrieveByTenantId(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentSettingsController.retrieveByTenantId(Optional.of(111L), false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByTenantId3() {
        // Mock
        when(shipmentSettingsService.retrieveByTenantId(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentSettingsController.retrieveByTenantId(Optional.of(111L), false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void syncShipmentToService() {
        // Mock
        when(shipmentSettingsReverseSync.reverseSync(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentSettingsController.syncShipmentToService(new ShipmentSettingsSyncRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void syncShipmentToService2() {
        // Mock
        when(shipmentSettingsReverseSync.reverseSync(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentSettingsController.syncShipmentToService(new ShipmentSettingsSyncRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void syncShipmentToService3() {
        // Mock
        when(shipmentSettingsReverseSync.reverseSync(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentSettingsController.syncShipmentToService(new ShipmentSettingsSyncRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getCustomProductSequence() throws RunnerException {
        // Mock
        when(shipmentSettingsSync.syncProductSequence(any(), any())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertValue(any(), eq(ProductSequenceConfig.class))).thenReturn(new ProductSequenceConfig());
        // Test
        var responseEntity = shipmentSettingsController.getCustomProductSequence(ProductSequenceConfigRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getCustomProductSequence2() throws RunnerException {
        // Mock
        when(shipmentSettingsSync.syncProductSequence(any(), any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(ProductSequenceConfig.class))).thenReturn(new ProductSequenceConfig());
        // Test
        var responseEntity = shipmentSettingsController.getCustomProductSequence(ProductSequenceConfigRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getCustomProductSequence3() throws RunnerException {
        // Mock
        when(shipmentSettingsSync.syncProductSequence(any(), any())).thenThrow(new RuntimeException("RuntimeException"));
        when(jsonHelper.convertValue(any(), eq(ProductSequenceConfig.class))).thenReturn(new ProductSequenceConfig());
        // Test
        var responseEntity = shipmentSettingsController.getCustomProductSequence(ProductSequenceConfigRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getCustomShipment() {
        // Mock
        when(shipmentSettingsSync.sync(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertValue(any(), eq(ShipmentSettingsDetails.class))).thenReturn(new ShipmentSettingsDetails());
        // Test
        var responseEntity = shipmentSettingsController.getCustomShipment(ShipmentSettingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getCustomShipment2() {
        // Mock
        when(shipmentSettingsSync.sync(any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(ShipmentSettingsDetails.class))).thenReturn(new ShipmentSettingsDetails());
        // Test
        var responseEntity = shipmentSettingsController.getCustomShipment(ShipmentSettingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getCustomShipment3() {
        // Mock
        when(shipmentSettingsSync.sync(any())).thenThrow(new RuntimeException("RuntimeException"));
        when(jsonHelper.convertValue(any(), eq(ShipmentSettingsDetails.class))).thenReturn(new ShipmentSettingsDetails());
        // Test
        var responseEntity = shipmentSettingsController.getCustomShipment(ShipmentSettingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void completeSettingsUpdateCreateV1() throws RunnerException {
        // Mock
        when(shipmentSettingsService.completeSettingsUpdateCreateV1(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentSettingsController.completeSettingsUpdateCreateV1(ShipmentSettingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void completeSettingsUpdateCreateV12() throws RunnerException {
        // Mock
        when(shipmentSettingsService.completeSettingsUpdateCreateV1(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentSettingsController.completeSettingsUpdateCreateV1(ShipmentSettingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void completeSettingsUpdateCreateV13() throws RunnerException {
        // Mock
        when(shipmentSettingsService.completeSettingsUpdateCreateV1(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentSettingsController.completeSettingsUpdateCreateV1(ShipmentSettingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void completeUpdate() throws RunnerException {
        // Mock
        when(shipmentSettingsService.completeUpdate(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentSettingsController.completeUpdate(ShipmentSettingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void completeUpdate2() throws RunnerException {
        // Mock
        when(shipmentSettingsService.completeUpdate(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentSettingsController.completeUpdate(ShipmentSettingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void completeUpdate3() throws RunnerException {
        // Mock
        when(shipmentSettingsService.completeUpdate(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentSettingsController.completeUpdate(ShipmentSettingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update() throws RunnerException {
        // Mock
        when(shipmentSettingsService.update(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentSettingsController.update(ShipmentSettingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update2() throws RunnerException {
        // Mock
        when(shipmentSettingsService.update(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentSettingsController.update(ShipmentSettingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update3() throws RunnerException {
        // Mock
        when(shipmentSettingsService.update(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentSettingsController.update(ShipmentSettingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveById() {
        // Mock
        when(shipmentSettingsService.retrieveById(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentSettingsController.retrieveById(Optional.of(11L), Optional.of(UUID.randomUUID().toString()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list() {
        // Mock
        when(shipmentSettingsService.list(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentSettingsController.list(ListCommonRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void delete() {
        // Mock
        when(shipmentSettingsService.delete(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentSettingsController.delete(Optional.of(123L), Optional.empty());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void uploadTemplate() {
        // Mock
        when(shipmentSettingsService.uploadTemplate(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentSettingsController.uploadTemplate(new BASE64DecodedMultipartFile(StringUtility.getRandomString(10).getBytes()), "previousField");
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void downloadTemplate() {
        // Mock
        when(shipmentSettingsService.downloadTemplate(anyString())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentSettingsController.downloadTemplate("previousField");
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listCoLoadStationTenantIds() {
        // Mock
        when(shipmentSettingsService.listCoLoadStationTenantIds()).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentSettingsController.listCoLoadStationTenantIds();
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listCoLoadStationTenantIds2() {
        // Mock
        when(shipmentSettingsService.listCoLoadStationTenantIds()).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentSettingsController.listCoLoadStationTenantIds();
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listCoLoadStationTenantIds3() {
        // Mock
        when(shipmentSettingsService.listCoLoadStationTenantIds()).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentSettingsController.listCoLoadStationTenantIds();
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listHubTenantIds() {
        // Mock
        when(shipmentSettingsService.listHubTenantIds()).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentSettingsController.listHubTenantIds();
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listHubTenantIds2() {
        // Mock
        when(shipmentSettingsService.listHubTenantIds()).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentSettingsController.listHubTenantIds();
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listHubTenantIds3() {
        // Mock
        when(shipmentSettingsService.listHubTenantIds()).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentSettingsController.listHubTenantIds();
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void hideManifest() throws RunnerException {
        // Mock
        when(shipmentSettingsService.hideManifest(true)).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentSettingsController.updateHideManifest(true);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void hideManifest2() throws RunnerException {
        // Mock
        when(shipmentSettingsService.hideManifest(true)).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentSettingsController.updateHideManifest(true);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void hideManifest3() throws RunnerException {
        // Mock
        when(shipmentSettingsService.hideManifest(true)).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentSettingsController.updateHideManifest(true);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdate() throws RunnerException {
        // Mock
        when(shipmentSettingsService.partialUpdate(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentSettingsController.partialUpdate(ShipmentSettingsPatchRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdate2() throws RunnerException {
        // Mock
        when(shipmentSettingsService.partialUpdate(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentSettingsController.partialUpdate(ShipmentSettingsPatchRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdate3() throws RunnerException {
        // Mock
        when(shipmentSettingsService.partialUpdate(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentSettingsController.partialUpdate(ShipmentSettingsPatchRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }
}
