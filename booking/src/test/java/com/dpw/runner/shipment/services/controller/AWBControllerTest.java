package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.awb.CustomAwbRetrieveRequest;
import com.dpw.runner.shipment.services.dto.request.awb.GenerateAwbPaymentInfoRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAwbService;
import com.dpw.runner.shipment.services.syncing.Entity.AwbRequestV2;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ContextConfiguration(classes = {MasterDataController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class AWBControllerTest {

    @Mock
    private IAwbService awbService;
    @InjectMocks
    private AwbController awbController;

    @Test
    void list() {
        // Mock
        when(awbService.list(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.list(new FetchAwbListRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list1() {
        // Mock
        when(awbService.list(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbController.list(new FetchAwbListRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createAwb() {
        // Mock
        when(awbService.createAwb(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.createAwb(new CreateAwbRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createAwb2() {
        // Mock
        when(awbService.createAwb(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbController.createAwb(new CreateAwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createAwb3() {
        // Mock
        when(awbService.createAwb(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbController.createAwb(new CreateAwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateAwbDetails() {
        // Mock
        when(awbService.updateAwb(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.updateAwbDetails(new AwbRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateAwbDetails2() {
        // Mock
        when(awbService.updateAwb(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbController.updateAwbDetails(new AwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateAwbDetails3() {
        // Mock
        when(awbService.updateAwb(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbController.updateAwbDetails(new AwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveById() {
        // Mock
        when(awbService.retrieveById(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.retrieveById(123L, List.of());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByIssuingAgentName() {
        // Mock
        when(awbService.customAwbRetrieve(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.retrieveByIssuingAgentName(new CustomAwbRetrieveRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createMawb() {
        // Mock
        when(awbService.createMawb(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.createMawb(new CreateAwbRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createMawb2() {
        // Mock
        when(awbService.createMawb(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbController.createMawb(new CreateAwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createMawb3() {
        // Mock
        when(awbService.createMawb(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbController.createMawb(new CreateAwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateGoodsAndPacksForMawb() {
        // Mock
        when(awbService.updateGoodsAndPacksForMawb(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.updateGoodsAndPacksForMawb(new CreateAwbRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateGoodsAndPacksForMawb2() {
        // Mock
        when(awbService.updateGoodsAndPacksForMawb(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbController.updateGoodsAndPacksForMawb(new CreateAwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateGoodsAndPacksForMawb3() {
        // Mock
        when(awbService.updateGoodsAndPacksForMawb(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbController.updateGoodsAndPacksForMawb(new CreateAwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createV1Awb() {
        // Mock
        when(awbService.createV1Awb(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.createV1Awb(new AwbRequestV2(), false);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createV1Awb2() {
        // Mock
        when(awbService.createV1Awb(any(), anyBoolean())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbController.createV1Awb(new AwbRequestV2(), false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createV1Awb3() {
        // Mock
        when(awbService.createV1Awb(any(), anyBoolean())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbController.createV1Awb(new AwbRequestV2(), false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }


    @Test
    void reset() throws RunnerException {
        // Mock
        when(awbService.reset(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.reset(new ResetAwbRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void reset2() throws RunnerException {
        // Mock
        when(awbService.reset(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbController.reset(new ResetAwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void reset3() throws RunnerException {
        // Mock
        when(awbService.reset(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbController.reset(new ResetAwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void partialAutoUpdateAwb() throws RunnerException {
        // Mock
        when(awbService.partialAutoUpdateAwb(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.partialAutoUpdateAwb(new CreateAwbRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void partialAutoUpdateAwb2() throws RunnerException {
        // Mock
        when(awbService.partialAutoUpdateAwb(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbController.partialAutoUpdateAwb(new CreateAwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void partialAutoUpdateAwb3() throws RunnerException {
        // Mock
        when(awbService.partialAutoUpdateAwb(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbController.partialAutoUpdateAwb(new CreateAwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void partialAutoUpdateMawb() throws RunnerException {
        // Mock
        when(awbService.partialAutoUpdateMawb(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.partialAutoUpdateMawb(new CreateAwbRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void partialAutoUpdateMawb2() throws RunnerException {
        // Mock
        when(awbService.partialAutoUpdateMawb(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbController.partialAutoUpdateMawb(new CreateAwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void partialAutoUpdateMawb3() throws RunnerException {
        // Mock
        when(awbService.partialAutoUpdateMawb(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbController.partialAutoUpdateMawb(new CreateAwbRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void generateAwbPaymentIndo() throws RunnerException {
        // Mock
        when(awbService.generateAwbPaymentInfo(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.generateAwbPaymentIndo(new GenerateAwbPaymentInfoRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void generateAwbPaymentIndo2() throws RunnerException {
        // Mock
        when(awbService.generateAwbPaymentInfo(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbController.generateAwbPaymentIndo(new GenerateAwbPaymentInfoRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void generateAwbPaymentIndo3() throws RunnerException {
        // Mock
        when(awbService.generateAwbPaymentInfo(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbController.generateAwbPaymentIndo(new GenerateAwbPaymentInfoRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getFnmStatusMessage() {
        // Mock
        when(awbService.getFnmStatusMessage(any(), any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.getFnmStatusMessage(Optional.of(11L), Optional.empty());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getFnmStatusMessage2() {
        // Mock
        when(awbService.getFnmStatusMessage(any(), any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbController.getFnmStatusMessage(Optional.of(11L), Optional.empty());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getFnmStatusMessage3() {
        // Mock
        when(awbService.getFnmStatusMessage(any(), any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbController.getFnmStatusMessage(Optional.of(11L), Optional.empty());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void validateIataAgent() {
        // Mock
        when(awbService.validateIataAgent(any(), any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.validateIataAgent(false, Optional.empty());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void validateIataAgent2() {
        // Mock
        when(awbService.validateIataAgent(any(), any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbController.validateIataAgent(false, Optional.empty());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void validateIataAgent3() {
        // Mock
        when(awbService.validateIataAgent(any(), any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbController.validateIataAgent(false, Optional.empty());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getChargeTypeMasterData() throws RunnerException {
        // Mock
        when(awbService.getChargeTypeMasterData(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.getChargeTypeMasterData(123L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getChargeTypeMasterData2() throws RunnerException {
        // Mock
        when(awbService.getChargeTypeMasterData(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbController.getChargeTypeMasterData(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getChargeTypeMasterData3() throws RunnerException {
        // Mock
        when(awbService.getChargeTypeMasterData(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbController.getChargeTypeMasterData(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void dimsText() throws RunnerException {
        // Mock
        when(awbService.generateUpdatedNatureAndQuantGoodsField(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.dimsText(new GenerateAwbPaymentInfoRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void dimsText2() throws RunnerException {
        // Mock
        when(awbService.generateUpdatedNatureAndQuantGoodsField(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbController.dimsText(new GenerateAwbPaymentInfoRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void dimsText3() throws RunnerException {
        // Mock
        when(awbService.generateUpdatedNatureAndQuantGoodsField(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbController.dimsText(new GenerateAwbPaymentInfoRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByAwbByMawb() {
        // Mock
        when(awbService.retrieveByAwbByMawb(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.retrieveByAwbByMawb(123L, List.of());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getAllMasterData() {
        // Mock
        when(awbService.getAllMasterData(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.getAllMasterData(11L, null);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getAllMasterData2() {
        // Mock
        when(awbService.getAllMasterData(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.getAllMasterData(null, 123L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getAllMasterData3() {
        // Mock
        // Test
        var responseEntity = awbController.getAllMasterData(null, null);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getAllMasterData4() {
        // Mock
        when(awbService.getAllMasterData(any(), anyBoolean())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbController.getAllMasterData(11L, null);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getAllMasterData5() {
        // Mock
        when(awbService.getAllMasterData(any(), anyBoolean())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = awbController.getAllMasterData(11L, null);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getFetchIataRates() throws RunnerException {
        // Mock
        when(awbService.getFetchIataRates(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = awbController.getFetchIataRates(new IataFetchRateRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getFetchIataRates2() throws RunnerException {
        // Mock
        when(awbService.getFetchIataRates(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = awbController.getFetchIataRates(new IataFetchRateRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }




}
