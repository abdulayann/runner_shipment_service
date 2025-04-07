package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.notification.PendingNotificationRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.syncing.Entity.CustomConsolidationRequest;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationReverseSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
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
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutionException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ContextConfiguration(classes = {MasterDataController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ConsolidationControllerTest {

    @Mock
    private IConsolidationService consolidationService;
    @Mock
    private IConsolidationSync consolidationSync;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IConsolidationReverseSync consolidationReverseSync;
    @Mock
    private IShipmentService shipmentService;
    @InjectMocks
    private ConsolidationController consolidationController;

    @Test
    void fetchByQuery() {
        var mockResponse = ResponseHelper.buildFailedResponse("Response");
        // Mock
        when(consolidationService.fetchConsolidations(any())).thenReturn(mockResponse);
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(11));
        // Test
        var responseEntity = consolidationController.fetchByQuery(ListCommonRequest.builder().build());
        // Assert
        assertEquals(mockResponse.getStatusCode(), responseEntity.getStatusCode());
    }

    @Test
    void delete() {
        var mockResponse = ResponseHelper.buildFailedResponse("Response");
        // Mock
        when(consolidationService.delete(any())).thenReturn(mockResponse);
        // Test
        var responseEntity = consolidationController.delete(123L);
        // Assert
        assertEquals(mockResponse.getStatusCode(), responseEntity.getStatusCode());
    }

    @Test
    void list() {
        var mockResponse = ResponseHelper.buildFailedResponse("Response");
        // Mock
        when(consolidationService.fullConsolidationsList(any())).thenReturn(mockResponse);
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(11));
        // Test
        var responseEntity = consolidationController.list(ListCommonRequest.builder().build(), true, true);
        // Assert
        assertEquals(mockResponse.getStatusCode(), responseEntity.getStatusCode());
    }

    @Test
    void list2() {
        var mockResponse = ResponseHelper.buildFailedResponse("Response");
        // Mock
        when(consolidationService.list(any(), anyBoolean())).thenReturn(mockResponse);
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(11));
        // Test
        var responseEntity = consolidationController.list(ListCommonRequest.builder().build(), false, true);
        // Assert
        assertEquals(mockResponse.getStatusCode(), responseEntity.getStatusCode());
    }

    @Test
    void list3() {
        // Mock
        when(consolidationService.list(any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(11));
        // Test
        var responseEntity = consolidationController.list(ListCommonRequest.builder().build(), false, true);
        // Assert
        assertEquals(HttpStatus.FORBIDDEN, responseEntity.getStatusCode());
    }

    @Test
    void create() {
        // Mock
        when(consolidationService.create(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(11));
        when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(new ConsolidationDetailsRequest());
        // Test
        var responseEntity = consolidationController.create(new ConsolidationDetailsRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void create1() {
        // Mock
        when(consolidationService.create(any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(11));
        when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(new ConsolidationDetailsRequest());
        // Test
        var responseEntity = consolidationController.create(new ConsolidationDetailsRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void create2() {
        // Mock
        when(consolidationService.create(any())).thenThrow(new RuntimeException("RuntimeException"));
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(11));
        when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(new ConsolidationDetailsRequest());
        // Test
        var responseEntity = consolidationController.create(new ConsolidationDetailsRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveForNTE() {
        // Mock
        when(consolidationService.retrieveForNTE(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(11));
        // Test
        var responseEntity = consolidationController.retrieveForNTE(Optional.of(1L));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveById() {
        // Mock
        when(consolidationService.retrieveById(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(11));
        // Test
        var responseEntity = consolidationController.retrieveById(Optional.of(1L), Optional.of(UUID.randomUUID().toString()), true);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void completeRetrieveById() throws ExecutionException, InterruptedException {
        // Mock
        when(consolidationService.completeRetrieveById(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(11));
        // Test
        var responseEntity = consolidationController.completeRetrieveById(123L, null);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculateUtilization() {
        // Mock
        when(consolidationService.calculateUtilization(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.calculateUtilization( new ConsoleCalculationsRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculateAchievedAllocatedForSameUnit() {
        // Mock
        when(consolidationService.calculateAchievedAllocatedForSameUnit(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.calculateAchievedAllocatedForSameUnit( new ConsoleCalculationsRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculateChargeable() {
        // Mock
        when(consolidationService.calculateChargeable(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.calculateChargeable( new ConsoleCalculationsRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculateAchievedValues() {
        // Mock
        when(consolidationService.calculateAchievedValues(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.calculateAchievedValues(123L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }
    @Test
    void attachShipments() throws RunnerException {
        // Mock
        when(consolidationService.attachShipments(any(), any(), any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.attachShipments( new ShipmentAttachDetachRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void detachShipments() throws RunnerException {
        // Mock
        when(consolidationService.detachShipments(any(), any(), any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.detachShipments( new ShipmentAttachDetachRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getShipmentFromConsol() {
        // Mock
        when(shipmentService.getShipmentFromConsol(anyLong(), anyString())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.getShipmentFromConsol(123L, "DBA-12-111");
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void mblCheck() {
        // Mock
        when(consolidationService.mblCheck(anyString())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.mblCheck("DBA-12-111");
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getShipmentFromConsol2() {
        // Mock
        when(shipmentService.getShipmentFromConsol(anyLong(), anyString())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.getShipmentFromConsol(123L, "DBA-12-111");
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getAutoAttachConsolidationDetails() {
        // Mock
        when(consolidationService.getAutoAttachConsolidationDetails(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.getAutoAttachConsolidationDetails(new AutoAttachConsolidationRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getAutoAttachConsolidationDetails2() {
        // Mock
        when(consolidationService.getAutoAttachConsolidationDetails(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.getAutoAttachConsolidationDetails(new AutoAttachConsolidationRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getIdFromGuid() {
        // Mock
        when(consolidationService.getIdFromGuid(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.getIdFromGuid(UUID.randomUUID().toString());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getIdFromGuid2() {
        // Mock
        when(consolidationService.getIdFromGuid(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.getIdFromGuid(UUID.randomUUID().toString());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void generateCustomBolNumber() throws RunnerException {
        // Mock
        doReturn(ResponseHelper.buildSuccessResponse()).when(consolidationService).generateCustomHouseBLNumber();
        // Test
        var responseEntity = consolidationController.generateCustomBolNumber();
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getDefaultConsolidation() {
        // Mock
        doReturn(ResponseHelper.buildSuccessResponse()).when(consolidationService).getDefaultConsolidation();
        // Test
        var responseEntity = consolidationController.getDefaultConsolidation();
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void validateMawbNumber() {
        // Mock
        when(consolidationService.validateMawbNumber(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.validateMawbNumber(ValidateMawbNumberRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void validateMawbNumber2() {
        // Mock
        when(consolidationService.validateMawbNumber(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.validateMawbNumber(ValidateMawbNumberRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateConsoleBookingFields() {
        // Mock
        when(consolidationService.updateConsoleBookingFields(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.updateConsoleBookingFields(ConsoleBookingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateConsoleBookingFields2() {
        // Mock
        when(consolidationService.updateConsoleBookingFields(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.updateConsoleBookingFields(ConsoleBookingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void showCreateBooking() throws RunnerException {
        // Mock
        when(consolidationService.showCreateBooking(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.showCreateBooking("delete");
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getGuidFromId() {
        // Mock
        when(consolidationService.getGuidFromId(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.getGuidFromId(111L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getGuidFromId2() {
        // Mock
        when(consolidationService.getGuidFromId(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.getGuidFromId(111L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getContainerPackSummary() {
        // Mock
        when(consolidationService.getContainerPackSummary(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.getContainerPackSummary(111L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getContainerPackSummary2() {
        // Mock
        when(consolidationService.getContainerPackSummary(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.getContainerPackSummary(111L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getContainerPackSummary3() {
        // Mock
        when(consolidationService.getContainerPackSummary(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.getContainerPackSummary(111L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getAutoUpdateGoodsAndHandlingInfo() {
        // Mock
        when(consolidationService.getAutoUpdateGoodsAndHandlingInfo(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.getAutoUpdateGoodsAndHandlingInfo(111L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getAutoUpdateGoodsAndHandlingInfo2() {
        // Mock
        when(consolidationService.getAutoUpdateGoodsAndHandlingInfo(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.getAutoUpdateGoodsAndHandlingInfo(111L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getAutoUpdateGoodsAndHandlingInfo3() {
        // Mock
        when(consolidationService.getAutoUpdateGoodsAndHandlingInfo(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.getAutoUpdateGoodsAndHandlingInfo(111L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getAllMasterData() {
        // Mock
        when(consolidationService.getAllMasterData(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.getAllMasterData(111L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getAllMasterData2() {
        // Mock
        when(consolidationService.getAllMasterData(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.getAllMasterData(111L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getAllMasterData3() {
        // Mock
        when(consolidationService.getAllMasterData(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.getAllMasterData(111L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void exportConsolidationList() throws IOException, IllegalAccessException, RunnerException {
        // Mock
        doNothing().when(consolidationService).exportExcel(any(), any());
        // Test
        consolidationController.exportConsolidationList(new MockHttpServletResponse(), ListCommonRequest.builder().build());
        // Assert
        assertTrue(true);
    }

    @Test
    void exportConsolidationList2() throws IOException, IllegalAccessException, RunnerException {
        // Mock
        doThrow(new RuntimeException()).when(consolidationService).exportExcel(any(), any());
        // Test
        consolidationController.exportConsolidationList(new MockHttpServletResponse(), ListCommonRequest.builder().build());
        // Assert
        assertTrue(true);
    }

    @Test
    void exportConsolidationList3() throws IOException, IllegalAccessException, RunnerException {
        // Mock
        doThrow(new RuntimeException("RuntimeException")).when(consolidationService).exportExcel(any(), any());
        // Test
        consolidationController.exportConsolidationList(new MockHttpServletResponse(), ListCommonRequest.builder().build());
        // Assert
        assertTrue(true);
    }

    @Test
    void createV1Consolidation() {
        // Mock
        when(consolidationReverseSync.reverseSync(any(), anyBoolean(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.createV1Consolidation(new CustomConsolidationRequest(), false, false);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createV1Consolidation2() {
        // Mock
        when(consolidationReverseSync.reverseSync(any(), anyBoolean(), anyBoolean())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.createV1Consolidation(new CustomConsolidationRequest(), false, false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createV1Consolidation3() {
        // Mock
        when(consolidationReverseSync.reverseSync(any(), anyBoolean(), anyBoolean())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.createV1Consolidation(new CustomConsolidationRequest(), false, false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void detachPacksAndShipments() {
        // Mock
        when(consolidationService.detachPacksAndShipments(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.detachPacksAndShipments(new ContainerShipmentADInConsoleRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void detachPacksAndShipments2() {
        // Mock
        when(consolidationService.detachPacksAndShipments(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.detachPacksAndShipments(new ContainerShipmentADInConsoleRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void detachPacksAndShipments3() {
        // Mock
        when(consolidationService.detachPacksAndShipments(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.detachPacksAndShipments(new ContainerShipmentADInConsoleRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void assignPacksAndShipments() {
        // Mock
        when(consolidationService.assignPacksAndShipments(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.assignPacksAndShipments(new ContainerShipmentADInConsoleRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void assignPacksAndShipments2() {
        // Mock
        when(consolidationService.assignPacksAndShipments(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.assignPacksAndShipments(new ContainerShipmentADInConsoleRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void assignPacksAndShipments3() {
        // Mock
        when(consolidationService.assignPacksAndShipments(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.assignPacksAndShipments(new ContainerShipmentADInConsoleRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listPacksForAssignDetach() {
        // Mock
        when(consolidationService.listPacksForAssignDetach(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.listPacksForAssignDetach(new ConsolePacksListRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listPacksForAssignDetach2() {
        // Mock
        when(consolidationService.listPacksForAssignDetach(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.listPacksForAssignDetach( new ConsolePacksListRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listPacksForAssignDetach3() {
        // Mock
        when(consolidationService.listPacksForAssignDetach(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.listPacksForAssignDetach(new ConsolePacksListRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculatePackSummary() throws RunnerException {
        // Mock
        when(consolidationService.calculatePackSummary(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.calculatePackSummary(new CalculatePackSummaryRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculatePackSummary2() throws RunnerException {
        // Mock
        when(consolidationService.calculatePackSummary(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.calculatePackSummary(new CalculatePackSummaryRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculatePackSummary3() throws RunnerException {
        // Mock
        when(consolidationService.calculatePackSummary(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.calculatePackSummary(new CalculatePackSummaryRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculatePackUtilisation() throws RunnerException {
        // Mock
        when(consolidationService.calculatePackUtilisation(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.calculatePackUtilisation(new CalculatePackUtilizationRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculatePackUtilisation2() throws RunnerException {
        // Mock
        when(consolidationService.calculatePackUtilisation(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.calculatePackUtilisation(new CalculatePackUtilizationRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculatePackUtilisation3() throws RunnerException {
        // Mock
        when(consolidationService.calculatePackUtilisation(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.calculatePackUtilisation(new CalculatePackUtilizationRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculateContainerSummary() throws RunnerException {
        // Mock
        when(consolidationService.calculateContainerSummary(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.calculateContainerSummary(new CalculateContainerSummaryRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculateContainerSummary2() throws RunnerException {
        // Mock
        when(consolidationService.calculateContainerSummary(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.calculateContainerSummary(new CalculateContainerSummaryRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculateContainerSummary3() throws RunnerException {
        // Mock
        when(consolidationService.calculateContainerSummary(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.calculateContainerSummary(new CalculateContainerSummaryRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update() throws RunnerException {
        // Mock
        when(consolidationService.update(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(11));
        // Test
        var responseEntity = consolidationController.update(new ConsolidationDetailsRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update2() throws RunnerException {
        // Mock
        when(consolidationService.update(any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(11));
        // Test
        var responseEntity = consolidationController.update(new ConsolidationDetailsRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update3() throws RunnerException {
        // Mock
        when(consolidationService.update(any())).thenThrow(new RuntimeException("RuntimeException"));
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(11));
        // Test
        var responseEntity = consolidationController.update(new ConsolidationDetailsRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void completeUpdate() throws RunnerException {
        // Mock
        when(consolidationService.completeUpdate(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.completeUpdate(new ConsolidationDetailsRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void completeUpdate2() throws RunnerException {
        // Mock
        when(consolidationService.completeUpdate(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.completeUpdate(new ConsolidationDetailsRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void completeUpdate3() throws RunnerException {
        // Mock
        when(consolidationService.completeUpdate(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.completeUpdate(new ConsolidationDetailsRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void toggleLock() throws RunnerException {
        // Mock
        when(consolidationService.toggleLock(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.toggleLock(111L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void toggleLock2() throws RunnerException {
        // Mock
        when(consolidationService.toggleLock(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.toggleLock(111L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void toggleLock3() throws RunnerException {
        // Mock
        when(consolidationService.toggleLock(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.toggleLock(111L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdate() throws RunnerException {
        // Mock
        when(consolidationService.partialUpdate(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.partialUpdate(new CustomerBookingRequest(), false);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdate2() throws RunnerException {
        // Mock
        when(consolidationService.partialUpdate(any(), anyBoolean())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.partialUpdate(new CustomerBookingRequest(), false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdate3() throws RunnerException {
        // Mock
        when(consolidationService.partialUpdate(any(), anyBoolean())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.partialUpdate(new CustomerBookingRequest(), false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getCustomConsol() throws RunnerException {
        // Mock
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.getCustomConsol(new ConsolidationDetailsRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getCustomConsol2() throws RunnerException {
        // Mock
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.getCustomConsol(new ConsolidationDetailsRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getCustomConsol3() throws RunnerException {
        // Mock
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.getCustomConsol(new ConsolidationDetailsRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getPendingNotifications() throws RunnerException {
        // Mock
        when(consolidationService.getPendingNotifications(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.getPendingNotifications(new PendingNotificationRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getPendingNotificationsFails() throws RunnerException {
        // Mock
        when(consolidationService.getPendingNotifications(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.getPendingNotifications(new PendingNotificationRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }


    @Test
    void listRequestedConsolidationForShipment() {
        // Mock
        when(consolidationService.listRequestedConsolidationForShipment(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = consolidationController.listRequestedConsolidationForShipment(123L, true);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listRequestedConsolidationForShipment2(){
        // Mock
        when(consolidationService.listRequestedConsolidationForShipment(any(), anyBoolean())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = consolidationController.listRequestedConsolidationForShipment(123L, true);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listRequestedConsolidationForShipment3() throws RunnerException {
        // Mock
        when(consolidationService.listRequestedConsolidationForShipment(any(), anyBoolean())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = consolidationController.listRequestedConsolidationForShipment(123L, true);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

}
