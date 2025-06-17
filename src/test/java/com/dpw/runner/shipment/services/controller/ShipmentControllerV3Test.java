package com.dpw.runner.shipment.services.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.request.GetMatchingRulesRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentConsoleAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.response.NotificationCount;
import com.dpw.runner.shipment.services.dto.response.ShipmentPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentRetrieveLiteResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksUnAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentSailingScheduleRequest;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentSailingScheduleResponse;
import com.dpw.runner.shipment.services.entity.enums.DpsExecutionStatus;
import com.dpw.runner.shipment.services.exception.exceptions.DpsException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import org.apache.http.auth.AuthenticationException;
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
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

@ContextConfiguration(classes = {ShipmentControllerV3.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentControllerV3Test {
    @Mock
    IShipmentServiceV3 shipmentService;
    @Mock
    IDpsEventService dpsEventService;
    @Mock
    JsonHelper jsonHelper;
    @InjectMocks
    ShipmentControllerV3 shipmentControllerV3;

    private MockMvc mockMvc;
    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        mockMvc = MockMvcBuilders.standaloneSetup(shipmentControllerV3).build();
    }

    @Test
    void getPendingNotificationCount() {
        when(shipmentService.getPendingNotificationCount()).thenReturn(NotificationCount.builder().build());
        var responseEntity = shipmentControllerV3.getPendingNotificationCount();
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list(){
        when(shipmentService.listShipment(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = shipmentControllerV3.list(ListCommonRequest.builder().build(), true);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getShipmentAssignContainerTray() {
        when(shipmentService.getShipmentAndPacksForConsolidationAssignContainerTray(anyLong(), anyLong())).thenReturn(new ShipmentPacksAssignContainerTrayDto());
        var responseEntity = shipmentControllerV3.getShipmentAssignContainerTray(1L, 2L);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void pendingNotificationsData(){
        ShipmentPendingNotificationResponse shipmentPendingNotificationResponse = new ShipmentPendingNotificationResponse();
        when(shipmentService.getPendingNotificationData(any())).thenReturn(shipmentPendingNotificationResponse);
        var responseEntity = shipmentControllerV3.pendingNotificationsData(1L);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void attachConsolidationTest() throws RunnerException {
        when(shipmentService.attachConsolidation(ShipmentConsoleAttachDetachV3Request.builder().build())).thenReturn(null);
        var responseEntity = shipmentControllerV3.attachConsolidation(ShipmentConsoleAttachDetachV3Request.builder().build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createTest() {
        ShipmentV3Request request = new ShipmentV3Request();
        when(shipmentService.create(any())).thenReturn(new ShipmentDetailsV3Response());
        when(jsonHelper.convertToJson(any())).thenReturn("json");

        var response = shipmentControllerV3.create(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(shipmentService).create(any());
    }

    @Test
    void completeUpdateTest() throws RunnerException {
        ShipmentV3Request request = new ShipmentV3Request();
        when(shipmentService.completeUpdate(any())).thenReturn(new ShipmentDetailsV3Response());
        when(jsonHelper.convertToJson(any())).thenReturn("json");

        var response = shipmentControllerV3.completeUpdate(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(shipmentService).completeUpdate(any());
    }

    @Test
    void deleteTest() {
        doNothing().when(shipmentService).delete(any());
        var response = shipmentControllerV3.delete(1L);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(shipmentService).delete(any());
    }

    @Test
    void retrieveByIdTest() throws RunnerException, AuthenticationException {
        Optional<Long> id = Optional.of(1L);
        Optional<String> guid = Optional.of("guid");
        when(shipmentService.retrieveById(any(), anyBoolean(), any())).thenReturn(new ShipmentRetrieveLiteResponse());
        when(jsonHelper.convertToJson(any())).thenReturn("json");

        var response = shipmentControllerV3.retrieveById(id, guid, true, "source");
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(shipmentService).retrieveById(any(), eq(true), eq("source"));
    }

    @Test
    void getAllMasterDataTest() {
        Map<String, Object> masterData = new HashMap<>();
        when(shipmentService.getAllMasterData(anyLong(), anyString())).thenReturn(masterData);
        var response = shipmentControllerV3.getAllMasterData(1L, "source");
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(shipmentService).getAllMasterData(1L, "source");
    }

    @Test
    void getShipmentUnAssignContainerTrayTest() {
        when(shipmentService.getShipmentAndPacksForConsolidationUnAssignContainerTray(anyLong())).thenReturn(new ShipmentPacksUnAssignContainerTrayDto());
        var response = shipmentControllerV3.getShipmentUnAssignContainerTray(1L);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(shipmentService).getShipmentAndPacksForConsolidationUnAssignContainerTray(1L);
    }

    @Test
    void updateSailingScheduleDataToShipmentTest() throws RunnerException {
        ShipmentSailingScheduleRequest request = new ShipmentSailingScheduleRequest();
        when(shipmentService.updateSailingScheduleDataToShipment(any())).thenReturn(new ShipmentSailingScheduleResponse());

        var response = shipmentControllerV3.updateSailingScheduleDataToShipment(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(shipmentService).updateSailingScheduleDataToShipment(request);
    }

    @Test
    void getMatchingRulesByGuidAndDpsExecutionState() {
        String guid = UUID.randomUUID().toString();
        GetMatchingRulesRequest getMatchingRulesRequest = new GetMatchingRulesRequest();
        getMatchingRulesRequest.setShipmentGuid(guid);
        getMatchingRulesRequest.setDpsExecutionStatusList(List.of(DpsExecutionStatus.ACTIVE, DpsExecutionStatus.COMPLETED));
        // Mock
        when(dpsEventService.getShipmentMatchingRulesByGuidAndExecutionState(getMatchingRulesRequest)).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentControllerV3.getMatchingRulesByGuidAndExecutionState(getMatchingRulesRequest);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getMatchingRulesByGuidAndDpsExecutionState_Failure() {
        String guid = UUID.randomUUID().toString();
        GetMatchingRulesRequest getMatchingRulesRequest = new GetMatchingRulesRequest();
        getMatchingRulesRequest.setShipmentGuid(guid);
        getMatchingRulesRequest.setDpsExecutionStatusList(List.of(DpsExecutionStatus.ACTIVE, DpsExecutionStatus.COMPLETED));
        // Mock
        when(dpsEventService.getShipmentMatchingRulesByGuidAndExecutionState(getMatchingRulesRequest)).thenThrow(new DpsException());
        // Test
        assertThrows(DpsException.class, () -> shipmentControllerV3.getMatchingRulesByGuidAndExecutionState(getMatchingRulesRequest));
    }

    @Test
    void getMatchingRulesByGuid() {
        String guid = UUID.randomUUID().toString();
        // Mock
        when(dpsEventService.getShipmentMatchingRulesByGuid(guid)).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentControllerV3.getMatchingRulesByGuid(guid);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getMatchingRulesByGuid_Failure() {
        String guid = UUID.randomUUID().toString();
        // Mock
        when(dpsEventService.getShipmentMatchingRulesByGuid((guid))).thenThrow(new DpsException());
        // Test
        assertThrows(DpsException.class, () -> shipmentControllerV3.getMatchingRulesByGuid(guid));
    }

    @Test
    void testConsoleShipmentList_Exception() throws AuthenticationException {
        // Mock
        when(shipmentService.consoleShipmentList(any(), anyLong(), eq(null), anyBoolean(), anyBoolean(), anyBoolean())).thenThrow(new RuntimeException("Test Exception"));
        ListCommonRequest request = mock(ListCommonRequest.class);
        // Test
        var responseEntity = shipmentControllerV3.consoleShipmentList(request, 1L, null, true, true, false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

}