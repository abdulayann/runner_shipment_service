package com.dpw.runner.shipment.services.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.GetMatchingRulesRequest;
import com.dpw.runner.shipment.services.dto.response.DpsEventResponse;
import com.dpw.runner.shipment.services.entity.DpsEvent;
import com.dpw.runner.shipment.services.entity.DpsEvent.DpsFieldData;
import com.dpw.runner.shipment.services.entity.DpsEventLog;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.DpsEntityType;
import com.dpw.runner.shipment.services.entity.enums.DpsExecutionStatus;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowState;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowType;
import com.dpw.runner.shipment.services.exception.exceptions.DpsException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto.DpsDataDto;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto.DpsFieldDataDto;
import com.dpw.runner.shipment.services.repository.interfaces.IDpsEventRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.fasterxml.jackson.core.JsonProcessingException;
import java.lang.reflect.InvocationTargetException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class DpsEventServiceTest {

    @InjectMocks
    private DpsEventService dpsEventService;
    @Mock
    private IDpsEventRepository dpsEventRepository;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private IAuditLogService auditLogService;

    @Test
    void getShipmentMatchingRulesByGuid_Success() {
        String guid = UUID.randomUUID().toString();
        DpsEvent dpsEvent = new DpsEvent().setWorkflowType(DpsWorkflowType.HOLD).setEntityType(DpsEntityType.SHIPMENT);
        List<DpsEvent> dpsEvents = new ArrayList<>();
        dpsEvents.add(dpsEvent);
        when(dpsEventRepository.findDpsEventByGuidAndExecutionState(any(), any())).thenReturn(dpsEvents);

        var responseEntity = dpsEventService.getShipmentMatchingRulesByGuid(guid);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getShipmentMatchingRulesByGuid_Success1() {
        String guid = UUID.randomUUID().toString();
        when(dpsEventRepository.findDpsEventByGuidAndExecutionState(any(), any())).thenReturn(null);
        assertEquals(HttpStatus.OK, dpsEventService.getShipmentMatchingRulesByGuid(guid).getStatusCode());
    }

    @Test
    void getShipmentMatchingRulesByGuid_Exception() {
        String guid = UUID.randomUUID().toString();
        when(dpsEventRepository.findDpsEventByGuidAndExecutionState(any(), any())).thenThrow(new DpsException());

        assertThrows(DpsException.class, () -> dpsEventService.getShipmentMatchingRulesByGuid(guid));
    }

    @Test
    void fetchMatchingRulesByGuid_Exception1() {
        assertThrows(DpsException.class, () -> dpsEventService.getShipmentMatchingRulesByGuid(null));
    }

    @Test
    void getShipmentMatchingRulesByGuidAndExecutionState_Success() {
        String guid = UUID.randomUUID().toString();
        GetMatchingRulesRequest getMatchingRulesRequest = new GetMatchingRulesRequest();
        getMatchingRulesRequest.setShipmentGuid(guid);
        getMatchingRulesRequest.setDpsExecutionStatusList(List.of(DpsExecutionStatus.ACTIVE, DpsExecutionStatus.COMPLETED));
        DpsEvent dpsEvent = new DpsEvent().setWorkflowType(DpsWorkflowType.HOLD).setEntityType(DpsEntityType.SHIPMENT);
        List<DpsEvent> dpsEvents = new ArrayList<>();
        dpsEvents.add(dpsEvent);
        when(dpsEventRepository.findDpsEventByGuidAndExecutionStateIn(any(), any())).thenReturn(dpsEvents);

        var responseEntity = dpsEventService.getShipmentMatchingRulesByGuidAndExecutionState(getMatchingRulesRequest);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getShipmentMatchingRulesByGuidAndExecutionStatus_Success1() {
        String guid = UUID.randomUUID().toString();
        GetMatchingRulesRequest getMatchingRulesRequest = new GetMatchingRulesRequest();
        getMatchingRulesRequest.setShipmentGuid(guid);
        getMatchingRulesRequest.setDpsExecutionStatusList(List.of(DpsExecutionStatus.ACTIVE, DpsExecutionStatus.COMPLETED));
        when(dpsEventRepository.findDpsEventByGuidAndExecutionStateIn(any(), any())).thenReturn(null);
        assertEquals(HttpStatus.OK, dpsEventService.getShipmentMatchingRulesByGuidAndExecutionState(getMatchingRulesRequest).getStatusCode());
    }

    @Test
    void getShipmentMatchingRulesByGuidAndExecutionState_Exception() {
        String guid = UUID.randomUUID().toString();
        GetMatchingRulesRequest getMatchingRulesRequest = new GetMatchingRulesRequest();
        getMatchingRulesRequest.setShipmentGuid(guid);
        getMatchingRulesRequest.setDpsExecutionStatusList(List.of(DpsExecutionStatus.ACTIVE, DpsExecutionStatus.COMPLETED));
        when(dpsEventRepository.findDpsEventByGuidAndExecutionStateIn(any(), any())).thenThrow(new DpsException());

        assertThrows(DpsException.class, () -> dpsEventService.getShipmentMatchingRulesByGuidAndExecutionState(getMatchingRulesRequest));
    }

    @Test
    void getShipmentMatchingRulesByGuidAndExecutionState_GuidException() {
        GetMatchingRulesRequest getMatchingRulesRequest = new GetMatchingRulesRequest();
        getMatchingRulesRequest.setDpsExecutionStatusList(List.of(DpsExecutionStatus.ACTIVE, DpsExecutionStatus.COMPLETED));

        assertThrows(DpsException.class, () -> dpsEventService.getShipmentMatchingRulesByGuidAndExecutionState(getMatchingRulesRequest));
    }

    @Test
    void testConstructDpsEvent_NewEvent() {
        DpsDto dpsDto = new DpsDto();
        DpsDataDto dataDto = new DpsDataDto();
        dataDto.setRuleExecutionId(UUID.randomUUID());
        dataDto.setEntityId("123");
        dataDto.setWorkflowType("HOLD");
        dataDto.setState("HOLD");
        dataDto.setRuleStatus("ACTIVE");
        dataDto.setEntityType("SHIPMENT");
        dataDto.setText("Test Event");
        dataDto.setMatchingCondition("Condition1");
        dataDto.setImplications(List.of("Implication1"));
        dataDto.setConditionMessage(List.of("Message1"));
        dataDto.setRuleMatchedFieldList(List.of("Field1"));
        dataDto.setUsernameList(List.of("User1"));
        dataDto.setTasks(List.of(new Object()));
        dataDto.setEventTimestamp(LocalDateTime.now());
        dpsDto.setTransactionId("TX123");
        dpsDto.setData(dataDto);

        // Mock repository
        when(dpsEventRepository.findByExecutionId(any(UUID.class))).thenReturn(null);

        DpsEvent dpsEvent = dpsEventService.constructDpsEvent(dpsDto);

        assertNotNull(dpsEvent);
        assertEquals(dpsDto.getData().getRuleExecutionId(), dpsEvent.getExecutionId());
        assertEquals(dpsDto.getData().getEntityId(), dpsEvent.getEntityId());
        assertEquals(DpsWorkflowType.HOLD, dpsEvent.getWorkflowType());
        assertEquals(DpsWorkflowState.HOLD, dpsEvent.getState());
        assertEquals(DpsExecutionStatus.ACTIVE, dpsEvent.getStatus());
        assertEquals(DpsEntityType.SHIPMENT, dpsEvent.getEntityType());
        assertEquals(dpsDto.getData().getText(), dpsEvent.getText());
        assertEquals(dpsDto.getData().getMatchingCondition(), dpsEvent.getMatchingCondition());
        assertEquals(dpsDto.getData().getImplications(), dpsEvent.getImplicationList());
        assertEquals(dpsDto.getData().getConditionMessage(), dpsEvent.getConditionMessageList());
        assertEquals(dpsDto.getData().getRuleMatchedFieldList(), dpsEvent.getRuleMatchedFieldList());
        assertEquals(dpsDto.getData().getUsernameList(), dpsEvent.getUsernameList());
        assertEquals(dpsDto.getData().getTasks(), dpsEvent.getTasks());
        assertEquals(dpsDto.getData().getEventTimestamp(), dpsEvent.getEventTimestamp());
        assertEquals(dpsDto.getTransactionId(), dpsEvent.getTransactionId());
    }

    @Test
    void testConstructDpsEvent_UpdateExistingEvent() {
        DpsDto dpsDto = new DpsDto();
        DpsDataDto dataDto = new DpsDataDto();
        dataDto.setRuleExecutionId(UUID.randomUUID());
        dataDto.setEntityId("123");
        dataDto.setWorkflowType("HOLD");
        dataDto.setState("UN_HOLD");
        dataDto.setRuleStatus("COMPLETED");
        dataDto.setEntityType("SHIPMENT");
        dataDto.setText("Updated Event");
        dataDto.setMatchingCondition("Updated Condition");
        dataDto.setImplications(List.of("Updated Implication"));
        dataDto.setConditionMessage(List.of("Updated Message"));
        dataDto.setRuleMatchedFieldList(List.of("Updated Field"));
        dataDto.setUsernameList(List.of("UpdatedUser"));
        dataDto.setTasks(List.of(new Object()));
        dataDto.setEventTimestamp(LocalDateTime.now());
        dpsDto.setTransactionId("TX123");
        dpsDto.setData(dataDto);

        // Mock repository
        DpsEvent existingEvent = new DpsEvent();
        existingEvent.setExecutionId(UUID.randomUUID());  // Different ID
        when(dpsEventRepository.findByExecutionId(any(UUID.class))).thenReturn(existingEvent);

        DpsEvent dpsEvent = dpsEventService.constructDpsEvent(dpsDto);

        assertNotNull(dpsEvent);
        assertEquals(dpsDto.getData().getRuleExecutionId(), dpsEvent.getExecutionId());
        assertEquals(dpsDto.getData().getEntityId(), dpsEvent.getEntityId());
        assertEquals(DpsWorkflowType.HOLD, dpsEvent.getWorkflowType());
        assertEquals(DpsWorkflowState.UN_HOLD, dpsEvent.getState());
        assertEquals(DpsExecutionStatus.COMPLETED, dpsEvent.getStatus());
        assertEquals(DpsEntityType.SHIPMENT, dpsEvent.getEntityType());
        assertEquals(dpsDto.getData().getText(), dpsEvent.getText());
        assertEquals(dpsDto.getData().getMatchingCondition(), dpsEvent.getMatchingCondition());
        assertEquals(dpsDto.getData().getImplications(), dpsEvent.getImplicationList());
        assertEquals(dpsDto.getData().getConditionMessage(), dpsEvent.getConditionMessageList());
        assertEquals(dpsDto.getData().getRuleMatchedFieldList(), dpsEvent.getRuleMatchedFieldList());
        assertEquals(dpsDto.getData().getUsernameList(), dpsEvent.getUsernameList());
        assertEquals(dpsDto.getData().getTasks(), dpsEvent.getTasks());
        assertEquals(dpsDto.getData().getEventTimestamp(), dpsEvent.getEventTimestamp());
        assertEquals(dpsDto.getTransactionId(), dpsEvent.getTransactionId());
    }

    @Test
    void testConstructDpsEvent_NullFields() {
        DpsDto dpsDto = new DpsDto();
        DpsDataDto dataDto = new DpsDataDto();
        dataDto.setRuleExecutionId(UUID.randomUUID());
        dataDto.setEntityId("123");
        dataDto.setEntityType("SHIPMENT");
        // Leave workflowType, state, status, entityType as null
        dataDto.setText("Test Event");
        dataDto.setEventTimestamp(LocalDateTime.now());
        dpsDto.setTransactionId("TX123");
        dpsDto.setData(dataDto);

        // Mock repository
        when(dpsEventRepository.findByExecutionId(any(UUID.class))).thenReturn(null);

        DpsEvent dpsEvent = dpsEventService.constructDpsEvent(dpsDto);

        assertNotNull(dpsEvent);
        assertNull(dpsEvent.getWorkflowType());
        assertNull(dpsEvent.getState());
        assertNull(dpsEvent.getStatus());
    }

    @Test
    void testConstructDpsEvent_ExceptionHandling() {
        DpsDto dpsDto = new DpsDto();
        DpsDataDto dataDto = new DpsDataDto();
        dataDto.setRuleExecutionId(UUID.randomUUID());
        dataDto.setEntityId("123");
        dataDto.setWorkflowType("HOLD");
        dataDto.setState("HOLD");
        dataDto.setRuleStatus("ACTIVE");
        dataDto.setEntityType("SHIPMENT");
        dpsDto.setData(dataDto);

        // Mock repository to simulate an exception
        when(dpsEventRepository.findByExecutionId(any(UUID.class))).thenThrow(new RuntimeException("Database error"));

        // Verify that DpsException is thrown
        assertThrows(DpsException.class, () -> dpsEventService.constructDpsEvent(dpsDto));
    }

    @Test
    void testConstructDpsEvent_PopulateDpsFieldData() {
        // Prepare test data
        DpsDto dpsDto = new DpsDto();
        DpsDataDto dataDto = new DpsDataDto();
        dataDto.setRuleExecutionId(UUID.randomUUID());
        dataDto.setEntityId("123");
        dataDto.setWorkflowType("HOLD");
        dataDto.setState("HOLD");
        dataDto.setRuleStatus("ACTIVE");
        dataDto.setEntityType("SHIPMENT");
        dataDto.setText("Test Event");

        // Add FieldsDetectedValues (non-empty)
        List<DpsFieldDataDto> fieldsDetectedValues = List.of(
                new DpsFieldDataDto("field1", "value1"),
                new DpsFieldDataDto("field2", "value2")
        );
        dataDto.setFieldsDetectedValues(fieldsDetectedValues);

        dpsDto.setData(dataDto);
        dpsDto.setTransactionId("TX123");

        // Mock repository
        when(dpsEventRepository.findByExecutionId(any(UUID.class))).thenReturn(null);

        // Create DpsEventService
        DpsEvent dpsEvent = dpsEventService.constructDpsEvent(dpsDto);

        // Assertions to ensure DpsFieldData is populated
        assertNotNull(dpsEvent.getDpsFieldData());
        assertEquals(2, dpsEvent.getDpsFieldData().size());
        assertEquals("field1", dpsEvent.getDpsFieldData().get(0).getKey());
        assertEquals("value1", dpsEvent.getDpsFieldData().get(0).getValue());
        assertEquals("field2", dpsEvent.getDpsFieldData().get(1).getKey());
        assertEquals("value2", dpsEvent.getDpsFieldData().get(1).getValue());
    }

    @Test
    void testConstructDpsEventResponse_SuccessfulTransformationWithEnums() {
        // Arrange
        DpsEvent dpsEvent = new DpsEvent();
        dpsEvent.setId(1L);
        dpsEvent.setGuid(UUID.randomUUID());
        dpsEvent.setExecutionId(UUID.randomUUID());
        dpsEvent.setEntityId(UUID.randomUUID().toString());
        dpsEvent.setEntityType(DpsEntityType.SHIPMENT);
        dpsEvent.setWorkflowType(DpsWorkflowType.HOLD);  // Assuming WorkflowType is an enum
        dpsEvent.setState(DpsWorkflowState.PER_BLOCKED);  // Assuming State is an enum
        dpsEvent.setStatus(DpsExecutionStatus.ACTIVE);  // Assuming Status is an enum
        dpsEvent.setText("text");
        dpsEvent.setMatchingCondition("matchingCondition");
        dpsEvent.setImplicationList(List.of("implication1", "implication2"));
        dpsEvent.setConditionMessageList(List.of("message1", "message2"));
        dpsEvent.setDpsFieldData(List.of(new DpsFieldData("key1", "value1")));
        dpsEvent.setUsernameList(List.of("user1", "user2"));
        dpsEvent.setEventTimestamp(LocalDateTime.now());
        dpsEvent.setTasks(List.of("task1", "task2"));

        // Act
        DpsEventResponse response = dpsEventService.constructDpsEventResponse(dpsEvent);

        // Assert
        assertNotNull(response);
        assertEquals(dpsEvent.getId(), response.getId());
        assertEquals(dpsEvent.getGuid(), response.getGuid());
        assertEquals(dpsEvent.getWorkflowType(), response.getWorkflowType());
        assertEquals(dpsEvent.getState(), response.getState());
        assertEquals(dpsEvent.getStatus(), response.getStatus());
        assertEquals(dpsEvent.getDpsFieldData().size(), response.getDpsFieldData().size());
        assertEquals(dpsEvent.getImplicationList().size(), response.getImplicationList().size());
        assertEquals(dpsEvent.getConditionMessageList().size(), response.getConditionMessageList().size());
    }

    @Test
     void testConstructDpsEventResponse_TransformationWithNullFields() {
        DpsEvent dpsEvent = new DpsEvent();
        dpsEvent.setId(null);
        dpsEvent.setGuid(null);
        dpsEvent.setExecutionId(null);
        dpsEvent.setEntityId(null);
        dpsEvent.setEntityType(null);
        dpsEvent.setWorkflowType(null);
        dpsEvent.setState(null);
        dpsEvent.setStatus(null);
        dpsEvent.setText(null);
        dpsEvent.setMatchingCondition(null);
        dpsEvent.setImplicationList(null);
        dpsEvent.setConditionMessageList(null);
        dpsEvent.setDpsFieldData(null);
        dpsEvent.setUsernameList(null);
        dpsEvent.setEventTimestamp(null);
        dpsEvent.setTasks(null);

        // Act
        DpsEventResponse response = dpsEventService.constructDpsEventResponse(dpsEvent);

        // Assert
        assertNotNull(response);
        assertNull(response.getId());
        assertNull(response.getGuid());
        assertNull(response.getExecutionId());
        assertNull(response.getEntityId());
        assertNull(response.getEntityType());
        assertNull(response.getWorkflowType());
        assertNull(response.getState());
        assertNull(response.getStatus());
        assertNull(response.getText());
        assertTrue(response.getImplicationList().isEmpty());
        assertTrue(response.getConditionMessageList().isEmpty());
        assertTrue(response.getDpsFieldData().isEmpty());
        assertTrue(response.getUsernameList().isEmpty());
        assertNull(response.getEventTimestamp());
        assertTrue(response.getTasks().isEmpty());
    }

    @Test
     void testConstructDpsEventResponse_TransformationWithEmptyLists() {
        // Arrange
        DpsEvent dpsEvent = new DpsEvent();
        dpsEvent.setId(1L);
        dpsEvent.setGuid(UUID.randomUUID());
        dpsEvent.setExecutionId(UUID.randomUUID());
        dpsEvent.setEntityId(UUID.randomUUID().toString());
        dpsEvent.setEntityType(DpsEntityType.SHIPMENT);
        dpsEvent.setWorkflowType(DpsWorkflowType.HOLD);
        dpsEvent.setState(DpsWorkflowState.PER_BLOCKED);
        dpsEvent.setStatus(DpsExecutionStatus.ACTIVE);
        dpsEvent.setText("text");
        dpsEvent.setMatchingCondition("matchingCondition");
        dpsEvent.setImplicationList(Collections.emptyList());  // Empty list
        dpsEvent.setConditionMessageList(Collections.emptyList());  // Empty list
        dpsEvent.setDpsFieldData(Collections.emptyList());  // Empty list
        dpsEvent.setUsernameList(Collections.emptyList());  // Empty list
        dpsEvent.setEventTimestamp(LocalDateTime.now());
        dpsEvent.setTasks(Collections.emptyList());  // Empty list

        // Act
        DpsEventResponse response = dpsEventService.constructDpsEventResponse(dpsEvent);

        // Assert
        assertNotNull(response);
        assertEquals(dpsEvent.getId(), response.getId());
        assertEquals(dpsEvent.getGuid(), response.getGuid());
        assertEquals(dpsEvent.getWorkflowType(), response.getWorkflowType());
        assertEquals(dpsEvent.getState(), response.getState());
        assertEquals(dpsEvent.getStatus(), response.getStatus());
        assertEquals(0, response.getImplicationList().size());
        assertEquals(0, response.getConditionMessageList().size());
        assertEquals(0, response.getDpsFieldData().size());
        assertEquals(0, response.getUsernameList().size());
        assertEquals(dpsEvent.getEventTimestamp(), response.getEventTimestamp());
        assertEquals(0, response.getTasks().size());
    }

    @Test
     void testConstructDpsEventResponse_TransformationWithEmptyFieldDataList() {
        // Arrange
        DpsEvent dpsEvent = new DpsEvent();
        dpsEvent.setId(1L);
        dpsEvent.setGuid(UUID.randomUUID());
        dpsEvent.setExecutionId(UUID.randomUUID());
        dpsEvent.setEntityId(UUID.randomUUID().toString());
        dpsEvent.setEntityType(DpsEntityType.SHIPMENT);
        dpsEvent.setWorkflowType(DpsWorkflowType.HOLD);
        dpsEvent.setState(DpsWorkflowState.PER_BLOCKED);
        dpsEvent.setStatus(DpsExecutionStatus.ACTIVE);
        dpsEvent.setText("text");
        dpsEvent.setMatchingCondition("matchingCondition");
        dpsEvent.setImplicationList(List.of("implication1", "implication2"));
        dpsEvent.setConditionMessageList(List.of("message1", "message2"));
        dpsEvent.setDpsFieldData(Collections.emptyList());  // Empty list
        dpsEvent.setUsernameList(List.of("user1", "user2"));
        dpsEvent.setEventTimestamp(LocalDateTime.now());
        dpsEvent.setTasks(List.of("task1", "task2"));

        // Act
        DpsEventResponse response = dpsEventService.constructDpsEventResponse(dpsEvent);

        // Assert
        assertNotNull(response);
        assertEquals(dpsEvent.getId(), response.getId());
        assertEquals(dpsEvent.getGuid(), response.getGuid());
        assertEquals(dpsEvent.getWorkflowType(), response.getWorkflowType());
        assertEquals(dpsEvent.getState(), response.getState());
        assertEquals(dpsEvent.getStatus(), response.getStatus());
        assertEquals(0, response.getDpsFieldData().size());
        assertEquals(dpsEvent.getImplicationList().size(), response.getImplicationList().size());
        assertEquals(dpsEvent.getConditionMessageList().size(), response.getConditionMessageList().size());
    }

    @Test
     void testConstructDpsEventResponse_ExceptionHandling() {
        // Arrange
        DpsEvent dpsEvent = mock(DpsEvent.class);
        when(dpsEvent.getDpsFieldData()).thenThrow(new RuntimeException("Database connection error"));

        // Act & Assert
        assertThrows(DpsException.class, () -> dpsEventService.constructDpsEventResponse(dpsEvent));
    }

    @Test
     void testCreateAuditLog_SuccessfulAuditLogCreation() throws RunnerException, IllegalAccessException, NoSuchFieldException, NoSuchMethodException, InvocationTargetException, JsonProcessingException {
        // Arrange
        DpsEvent dpsEvent = new DpsEvent();
        dpsEvent.setExecutionId(UUID.randomUUID());
        dpsEvent.setTransactionId("transaction-123");
        dpsEvent.setUsernameList(List.of("user1", "user2"));
        dpsEvent.setState(DpsWorkflowState.PER_BLOCKED); // Assuming this is an enum
        dpsEvent.setEventTimestamp(LocalDateTime.now());
        dpsEvent.setEntityId(UUID.randomUUID().toString());

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setShipmentId("shipment-123");
        shipmentDetails.setTenantId(456); // Tenant ID as Long
        shipmentDetails.setId(1L);

        // Mocking the method to do nothing successfully
        doAnswer(invocation -> null).when(auditLogService).addAuditLog(any(AuditLogMetaData.class));

        // Act
        dpsEventService.createAuditLog(dpsEvent, shipmentDetails);

        // Assert
        ArgumentCaptor<AuditLogMetaData> captor = ArgumentCaptor.forClass(AuditLogMetaData.class);
        verify(auditLogService, times(1)).addAuditLog(captor.capture());

        AuditLogMetaData capturedLog = captor.getValue();
        assertNotNull(capturedLog);
        assertEquals(shipmentDetails.getTenantId(), capturedLog.getTenantId());
        assertEquals(ShipmentDetails.class.getSimpleName(), capturedLog.getParent());
        assertEquals(shipmentDetails.getId(), capturedLog.getParentId());
        assertEquals(DpsEventLog.class.getSimpleName(), capturedLog.getEntityType());
        assertEquals(DBOperationType.LOG.name(), capturedLog.getOperation());

        DpsEventLog capturedEventLog = (DpsEventLog) capturedLog.getNewData();
        assertNotNull(capturedEventLog);
        assertEquals(dpsEvent.getExecutionId().toString(), capturedEventLog.getExecutionId());
        assertEquals(dpsEvent.getTransactionId(), capturedEventLog.getTransactionId());
        assertEquals(String.join(",", dpsEvent.getUsernameList()), capturedEventLog.getUsernameList());
        assertEquals(dpsEvent.getState(), capturedEventLog.getDpsWorkflowState());
        assertEquals(dpsEvent.getEventTimestamp(), capturedEventLog.getEventTimeStamp());
        assertEquals(shipmentDetails.getShipmentId(), capturedEventLog.getShipmentId());
        assertEquals(shipmentDetails.getTenantId(), capturedEventLog.getTenantId());
    }

    @Test
     void testCreateAuditLog_ExceptionHandling()
            throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        DpsEvent dpsEvent = new DpsEvent();
        dpsEvent.setExecutionId(UUID.randomUUID());
        dpsEvent.setTransactionId("transaction-123");
        dpsEvent.setUsernameList(List.of("user1", "user2"));
        dpsEvent.setState(DpsWorkflowState.PER_BLOCKED); // Assuming this is an enum
        dpsEvent.setEventTimestamp(LocalDateTime.now());
        dpsEvent.setEntityId(UUID.randomUUID().toString());

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setShipmentId("shipment-123");
        shipmentDetails.setTenantId(456); // Tenant ID as Long
        shipmentDetails.setId(1L);

        // Mock the behavior to throw an exception
        doThrow(new RuntimeException("Mocked Exception"))
                .when(auditLogService).addAuditLog(any(AuditLogMetaData.class));

        // Act & Assert
        assertThrows(DpsException.class, () -> {
            dpsEventService.createAuditLog(dpsEvent, shipmentDetails);
        });
    }

    @Test
     void testGetImplicationsForShipment_NullOrEmptyShipmentGuid() {
        assertThrows(DpsException.class, () ->
                dpsEventService.getImplicationsForShipment(null)
        );

        String emptyGuid = "";

        assertThrows(DpsException.class, () ->
                dpsEventService.getImplicationsForShipment(emptyGuid)
        );
    }

    @Test
     void testGetImplicationsForShipment_NoImplicationsFound() {
        // Arrange
        String shipmentGuid = "shipment-123";
        when(dpsEventRepository.findImplicationsByEntityIdAndEntityType(
                shipmentGuid,
                DpsEntityType.SHIPMENT.name(),
                DpsExecutionStatus.ACTIVE.name()
        )).thenReturn(Collections.emptyList());

        // Act
        List<String> result = dpsEventService.getImplicationsForShipment(shipmentGuid);

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(dpsEventRepository).findImplicationsByEntityIdAndEntityType(
                shipmentGuid,
                DpsEntityType.SHIPMENT.name(),
                DpsExecutionStatus.ACTIVE.name()
        );
        // You can verify logs using logging frameworks or mocks (if configured).
    }

    @Test
     void testGetImplicationsForShipment_ValidImplicationsFound() {
        // Arrange
        String shipmentGuid = "shipment-123";
        List<String> mockImplications = List.of("implication1", "implication2");

        when(dpsEventRepository.findImplicationsByEntityIdAndEntityType(
                shipmentGuid,
                DpsEntityType.SHIPMENT.name(),
                DpsExecutionStatus.ACTIVE.name()
        )).thenReturn(mockImplications);

        // Act
        List<String> result = dpsEventService.getImplicationsForShipment(shipmentGuid);

        // Assert
        assertNotNull(result);
        assertEquals(mockImplications.size(), result.size());
        assertEquals(mockImplications, result);
        verify(dpsEventRepository).findImplicationsByEntityIdAndEntityType(
                shipmentGuid,
                DpsEntityType.SHIPMENT.name(),
                DpsExecutionStatus.ACTIVE.name()
        );
    }

    @Test
     void testGetImplicationsForShipment_RepositoryException() {
        // Arrange
        String shipmentGuid = "shipment-123";

        when(dpsEventRepository.findImplicationsByEntityIdAndEntityType(
                shipmentGuid,
                DpsEntityType.SHIPMENT.name(),
                DpsExecutionStatus.ACTIVE.name()
        )).thenThrow(new RuntimeException("Database error"));

        // Act & Assert
        RuntimeException exception = assertThrows(RuntimeException.class, () ->
                dpsEventService.getImplicationsForShipment(shipmentGuid)
        );

        assertEquals("Database error", exception.getMessage());
        verify(dpsEventRepository).findImplicationsByEntityIdAndEntityType(
                shipmentGuid,
                DpsEntityType.SHIPMENT.name(),
                DpsExecutionStatus.ACTIVE.name()
        );
    }

    @Test
     void testSaveDpsEvent_Successful()
            throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        DpsDto dpsDto = new DpsDto();
        DpsDto.DpsDataDto dpsDataDto = new DpsDto.DpsDataDto();
        dpsDataDto.setRuleExecutionId(UUID.randomUUID());
        dpsDto.setData(dpsDataDto);

        DpsEvent savedEvent = new DpsEvent();
        savedEvent.setExecutionId(UUID.randomUUID());
        savedEvent.setEntityType(DpsEntityType.SHIPMENT);
        savedEvent.setEntityId(UUID.randomUUID().toString());
        savedEvent.setState(DpsWorkflowState.PER_BLOCKED);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);

        when(dpsEventRepository.findByExecutionId(any())).thenReturn(savedEvent);
        when(dpsEventRepository.save(any(DpsEvent.class))).thenReturn(savedEvent);
        when(shipmentDao.findShipmentsByGuids(anySet()))
                .thenReturn(List.of(shipmentDetails));
        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));
        doNothing().when(shipmentDao).saveStatus(any(), any());
        // Act
        DpsEvent result = dpsEventService.saveDpsEvent(dpsDto);

        // Assert
        assertNotNull(result);
        assertEquals(savedEvent, result);

        verify(dpsEventRepository).save(any(DpsEvent.class));
        verify(shipmentDao).findShipmentsByGuids(anySet());
        verify(auditLogService).addAuditLog(any(AuditLogMetaData.class));
    }

    @Test
     void testSaveDpsEvent_ShipmentGuidNullOrEmpty() {
        // Arrange
        DpsDto dpsDto = new DpsDto();
        DpsEvent constructedEvent = new DpsEvent();
        constructedEvent.setEntityType(DpsEntityType.SHIPMENT);
        constructedEvent.setEntityId(null); // Simulate null GUID
        assertThrows(DpsException.class, () ->
                dpsEventService.saveDpsEvent(dpsDto)
        );

    }

    @Test
     void testSaveDpsEvent_ShipmentNotFound() {
        // Arrange
        DpsDto dpsDto = new DpsDto();
        DpsEvent constructedEvent = new DpsEvent();
        constructedEvent.setEntityType(DpsEntityType.SHIPMENT);
        constructedEvent.setEntityId(UUID.randomUUID().toString());

        assertThrows(DpsException.class, () ->
                dpsEventService.saveDpsEvent(dpsDto)
        );
    }

    @Test
     void testSaveDpsEvent_StateTransitionValidationFails() {
        // Arrange
        DpsDto dpsDto = new DpsDto();
        DpsEvent constructedEvent = new DpsEvent();
        constructedEvent.setEntityType(DpsEntityType.SHIPMENT);
        constructedEvent.setEntityId(UUID.randomUUID().toString());
        constructedEvent.setState(DpsWorkflowState.PER_BLOCKED);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);

        // Act & Assert
        assertThrows(DpsException.class, () ->
                dpsEventService.saveDpsEvent(dpsDto)
        );

    }

    @Test
    void testSaveDpsEvent_RepositorySaveFails() {
        DpsDto dpsDto = new DpsDto();
        assertThrows(DpsException.class, () ->
                dpsEventService.saveDpsEvent(dpsDto)
        );
    }


}