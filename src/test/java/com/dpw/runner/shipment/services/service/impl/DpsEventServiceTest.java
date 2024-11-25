package com.dpw.runner.shipment.services.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.entity.DpsEvent;
import com.dpw.runner.shipment.services.entity.enums.DpsEntityType;
import com.dpw.runner.shipment.services.entity.enums.DpsExecutionStatus;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowState;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowType;
import com.dpw.runner.shipment.services.exception.exceptions.DpsException;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto.DpsDataDto;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto.DpsFieldDataDto;
import com.dpw.runner.shipment.services.repository.interfaces.IDpsEventRepository;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
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

    @Test
    void getShipmentMatchingRulesByGuid_Success() {
        String guid = UUID.randomUUID().toString();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(guid).build());
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
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(guid).build());
        when(dpsEventRepository.findDpsEventByGuidAndExecutionState(any(), any())).thenReturn(null);

        assertThrows(DpsException.class, () -> dpsEventService.getShipmentMatchingRulesByGuid(guid));
    }

    @Test
    void getShipmentMatchingRulesByGuid_Exception() {
        String guid = UUID.randomUUID().toString();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(guid).build());
        when(dpsEventRepository.findDpsEventByGuidAndExecutionState(any(), any())).thenThrow(new DpsException());

        assertThrows(DpsException.class, () -> dpsEventService.getShipmentMatchingRulesByGuid(guid));
    }

    @Test
    void fetchMatchingRulesByGuid_Exception1() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().build());
        assertThrows(DpsException.class, () -> dpsEventService.getShipmentMatchingRulesByGuid(null));
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
        assertNull(dpsEvent.getEntityType());
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



}