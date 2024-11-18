package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.entity.DpsEvent;
import com.dpw.runner.shipment.services.entity.enums.DpsEntityType;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowType;
import com.dpw.runner.shipment.services.dto.request.MatchingRulesRequest;
import com.dpw.runner.shipment.services.exception.exceptions.DpsException;
import com.dpw.runner.shipment.services.repository.interfaces.IDpsEventRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.*;

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

        var responseEntity = dpsEventService.getShipmentMatchingRulesByGuid(commonRequestModel);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getShipmentMatchingRulesByGuid_Success1() {
        String guid = UUID.randomUUID().toString();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(guid).build());
        DpsEvent dpsEvent = new DpsEvent().setWorkflowType(DpsWorkflowType.HOLD).setEntityType(DpsEntityType.SHIPMENT);
        List<DpsEvent> dpsEvents = new ArrayList<>();
        dpsEvents.add(dpsEvent);
        when(dpsEventRepository.findDpsEventByGuidAndExecutionState(any(), any())).thenReturn(dpsEvents);
        when(dpsEventRepository.findDpsEventByGuidAndExecutionState(any(), any())).thenReturn(null);

        var responseEntity = dpsEventService.getShipmentMatchingRulesByGuid(commonRequestModel);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getShipmentMatchingRulesByGuid_Exception() {
        String guid = UUID.randomUUID().toString();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(guid).build());
        when(dpsEventRepository.findDpsEventByGuidAndExecutionState(any(), any())).thenThrow(new DpsException());

        var responseEntity = dpsEventService.getShipmentMatchingRulesByGuid(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void fetchMatchingRulesByGuid_Exception1() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().build());
        var responseEntity = dpsEventService.getShipmentMatchingRulesByGuid(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateWarningRulesStatus_Success() {
        String guid = UUID.randomUUID().toString();
        List<String> ruleList = new ArrayList<>();
        ruleList.add("test");
        MatchingRulesRequest matchingRulesRequest = MatchingRulesRequest.builder().guid(guid).username("test").ruleExecutionIds(ruleList).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(matchingRulesRequest);

        doNothing().when(dpsEventRepository).updateRuleStatus(any(), any(), any());

        var responseEntity = dpsEventService.updateWarningRulesStatus(commonRequestModel);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateWarningRulesStatus_Failure() {
        List<String> ruleList = new ArrayList<>();
        ruleList.add("test");
        MatchingRulesRequest matchingRulesRequest = MatchingRulesRequest.builder().username("test").ruleExecutionIds(ruleList).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(matchingRulesRequest);

        var responseEntity = dpsEventService.updateWarningRulesStatus(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateWarningRulesStatus_Failure2() {
        String guid = UUID.randomUUID().toString();
        List<String> ruleList = new ArrayList<>();
        ruleList.add("test");
        MatchingRulesRequest matchingRulesRequest = MatchingRulesRequest.builder().guid(guid).ruleExecutionIds(ruleList).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(matchingRulesRequest);

        var responseEntity = dpsEventService.updateWarningRulesStatus(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateWarningRulesStatus_Failure3() {
        String guid = UUID.randomUUID().toString();
        MatchingRulesRequest matchingRulesRequest = MatchingRulesRequest.builder().guid(guid).username("test").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(guid).build());
        when(dpsEventRepository.findDpsEventByGuidAndExecutionState(any(), any())).thenThrow(new DpsException());

        var responseEntity = dpsEventService.getShipmentMatchingRulesByGuid(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateWarningRulesStatus_Failure4() {
        String guid = UUID.randomUUID().toString();
        List<String> ruleList = new ArrayList<>();
        ruleList.add("test");
        MatchingRulesRequest matchingRulesRequest = MatchingRulesRequest.builder().guid(guid).username("test").ruleExecutionIds(ruleList).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(matchingRulesRequest);
        doThrow(RuntimeException.class).when(dpsEventRepository).updateRuleStatus(any(), any(), any());

        var responseEntity = dpsEventService.updateWarningRulesStatus(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }
}