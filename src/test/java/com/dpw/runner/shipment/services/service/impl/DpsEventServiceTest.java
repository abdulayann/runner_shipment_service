package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.exception.exceptions.DpsException;
import com.dpw.runner.shipment.services.repository.interfaces.IDpsEventRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class DpsEventServiceTest {

    @InjectMocks
    private DpsEventService dpsEventService;
    @Mock
    private IDpsEventRepository dpsEventRepository;

    @Test
    void getMatchingRulesByGuid() {
        String guid = UUID.randomUUID().toString();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(guid).build());
        when(dpsEventRepository.findDpsEventByGuidAndExecutionState(any(), any())).thenReturn(new ArrayList<>());

        var response = dpsEventService.getMatchingRulesByGuid(commonRequestModel);
        assertNotNull(response);
    }

    @Test
    void getMatchingRulesByGuid_Exception1() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().build());

        DpsException exception = assertThrows(DpsException.class, () -> {
            dpsEventService.getMatchingRulesByGuid(commonRequestModel);
        });
        assertEquals("Error in fetching object of DpsEvent: GUID can't be null. Please provide guid!", exception.getMessage());
    }

    @Test
    void getMatchingRulesByGuid_Exception2() {
        String guid = UUID.randomUUID().toString();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(guid).build());
        when(dpsEventRepository.findDpsEventByGuidAndExecutionState(any(), any())).thenThrow(new DpsException("null"));

        DpsException exception = assertThrows(DpsException.class, () -> {
            dpsEventService.getMatchingRulesByGuid(commonRequestModel);
        });
        assertEquals("Error in fetching object of DpsEvent: null", exception.getMessage());
    }
}