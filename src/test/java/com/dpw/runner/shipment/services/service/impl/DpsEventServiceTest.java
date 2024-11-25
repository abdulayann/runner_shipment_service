package com.dpw.runner.shipment.services.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.entity.DpsEvent;
import com.dpw.runner.shipment.services.entity.enums.DpsEntityType;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowType;
import com.dpw.runner.shipment.services.exception.exceptions.DpsException;
import com.dpw.runner.shipment.services.repository.interfaces.IDpsEventRepository;
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
}