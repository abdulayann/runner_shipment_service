package com.dpw.runner.shipment.services.service.handler;

import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowState;
import com.dpw.runner.shipment.services.exception.exceptions.DpsException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import javax.annotation.PostConstruct;
import org.springframework.stereotype.Component;

@Component
public class DpsWorkflowStateHandlerFactory {

    private final Map<DpsWorkflowState, IDpsWorkflowStateHandler> stateHandlers = new HashMap<>();

    @PostConstruct
    public void init() {
        stateHandlers.put(DpsWorkflowState.HOLD, new HoldStateHandler());
        stateHandlers.put(DpsWorkflowState.UN_HOLD, new UnHoldStateHandler());
        stateHandlers.put(DpsWorkflowState.TEMP_BLOCKED, new TempBlockedStateHandler());
        stateHandlers.put(DpsWorkflowState.PER_BLOCKED, new PermanentBlockedStateHandler());
        stateHandlers.put(DpsWorkflowState.UN_HOLD_WITH_CONDITION, new HoldStateHandler());
    }

    public IDpsWorkflowStateHandler getHandler(DpsWorkflowState currentState) {
        return Optional.ofNullable(stateHandlers.get(currentState))
                .orElseThrow(() -> new DpsException("No handler found for state: " + currentState));
    }
}
