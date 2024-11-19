package com.dpw.runner.shipment.services.service.handler;

import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowState;
import com.dpw.runner.shipment.services.exception.exceptions.DpsException;
import java.util.Set;

public class HoldStateHandler implements IDpsWorkflowStateHandler {

    private static final Set<DpsWorkflowState> VALID_NEXT_STATES = Set.of(
            DpsWorkflowState.HOLD,
            DpsWorkflowState.UN_HOLD,
            DpsWorkflowState.TEMP_BLOCKED,
            DpsWorkflowState.PER_BLOCKED,
            DpsWorkflowState.UN_HOLD_WITH_CONDITION
    );

    /**
     * Validates the transition between the current and new DPS workflow states.
     *
     * <p>This method checks whether a transition from the current state to the new state is valid based on the state rules.
     * If the current state is null, or if it is "HOLD" and the new state is one of the valid next states, the transition is allowed. If the transition is invalid, an exception is
     * thrown to indicate the invalid transition.</p>
     *
     * @param currentState The current DPS workflow state.
     * @param newState     The new DPS workflow state to transition to.
     * @throws DpsException if the transition is invalid.
     */
    @Override
    public void validateTransition(DpsWorkflowState currentState, DpsWorkflowState newState) {
        // If the current state is null, consider the transition as valid
        // This allows the system to accept a transition from a null state to any valid state.
        if (currentState == null || (currentState == DpsWorkflowState.HOLD && VALID_NEXT_STATES.contains(newState))) {
            return;  // Valid transition, no action needed
        }

        // Throw exception if the transition is invalid (current state is not null and not a valid transition)
        throw new DpsException("Invalid transition from " + currentState + " to " + newState);
    }

}