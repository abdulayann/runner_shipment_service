package com.dpw.runner.shipment.services.service.handler;

import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowState;
import com.dpw.runner.shipment.services.exception.exceptions.DpsException;
import java.util.Set;

public class PermanentBlockedStateHandler implements IDpsWorkflowStateHandler {

    private static final Set<DpsWorkflowState> VALID_NEXT_STATES = Set.of();

    /**
     * Validates the transition between the current and new DPS workflow states.
     *
     * <p>This method ensures that a transition from the current state to the new state is allowed based on defined rules.
     * If the current state is null, or if the current state is "PER_BLOCKED" and the new state is one of the valid next states, the transition is allowed. If the transition is
     * invalid, an exception is thrown.</p>
     *
     * @param currentState The current DPS workflow state.
     * @param newState     The new DPS workflow state to transition to.
     * @throws DpsException if the transition is invalid.
     */
    @Override
    public void validateTransition(DpsWorkflowState currentState, DpsWorkflowState newState) {
        // If the current state is null, consider the transition as valid (null state is treated as a starting point)
        // This allows any transition when the current state is not yet set.
        // If the current state is PER_BLOCKED and the new state is valid (exists in VALID_NEXT_STATES), the transition is valid
        if (currentState == null || (currentState == DpsWorkflowState.PER_BLOCKED && VALID_NEXT_STATES.contains(newState))) {
            return;  // Valid transition, no action needed
        }

        // If the transition doesn't meet the criteria, throw an exception indicating the invalid state transition
        throw new DpsException("Invalid transition from " + currentState + " to " + newState);
    }

}
