package com.dpw.runner.shipment.services.service.handler;

import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowState;

/**
 * Interface for handling DPS workflow state transitions.
 * <p>
 * This interface defines the method to validate a transition between the current state and a new state in the DPS workflow. Each implementing class is responsible for defining the
 * specific rules for valid state transitions.
 * </p>
 */
public interface IDpsWorkflowStateHandler {

    void validateTransition(DpsWorkflowState currentState, DpsWorkflowState newState);
}
