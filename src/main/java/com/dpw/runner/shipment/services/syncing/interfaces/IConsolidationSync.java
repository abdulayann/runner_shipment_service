package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import org.springframework.http.ResponseEntity;

public interface IConsolidationSync {
    ResponseEntity<IRunnerResponse> sync(ConsolidationDetails request, String transactionId, boolean isDirectSync);
    void syncLockStatus(ConsolidationDetails consolidationDetails);
}
