package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.syncing.Entity.CustomConsolidationRequest;
import org.springframework.http.ResponseEntity;

public interface IConsolidationSync {
    ResponseEntity<IRunnerResponse> sync(ConsolidationDetails request, String transactionId, boolean isDirectSync) throws RunnerException;

    void syncLockStatus(ConsolidationDetails consolidationDetails);

    CustomConsolidationRequest createConsoleSyncReq(ConsolidationDetails request);
}
