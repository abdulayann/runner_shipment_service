package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.Hbl;
import org.springframework.http.ResponseEntity;

public interface IHblSync {
    ResponseEntity<IRunnerResponse> sync(Hbl hbl, String transactionId);
}
