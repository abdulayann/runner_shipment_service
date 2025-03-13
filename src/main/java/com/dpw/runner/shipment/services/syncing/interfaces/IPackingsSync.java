package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.Packing;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IPackingsSync {
    ResponseEntity<IRunnerResponse> sync(List<Packing> packingList, String transactionId);
}
