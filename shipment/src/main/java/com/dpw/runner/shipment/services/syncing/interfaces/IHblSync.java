package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.entity.Hbl;
import org.springframework.http.ResponseEntity;

public interface IHblSync {
    ResponseEntity<?> sync(Hbl hbl, String transactionId);
}
