package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.commons.entity.Packing;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IPackingsSync {
    ResponseEntity<?> sync(List<Packing> packingList, String transactionId);
}
