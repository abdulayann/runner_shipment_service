package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import org.springframework.http.ResponseEntity;

public interface IConsolidationSync {
    ResponseEntity<?> sync(ConsolidationDetails request);
}
