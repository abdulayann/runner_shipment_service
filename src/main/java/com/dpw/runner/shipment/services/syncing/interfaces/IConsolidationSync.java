package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import org.springframework.http.ResponseEntity;

public interface IConsolidationSync {
    ResponseEntity<?> sync(ConsolidationDetailsRequest request);
    ResponseEntity<?> reverseSync(CommonRequestModel request_);
}
