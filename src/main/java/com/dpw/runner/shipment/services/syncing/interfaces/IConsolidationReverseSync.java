package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface IConsolidationReverseSync {
    ResponseEntity<?> reverseSync(CommonRequestModel request_);
}
