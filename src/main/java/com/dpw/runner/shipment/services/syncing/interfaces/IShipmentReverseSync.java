package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface IShipmentReverseSync {
    ResponseEntity<?> reverseSync(CommonRequestModel request, boolean checkForSync);
}
