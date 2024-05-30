package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import org.springframework.http.ResponseEntity;

public interface IShipmentSettingsReverseSync {
    ResponseEntity<IRunnerResponse> reverseSync(CommonRequestModel request);
}
