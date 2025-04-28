package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import org.springframework.http.ResponseEntity;

public interface IShipmentServiceV3 {

    ResponseEntity<IRunnerResponse> getPendingNotificationCount();

    ResponseEntity<IRunnerResponse> listShipment(CommonRequestModel commonRequestModel, boolean getMasterData);

    ResponseEntity<IRunnerResponse> getPendingNotificationData(CommonGetRequest request);
}