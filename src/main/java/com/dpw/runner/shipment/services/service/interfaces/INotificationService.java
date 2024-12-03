package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import org.springframework.http.ResponseEntity;

public interface INotificationService {
    ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> acceptNotification(Long id);
    ResponseEntity<IRunnerResponse> confirmationMessage(Long id);
}