package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.json.JSONObject;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.io.IOException;

@Service
public interface IAirMessagingPayloadService {

    ResponseEntity<JSONObject> getFailedPayloadById(Long id) throws RunnerException, IOException;

    ResponseEntity<IRunnerResponse> resubmitFailedPayload(Object payload, String messageType) throws RunnerException;

}
