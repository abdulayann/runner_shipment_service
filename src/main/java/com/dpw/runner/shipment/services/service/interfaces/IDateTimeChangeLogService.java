package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import org.springframework.http.ResponseEntity;

public interface IDateTimeChangeLogService {

    ResponseEntity<IRunnerResponse> getDateTimeChangeLog(Long shipmentId);
    void deleteChangeLogs(Long shipmentId);
}
