package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.SailingScheduleRequest;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface ISailingScheduleService {

    ResponseEntity<IRunnerResponse> create(List<SailingScheduleRequest> requestList);

}