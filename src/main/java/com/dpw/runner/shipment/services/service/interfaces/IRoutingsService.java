package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.RoutingsUpdateRequest;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import java.util.List;
import java.util.concurrent.ExecutionException;
import org.springframework.http.ResponseEntity;

public interface IRoutingsService {

    void updateRoutingsBasedOnTracking(Long shipmentId, List<Routings> routings)
        throws RunnerException, ExecutionException, InterruptedException;

    ResponseEntity<IRunnerResponse> updateRoutings(RoutingsUpdateRequest routingsUpdateRequest);

    RoutingsResponse routingsToRoutingsResponse(Routings routings);

    List<RoutingsResponse> routingsListToRoutingsResponseList(List<Routings> list);
}
