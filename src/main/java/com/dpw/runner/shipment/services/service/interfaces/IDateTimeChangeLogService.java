package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.response.UpstreamDateUpdateResponse;
import com.dpw.runner.shipment.services.entity.DateTimeChangeLog;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IDateTimeChangeLogService {

    DateTimeChangeLog create(ShipmentDetails shipmentDetails, UpstreamDateUpdateResponse upstreamDates);

    List<DateTimeChangeLog> getDateTimeChangeLog(Long shipmentId);

    void deleteDateTimeLogs(Long shipmentId);
}
