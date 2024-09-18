package com.dpw.runner.shipment.services.service.TO;

import com.dpw.runner.shipment.services.service.TO.response.DescartesResponse;
import com.dpw.runner.shipment.services.service.TO.response.DescartesStatusResponse;
import org.springframework.stereotype.Service;

@Service
public interface IDescartesAdapter {

    DescartesResponse pushAirWayBillDetails(String base64EncodedContent);

    void updateEvents(String json);

    void updateStatus(String data);

    DescartesStatusResponse getStatusResponse(String entityId);
}