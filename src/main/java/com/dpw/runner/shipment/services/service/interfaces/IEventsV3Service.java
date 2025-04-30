package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;

import java.util.List;

public interface IEventsV3Service {
    List<EventsResponse> listV2(CommonRequestModel commonRequestModel, String source);
}
