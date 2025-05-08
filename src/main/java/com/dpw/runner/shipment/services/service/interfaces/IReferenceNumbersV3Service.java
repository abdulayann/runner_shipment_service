package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.response.ReferenceNumbersResponse;

import java.util.List;

public interface IReferenceNumbersV3Service {
    ReferenceNumbersResponse create(ReferenceNumbersRequest request);
    ReferenceNumbersResponse update(ReferenceNumbersRequest request);
    ReferenceNumbersResponse delete(ReferenceNumbersRequest request);
    List<ReferenceNumbersResponse> list(ListCommonRequest listCommonRequest, String xSource);
}
