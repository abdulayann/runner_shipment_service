package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.response.BulkReferenceNumbersResponse;
import com.dpw.runner.shipment.services.dto.response.ReferenceNumbersResponse;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IReferenceNumbersV3Service {
    ReferenceNumbersResponse create(ReferenceNumbersRequest request);

    ReferenceNumbersResponse update(ReferenceNumbersRequest request);

    BulkReferenceNumbersResponse updateBulk(List<ReferenceNumbersRequest> requests);

    ResponseEntity<IRunnerResponse> listReferenceNumbers(ListCommonRequest request, String xSource);

    ReferenceNumbersResponse delete(ReferenceNumbersRequest request);

    List<ReferenceNumbersResponse> list(ListCommonRequest listCommonRequest, String xSource);
}
