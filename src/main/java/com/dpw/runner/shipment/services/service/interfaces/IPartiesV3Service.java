package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.v1.request.PartiesOrgAddressRequest;

import java.util.List;

public interface IPartiesV3Service {
    PartiesResponse create(PartiesRequest request);
    PartiesResponse update(PartiesRequest request);
    PartiesResponse get(PartiesOrgAddressRequest request);
    PartiesResponse delete(PartiesRequest request);
    List<PartiesResponse> list(ListCommonRequest listCommonRequest);
}
