package com.dpw.runner.shipment.services.commons.dto.request.npm;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.util.List;

@Data
public class NPMOffersResponseParent extends CommonRequest implements IRunnerRequest {
    public String rate_type;
    public List<NPMOffer> rates;
}
