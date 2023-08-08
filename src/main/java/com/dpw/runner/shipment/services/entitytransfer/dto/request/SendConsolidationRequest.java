package com.dpw.runner.shipment.services.entitytransfer.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;

import java.util.List;
import java.util.Map;

public class SendConsolidationRequest implements IRunnerRequest {
    private Long consolId;
    private List<Long> sendToBranch;
    private List<String> sendToOrg;
    private Map<Long, List<Long>> additionalDocs;
}
