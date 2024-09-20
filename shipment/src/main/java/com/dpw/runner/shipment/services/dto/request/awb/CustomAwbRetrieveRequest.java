package com.dpw.runner.shipment.services.dto.request.awb;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.util.List;

@Data
public class CustomAwbRetrieveRequest implements IRunnerRequest {

    private List<String> awbNumber;
    private String issuingAgent;
}
