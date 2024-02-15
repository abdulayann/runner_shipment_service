package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Setter;

@Setter
public class AwbChargeTypeMasterDataResponse implements IRunnerResponse {
    private String iataDescription;
    private Integer chargeDue;
}
