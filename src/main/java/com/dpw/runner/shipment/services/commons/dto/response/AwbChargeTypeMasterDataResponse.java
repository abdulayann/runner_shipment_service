package com.dpw.runner.shipment.services.commons.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

@Data
public class AwbChargeTypeMasterDataResponse implements IRunnerResponse {
    private String iataDescription;
    private Integer chargeDue;
}
