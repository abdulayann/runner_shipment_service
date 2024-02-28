package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

@Data
public class AwbSpecialHandlingCodesMappingInfoV2 implements IRunnerRequest {
    public String entityType;
    public String shcId;
}
