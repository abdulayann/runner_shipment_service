package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

@Data
public class AwbOCIInfoV2 implements IRunnerRequest {
    public String entityType;
    public Integer informationIdentifier;
    public Integer tradeIdentificationCode;
    public String tradeIdentificationComment;
}
