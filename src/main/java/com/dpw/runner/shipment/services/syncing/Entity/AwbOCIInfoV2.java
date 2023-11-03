package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

@Data
public class AwbOCIInfoV2 {
    // public Integer entityId;
    public String entityType;
    public Integer informationIdentifier;
    public Integer tradeIdentificationCode;
    public String tradeIdentificationComment;
}
