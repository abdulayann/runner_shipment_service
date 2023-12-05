package com.dpw.runner.shipment.services.dto.request;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class CustomAutoEventRequest {
    public long entityId;
    public String entityType;
    public String eventCode;
    public Boolean isEstimatedRequired = true;
    public Boolean isActualRequired = true;
}
