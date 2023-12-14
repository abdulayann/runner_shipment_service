package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class AwbOtherInfoV2 {

    // public Int64 entityId;
    public String entityType;
    public String shipper;
    public String carrier;
    public String executedAt;
    public LocalDateTime executedOn;
}
