package com.dpw.runner.shipment.services.service.TO.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbKafkaEntity{
    public String shipmentId;
    public String createdAt;
    public Integer tenantId;
    public String updatedBy;
}
