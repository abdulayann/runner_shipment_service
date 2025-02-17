package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
public class ProductSequenceConfigDto implements IRunnerRequest {
    @JsonProperty("TenantProductObj")
    public TenantProductsDto TenantProductObj;
    @JsonProperty("ProductProcessTypes")
    public String ProductProcessTypes;
    @JsonProperty("SequenceGroup")
    public String SequenceGroup;
    @JsonProperty("SequenceGroupForPaymentNoGen")
    public String SequenceGroupForPaymentNoGen;
    @JsonProperty("GenerationType")
    public String GenerationType;
    @JsonProperty("Prefix")
    public String Prefix;
    @JsonProperty("SerialCounter")
    public Integer SerialCounter;
    @JsonProperty("sequenceStartTime")
    public LocalDateTime sequenceStartTime;
    @JsonProperty("Guid")
    private UUID Guid;
}
