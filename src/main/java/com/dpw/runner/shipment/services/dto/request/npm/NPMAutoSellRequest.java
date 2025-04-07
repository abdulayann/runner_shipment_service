package com.dpw.runner.shipment.services.dto.request.npm;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.Awb;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.List;

@Data
@SuppressWarnings("java:S125")
public class NPMAutoSellRequest implements IRunnerRequest {
    @JsonProperty("awb")
    private Awb awb;
    @JsonProperty("EntityId")
    private String EntityId;
    @JsonProperty("EntityType")
    private String EntityType;  // Under EntityType : send either DMAWB or MAWB, this api is also used by shipment;
    @JsonProperty("isP100Tenant")
    private Boolean isP100Tenant;
    @JsonProperty("mandatoryChargeCodes")
    private List<String> mandatoryChargeCodes;
}
