package com.dpw.runner.shipment.services.dto.response.billing;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ExternalBillResponse implements Serializable {

    @JsonProperty("externalBillResponse")
    private transient Map<String, Object> billResponse;
    @JsonProperty("externalBillChargeResponse")
    private transient List<Map<String, Object>> billChargeResponse;
}
