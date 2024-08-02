package com.dpw.runner.shipment.services.dto.response.billing;

import com.fasterxml.jackson.annotation.JsonInclude;
import java.io.Serializable;
import java.util.List;
import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ExternalBillResponse implements Serializable {

    private Map<String, Object> externalBillResponse;
    private List<Map<String, Object>> externalBillChargeResponse;
}
