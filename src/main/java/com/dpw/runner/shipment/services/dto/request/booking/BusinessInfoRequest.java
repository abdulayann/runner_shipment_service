package com.dpw.runner.shipment.services.dto.request.booking;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class BusinessInfoRequest extends CommonRequest implements IRunnerRequest {

    @JsonProperty("product_name")
    private String productName;
    @JsonProperty("tenant_id")
    private String tenantId;
}
