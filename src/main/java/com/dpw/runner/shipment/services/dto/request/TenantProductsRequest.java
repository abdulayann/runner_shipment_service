package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.ProductType;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Getter
@Setter
@ApiModel("Tenant Products Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TenantProductsRequest extends CommonRequest implements IRunnerRequest {

    private Long id;
    private ProductType productType;
    private String alias;
    private Integer priority;
    private Boolean enabled;
    private Boolean enableGrouping;
    private Long shipmentSettingsId;
}
