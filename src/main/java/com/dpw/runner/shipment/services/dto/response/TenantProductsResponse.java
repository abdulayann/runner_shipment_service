package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.ProductType;
import lombok.Data;

import java.util.List;

@Data
public class TenantProductsResponse implements IRunnerResponse {

    private Long id;
    private ProductType productType;
    private String alias;
    private Integer priority;
    private Boolean enabled;
    private Boolean enableGrouping;
    private Long shipmentSettingsId;
    private Boolean isCommonSequence;
    private List<String> transportModes;
}
