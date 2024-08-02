package com.dpw.runner.shipment.services.commons.requests.billing;

import java.util.List;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.stereotype.Component;

@EqualsAndHashCode(callSuper = true)
@Data
@Component
public class ChargeTypeFilterRequest extends BillingPaginatedRequest {

    private Boolean isTaxable;
    private Boolean active;
    private String chargeGroup;
    private String arAccountCode;
    private String apAccountCode;
    private List<String> guidList;
    private List<String> chargeCodes;
    private List<String> transportModes;
}
