package com.dpw.runner.shipment.services.dto.request.billing;

import lombok.Getter;
import lombok.Setter;
import org.springframework.stereotype.Component;

import java.util.List;

@Getter
@Setter
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
