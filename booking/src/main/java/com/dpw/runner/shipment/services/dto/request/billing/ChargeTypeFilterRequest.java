package com.dpw.runner.shipment.services.dto.request.billing;

import java.util.List;
import lombok.Getter;
import lombok.Setter;
import org.springframework.stereotype.Component;

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
