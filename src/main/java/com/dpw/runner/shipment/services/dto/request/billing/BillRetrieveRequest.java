package com.dpw.runner.shipment.services.dto.request.billing;

import lombok.Getter;
import lombok.Setter;
import org.springframework.stereotype.Component;

@Getter
@Setter
@Component
public class BillRetrieveRequest extends BillingPaginatedRequest {

    private String moduleType;
    private String moduleGuid;
}
