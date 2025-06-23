package com.dpw.runner.shipment.services.dto.request.billing;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class BillingSearchRequest extends BillingPaginatedRequest {

    private String searchString;
}
