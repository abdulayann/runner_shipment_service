package com.dpw.runner.shipment.services.commons.requests.billing;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;


/**
 * @author kaushikyelne Created by kaushikyelne. Date: 13/07/23
 */
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class BillingSearchRequest extends BillingPaginatedRequest {

    private String searchString;
}
