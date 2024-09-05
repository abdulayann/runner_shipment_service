package com.dpw.runner.shipment.services.dto.request.billing;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class LastPostedInvoiceDateRequest implements IRunnerRequest {
    private String moduleType;
    private String moduleGuid;
}
