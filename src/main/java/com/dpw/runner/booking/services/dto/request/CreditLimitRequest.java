package com.dpw.runner.booking.services.dto.request;

import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CreditLimitRequest implements IRunnerRequest {
    private String customerIdentifierId;
    private String siteIdentifierId;
    private String clientOrgCode;
    private String clientAddressCode;
}
