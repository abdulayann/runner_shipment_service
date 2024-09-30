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
public class CheckCreditLimitFromV1Request implements IRunnerRequest {
    private Long shipmentId;
    private String docType;
    private Boolean taskCreation;
}
