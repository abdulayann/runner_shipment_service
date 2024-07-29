package com.dpw.runner.shipment.services.commons.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
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
