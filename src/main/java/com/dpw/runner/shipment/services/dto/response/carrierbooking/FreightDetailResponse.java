package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.PayerType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FreightDetailResponse implements IRunnerResponse {

    private Long id;   // from MultiTenancy / BaseEntity

    private String chargeType;
    private String paymentTerms;
    private PayerType payerType;
    private String payerLocation;
}
