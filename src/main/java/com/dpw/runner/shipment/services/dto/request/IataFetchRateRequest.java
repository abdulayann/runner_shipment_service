package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class IataFetchRateRequest implements IRunnerRequest {
    private BigDecimal chargeableWeight;
    private String originPort;
    private String destinationPort;
    private String flightCarrier;
    private String currency;
}
