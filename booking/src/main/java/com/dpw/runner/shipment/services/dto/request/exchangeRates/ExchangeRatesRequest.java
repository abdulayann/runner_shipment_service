package com.dpw.runner.shipment.services.dto.request.exchangeRates;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ExchangeRatesRequest extends CommonRequest implements IRunnerRequest {
    private List<String> country_code;
}
