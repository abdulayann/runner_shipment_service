package com.dpw.runner.shipment.services.commons.dto.response.ExchangeRates;

import lombok.*;

import java.util.List;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ExchangeRatesResponse {
    private List<CountryCurrencyResponse> data;
}
