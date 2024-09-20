package com.dpw.runner.shipment.services.dto.response.ExchangeRates;

import lombok.*;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class CountryCurrencyResponse {
    private String currency_code;
    private String currency_name;
    private String country_code;
    private String country_name;
}
