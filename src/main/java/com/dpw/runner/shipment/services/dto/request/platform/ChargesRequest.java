package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ChargesRequest {
    private String description;
    private String charge_code;
    private String charge_value;
    private String charge_currency;
    private TaxDTO tax;
    private String bill_charge_id;
    private List<String> container_details;
}
