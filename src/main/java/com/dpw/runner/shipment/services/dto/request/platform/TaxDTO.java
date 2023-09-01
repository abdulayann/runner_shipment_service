package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
public class TaxDTO {
    private String type;
    private double value;
    private String tax_type;
}
