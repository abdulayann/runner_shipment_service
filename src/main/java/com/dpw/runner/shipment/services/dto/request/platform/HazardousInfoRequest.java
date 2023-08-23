package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class HazardousInfoRequest {
    private String description;
    private double product_class;
    private int product_un_id;
    private String product_imco;
}

