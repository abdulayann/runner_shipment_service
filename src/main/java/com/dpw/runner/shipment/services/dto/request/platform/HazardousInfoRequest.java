package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class HazardousInfoRequest implements Serializable {
    private String description;
    private String product_class;
    private String product_un_id;
    private String product_imco;
}

