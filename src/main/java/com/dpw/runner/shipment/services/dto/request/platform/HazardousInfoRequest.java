package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data @Builder
@NoArgsConstructor
@AllArgsConstructor
public class HazardousInfoRequest {
    private String description;
    private String product_class;
    private String product_un_id;
    private String product_imco;
}

