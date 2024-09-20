package com.dpw.runner.shipment.services.commons.requests;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SortRequest {
    private String fieldName;
    private String order;
}
