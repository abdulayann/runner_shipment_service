package com.dpw.runner.shipment.services.commons.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RouteLegRequest implements Serializable {
    private String order;
    private String origin_code;
    private String origin_location_type;
    private String destination_code;
    private String destination_location_type;
    private String transport_mode;
}
