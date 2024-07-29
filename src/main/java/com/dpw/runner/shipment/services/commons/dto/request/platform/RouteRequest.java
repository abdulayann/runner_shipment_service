package com.dpw.runner.shipment.services.commons.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class RouteRequest implements Serializable {
    private List<RouteLegRequest> legs;
    // Getters and Setters
}

