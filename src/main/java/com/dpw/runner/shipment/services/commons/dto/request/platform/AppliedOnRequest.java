package com.dpw.runner.shipment.services.commons.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class AppliedOnRequest {
    private List<RouteLegRequest> legs;
    private List<RouteNodeRequest> nodes;
}
