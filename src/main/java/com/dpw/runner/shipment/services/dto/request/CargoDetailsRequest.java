package com.dpw.runner.shipment.services.dto.request;

import io.swagger.annotations.ApiModel;
import lombok.*;

@Setter
@Getter
@ApiModel("Cargo Details Request Model")
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CargoDetailsRequest {
    private String entityType;
    private String entityId;
}
