package com.dpw.runner.shipment.services.dto.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

@Setter
@Getter
@Schema(description = "Cargo Details Request Model")
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CargoDetailsRequest {
    private String entityType;
    private String entityId;
}
