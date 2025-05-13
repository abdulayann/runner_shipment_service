package com.dpw.runner.shipment.services.dto.request;

import io.swagger.annotations.ApiModel;
import lombok.*;

@Setter
@Getter
@ApiModel("Container Details Request Model")
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ContainerDetailsRequest {
    private String entityType;
    private String entityId;
}
