package com.dpw.runner.shipment.services.dto.request;

import lombok.*;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ContainerDetailsRequest {
    private String entityType;
    private String entityId;
}
