package com.dpw.runner.shipment.services.commons.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TenantIdResponse {
    @JsonProperty("Id")
    private int id;
}
