package com.dpw.runner.shipment.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.util.Map;

@Data
@Builder
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class OrgAddressResponse {
    @JsonProperty("Organizations")
    Map<String, Map<String, Object>> organizations;
    @JsonProperty("Addresses")
    Map<String, Map<String, Object>> addresses;
}