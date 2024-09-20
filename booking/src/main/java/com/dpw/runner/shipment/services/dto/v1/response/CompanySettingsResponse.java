package com.dpw.runner.shipment.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CompanySettingsResponse {
    @JsonProperty("SeaLclContainerFlag")
    private Boolean SeaLclContainerFlag;
}
