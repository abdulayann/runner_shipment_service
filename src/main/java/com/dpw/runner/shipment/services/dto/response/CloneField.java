package com.dpw.runner.shipment.services.dto.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
class CloneField {
    private String label;
    private String value;
    @JsonProperty("is_disabled")
    private boolean isDisabled;
}
