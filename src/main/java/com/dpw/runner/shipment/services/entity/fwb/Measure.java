package com.dpw.runner.shipment.services.entity.fwb;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.NotNull;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Measure {
    @JsonProperty("unitCode")
    @NotNull(message = "Unit Code cannot be null")
    private String unitCode;
    @JsonProperty("unitCodeSpecified")
    private Boolean unitCodeSpecified;
    @JsonProperty("value")
    private Double value;
}
