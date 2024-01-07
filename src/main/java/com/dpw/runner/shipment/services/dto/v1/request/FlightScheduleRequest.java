package com.dpw.runner.shipment.services.dto.v1.request;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FlightScheduleRequest {
    @JsonProperty("EqualityFilter")
    Map<String, String> EqualityFilter;
    @JsonProperty("IncludeColumns")
    Object IncludeColumns;
    @JsonProperty("Take")
    Integer Take;
}
