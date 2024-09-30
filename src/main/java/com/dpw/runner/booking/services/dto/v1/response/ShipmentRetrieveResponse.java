package com.dpw.runner.booking.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.util.UUID;

@Data
@Builder
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class ShipmentRetrieveResponse {
    @JsonProperty("Guid")
    UUID guid;
}
