package com.dpw.runner.booking.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Data
@Builder
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class V1RetrieveResponse {
    @JsonProperty("Entity")
    Object entity;
}
