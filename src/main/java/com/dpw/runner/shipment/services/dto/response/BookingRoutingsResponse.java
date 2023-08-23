package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
public class BookingRoutingsResponse implements IRunnerResponse {
    private UUID reference_id;
    private UUID guid;
    private Long leg;
    @JsonProperty("origin")
    private String pol;
    @JsonProperty("destination")
    private String pod;
    @JsonProperty("transportMode")
    private String mode;
    private String carrier;
}
