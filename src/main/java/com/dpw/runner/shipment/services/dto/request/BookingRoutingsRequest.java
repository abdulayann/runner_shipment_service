package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
public class BookingRoutingsRequest extends CommonRequest implements IRunnerRequest {
    private UUID reference_id;
    private Long leg;
    @JsonProperty("origin")
    private String pol;
    @JsonProperty("destination")
    private String pod;
    @JsonProperty("transportMode")
    private String mode;
    private String carrier;
}
