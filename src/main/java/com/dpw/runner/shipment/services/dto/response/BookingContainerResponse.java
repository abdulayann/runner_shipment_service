package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class BookingContainerResponse implements IRunnerResponse {
    private UUID reference_id;
    private UUID guid;
    private UUID runner_guid;
    @JsonProperty("containerType")
    private String containerCode;
    @JsonProperty("commodity")
    private String commodityCode;
    private BigDecimal grossWeight;
    private String grossWeightUnit;
    @JsonProperty("count")
    private Long containerCount;
}
