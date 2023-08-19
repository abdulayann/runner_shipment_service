package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class BookingContainerRequest extends CommonRequest implements IRunnerRequest {
    private UUID reference_id;
    @JsonProperty("containerType")
    private String containerCode;
    @JsonProperty("commodity")
    private String commodityCode;
    private BigDecimal grossWeight;
    private String grossWeightUnit;
    @JsonProperty("count")
    private Long containerCount;
}
