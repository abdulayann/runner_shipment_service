package com.dpw.runner.booking.services.dto.request.platformBooking;

import com.dpw.runner.booking.services.commons.requests.CommonRequest;
import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
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
    private String reference_id;
    private UUID runner_guid;
    @JsonProperty("containerType")
    private String containerCode;
    @JsonProperty("commodity")
    private String commodityGroup;
    private BigDecimal grossWeight;
    private String grossWeightUnit;
    @JsonProperty("count")
    private Long containerCount;
}
