package com.dpw.runner.shipment.services.entity.fwb;

import com.dpw.runner.shipment.services.entity.SpecifiedAddressLocation;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.Valid;
import java.time.LocalDateTime;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ArrivalEventDto {

    @JsonProperty("ScheduledOccurrenceDateTime")
    // Date at which goods are arriving from the means of transport having been used for their carriage
    private LocalDateTime scheduledOccurrenceDateTime;

    @Valid
    @JsonProperty("OccurrenceArrivalLocation")
    private SpecifiedAddressLocation specifiedAddressLocation;

}
