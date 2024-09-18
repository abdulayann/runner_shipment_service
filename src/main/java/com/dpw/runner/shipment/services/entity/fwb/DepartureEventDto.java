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
public class DepartureEventDto {
    @JsonProperty("ScheduledOccurrenceDateTime")
    // Date at which goods are departing onto the means of transport being used for their carriage
    private LocalDateTime scheduledOccurrenceDateTime;

    @Valid
    @JsonProperty("OccurrenceDepartureLocation")
    private SpecifiedAddressLocation specifiedAddressLocation;

}
