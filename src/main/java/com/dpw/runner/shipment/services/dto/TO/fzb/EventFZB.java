package com.dpw.runner.shipment.services.dto.TO.fzb;
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
public class EventFZB {

    @JsonProperty("ScheduledOccurrenceDateTime")
    //@NotNull(message = "ScheduledOccurrenceDateTime is mandatory")
    private LocalDateTime scheduledOccurrenceDateTime;

    @JsonProperty("OccurrenceLocation")
    @Valid
    private SpecifiedAddressLocation specifiedAddressLocation;
}
