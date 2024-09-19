package com.dpw.runner.shipment.services.dto.TO.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.NotNull;


@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ScheduledArrivalEvent {
    @JacksonXmlProperty(localName ="ScheduledOccurrenceDateTime")
    @NotNull(message = "Scheduled arrival event date time cannot be null")
    private String scheduledArrivalEventDateTime;

}
