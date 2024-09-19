package com.dpw.runner.shipment.services.dto.TO.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;


@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class FSUDepartureEventDto {
    @JacksonXmlProperty(localName ="DepartureOccurrenceDateTime")
    @NotNull(message = "Departure occurrence date time cannot be null")
    private String departureOccurrenceDateTime;

    @JacksonXmlProperty(localName ="DepartureDateTimeTypeCode")
    @NotNull(message = "Departure date time type code cannot be null")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid departure date time type code provided")
    @Size(max = 1, message = "Departure date time type code can have max length {max}")
    private String departureDateTimeTypeCode;

}
