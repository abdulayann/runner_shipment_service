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
public class FSUArrivalEventDto {
    @JacksonXmlProperty(localName ="ArrivalOccurrenceDateTime")
    @NotNull(message = "Arrival occurrence date time cannot be null")
    private String arrivalOccurrenceDateTime;

    @JacksonXmlProperty(localName ="ArrivalDateTimeTypeCode")
    @NotNull(message = "Arrival date time type code cannot be null")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid Arrival date time type code provided")
    @Size(max = 1, message = "Arrival date time type code can have max length {max}")
    private String arrivalDateTimeTypeCode;

}
