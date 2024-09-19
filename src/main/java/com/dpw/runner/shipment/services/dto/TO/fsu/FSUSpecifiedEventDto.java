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
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class FSUSpecifiedEventDto {
    @JacksonXmlProperty(localName ="OccurrenceDateTime")
    @NotNull(message = "Occurrence date time cannot be null")
    private String occurrenceDateTime;

    @JacksonXmlProperty(localName ="DateTimeTypeCode")
    @NotNull(message = "Occurrence date time type code cannot be null")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid Occurrence date time type code provided")
    @Size(max = 1, message = "Occurrence date time type code can have max length {max}")
    private String dateTimeTypeCode;

}
