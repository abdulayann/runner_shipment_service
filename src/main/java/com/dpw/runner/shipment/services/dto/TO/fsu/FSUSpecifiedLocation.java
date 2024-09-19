package com.dpw.runner.shipment.services.dto.TO.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class FSUSpecifiedLocation {

    @JacksonXmlProperty(localName ="ID")
    @Size(max = 5, message = "Location Id can have max length {max}")
    @NotNull(message = "Specified location id cannot be null")
    private String id;

    @JacksonXmlProperty(localName ="Name")
    @Size(max = 70, message = "Location Name can have max length {max}")
    private String name;


    @JacksonXmlProperty(localName ="TypeCode")
    @Size(max = 35, message = "Location Name can have max length {max}")
    private String typeCode;

    @JacksonXmlProperty(localName ="FlightStatusTypeCode")
    @Size(max = 15, message = "Invalid Flight status type code can have max length {max}")
    @NotNull(message = "Flight status type code cannot be null")
    private String flightStatusTypeCode;

}
