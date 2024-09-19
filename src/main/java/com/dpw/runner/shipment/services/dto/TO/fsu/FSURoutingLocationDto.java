package com.dpw.runner.shipment.services.dto.TO.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class FSURoutingLocationDto {

    @JacksonXmlProperty(localName ="ID")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid Routing Location provided")
    @Size(max = 5, message = "Routing Location can have max length {max}")
    private String id;

    @JacksonXmlProperty(localName ="Name")
    @Size(max = 70, message = "Routing Location Name can have max length {max}")
    private String name;
}
