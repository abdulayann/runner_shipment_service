package com.dpw.runner.shipment.services.dto.TO.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@Getter
@NoArgsConstructor
@Builder
@Setter
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class FSUHandlingOSIInstructions {

    @JacksonXmlProperty(localName ="Description")
    @NotNull(message = "Handling OSI instructions description cannot be null")
    @Size(min = 1, max = 70, message = "Handling OSI instructions description can have max length {max}")
    private String description;

    @JacksonXmlProperty(localName ="DescriptionCode")
    @Size(max = 3, message = "Handling osi instructions description code can have max length {max}")
    private String descriptionCode;
}
