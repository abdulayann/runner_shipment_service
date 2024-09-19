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
public class FSUBusinessHeaderDocument {

    @JacksonXmlProperty(localName ="ID")
    @NotNull(message = "Business header document id cannot be null")
    @Size(max = 70, message = "Business header document id can have max length {max}")
    private String id;
}
