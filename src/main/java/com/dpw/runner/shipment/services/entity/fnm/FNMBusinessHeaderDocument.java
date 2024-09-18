package com.dpw.runner.shipment.services.entity.fnm;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = false)
@ToString
public class FNMBusinessHeaderDocument {

    @Size(max = 70, message = "id exceeds the maximum size")
    @NotNull(message = "id is required")
    @JacksonXmlProperty(localName = "ID")
    private String id;

    @Size(max = 70, message = "name exceeds the maximum size")
    @NotNull(message = "name is required")
    @JacksonXmlProperty(localName = "Name")
    private String name;

    @NotNull(message = "Type code cannot be null")
    @Size(min = 1, max = 4, message = "Type Code Length is greater than 4" )
    @JacksonXmlProperty(localName = "TypeCode")
    private String typeCode;

    @Size(max = 15, message = "statusCode exceeds the maximum size")
    @NotNull(message = "Status code cannot be null")
    @JacksonXmlProperty(localName = "StatusCode")
    private String statusCode;
}
