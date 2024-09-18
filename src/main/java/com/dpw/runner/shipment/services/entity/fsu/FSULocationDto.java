package com.dpw.runner.shipment.services.entity.fsu;

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
public class FSULocationDto {
    @JacksonXmlProperty(localName ="ID")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid Origin Location provided")
    @Size(max = 5, message = "Origin Location can have max length {max}")
    @NotNull(message = "Origin Location code cannot be null")
    private String id;

    @JacksonXmlProperty(localName ="Name")
    @Size(max = 70, message = "Origin Location Name can have max length {max}")
    private String name;
}
