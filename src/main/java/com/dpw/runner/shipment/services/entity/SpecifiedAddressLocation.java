package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SpecifiedAddressLocation {

    @JsonProperty("ID")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Specified Location Id pattern mismatch")
    @Size(max = 5, message = "Location Id can have max length {max}")
    // TODO: 3 (IATA Airport Code) , 5 (UNLOCODE)
    /** Code name of a seaport, airport, freight terminal, rail station or other place. */
    private String id;

    @JsonProperty("Name")
    @StringModifier(maxLength = 70, pattern = StringModifier.PatternType.TEXT)
    //@Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Specified Location Name pattern mismatch")
    @Size(max = 70, message = "Location Name can have max length {max}")
    private String name;

    @JsonProperty("TypeCode")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Specified Location TypeCode pattern mismatch")
    @Size(max = 35, message = "Specified Location TypeCode can have max length {max}")
    // TODO:  Field can be hardcoded to the following codes "Seaport", "Airport", "Freight Terminal", "Rail Station", "Other"
    private String typeCode;
}
