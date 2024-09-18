package com.dpw.runner.shipment.services.entity.fzb;
import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class LocationFZB {

    @JsonProperty("Id")
    @NotBlank(message = "Airport IATA code is mandatory")
    @Pattern(regexp = "[A-Za-z]{3}", message = "Airport IATA code must consist of 3 alphabetic characters")
    private String id;

    @JsonProperty("Name")
    @Size(max = 70, message = "Name must be at most 70 characters long")
    @StringModifier(maxLength = 70, pattern = StringModifier.PatternType.TEXT)
    private String name;

    @JsonProperty("TypeCode")
    @Size(max = 35, message = "TypeCode must be at most 35 characters long")
    private String typeCode;
}
