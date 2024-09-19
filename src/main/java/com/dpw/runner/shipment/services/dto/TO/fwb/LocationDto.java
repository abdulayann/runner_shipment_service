package com.dpw.runner.shipment.services.dto.TO.fwb;

import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class LocationDto {
    @JsonProperty("ID")
    /** Contains the code of the origin location */
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid Origin/Destination Location provided")
    @Size(max = 5, message = "Origin/Destination Location can have max length {max}")
    @NotNull(message = "airport IATA code cannot be null")
    private String id;

    @JsonProperty("Name")
    @StringModifier(maxLength = 70, pattern = StringModifier.PatternType.TEXT)
    //@Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid Origin/Destination Location Name provided")
    @Size(max = 70, message = "Origin/Destination Location Name can have max length {max}")
    private String name;
}
