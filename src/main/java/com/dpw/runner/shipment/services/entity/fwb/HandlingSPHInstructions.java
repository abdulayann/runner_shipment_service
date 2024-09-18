package com.dpw.runner.shipment.services.entity.fwb;

import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@NoArgsConstructor
@Builder
@Setter
@AllArgsConstructor
public class HandlingSPHInstructions {

    @JsonProperty("Description")
    @StringModifier(maxLength = 70, pattern = StringModifier.PatternType.ALPHA_NUMERIC)
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid handling sph instructions description provided")
    @Size(max = 70, message = "Handling sph instructions description can have max length {max}")
    private String description;

    @JsonProperty("DescriptionCode")
    @StringModifier(maxLength = 3, pattern = StringModifier.PatternType.ALPHA)
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid handling sph instructions description code provided")
    @Size(max = 3, message = "Handling sph instructions description code can have max length {max}")
    @NotNull(message = "Handling sph instructions description code cannot be null")
    //  Identifies the special handling code indicating that nature of consignment may necessitate use of special handling procedures
    private String descriptionCode;
}
