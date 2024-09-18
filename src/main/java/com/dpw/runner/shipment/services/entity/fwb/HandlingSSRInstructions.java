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
public class HandlingSSRInstructions {

    @JsonProperty("Description")
    @StringModifier(maxLength = 195, pattern = StringModifier.PatternType.TEXT)
    @NotNull(message = "Handling SSR instructions description cannot be null")
    @Size(min = 1, max = 195, message = "Handling SSR instructions description can have max length {max}")
    // Service Type Text, Describes in free text the instructions for special action required
    private String description;

    @JsonProperty("DescriptionCode")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid handling ssr instructions description code provided")
    @Size(max = 5, message = "Handling ssr instructions description code can have max length {max}")
    //Identifies the code related to instructions for special action required
    private String descriptionCode;
}
