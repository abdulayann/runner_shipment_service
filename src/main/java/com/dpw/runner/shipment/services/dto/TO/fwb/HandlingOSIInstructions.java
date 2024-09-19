package com.dpw.runner.shipment.services.dto.TO.fwb;

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
public class HandlingOSIInstructions {

    @JsonProperty("Description")
    @NotNull(message = "Handling OSI instructions description cannot be null")
    @Size(min = 1, max = 195, message = "Handling OSI instructions description can have max length {max}")
    private String description;

    @JsonProperty("DescriptionCode")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid handling osi instructions description code provided")
    @Size(max = 5, message = "Handling osi instructions description code can have max length {max}")
    private String descriptionCode;
}
