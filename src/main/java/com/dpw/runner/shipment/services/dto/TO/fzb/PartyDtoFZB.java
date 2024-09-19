package com.dpw.runner.shipment.services.dto.TO.fzb;

import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.NotBlank;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PartyDtoFZB {

    @JsonProperty("schemeID")
    @NotBlank(message = "schemeID in party is required")
    private String schemeID;

    @JsonProperty("value")
    @NotBlank(message = "value in party is required")
    @StringModifier(maxLength = 70, pattern = StringModifier.PatternType.TEXT)
    private String value;
}
