package com.dpw.runner.shipment.services.dto.TO.fwb;

import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SignatoryCarrierAuthentication {
    @JsonProperty("ActualDateTime")
    @NotNull(message = "Signatory Carrier Authentication actual date time cannot be null")
    private String actualDateTime;

    @JsonProperty("Signatory")
    @StringModifier(maxLength = 20, pattern = StringModifier.PatternType.TEXT)
    @NotNull(message = "Signatory carrier authentication signatory cannot be null")
    @Size(min = 1, max = 20, message = "Signatory carrier authentication signatory can be of max length {max}")
    //@Pattern(regexp = "^[a-zA-Z0-9.\\- ]*$", message = "signatory FWB Pattern mismatch")
    private String signatory;

    @JsonProperty("IssueAuthenticationLocationName")
    @StringModifier(maxLength = 35, pattern = StringModifier.PatternType.TEXT)
    @NotNull(message = "Signatory carrier authentication issue authentication location name cannot be null")
    @Size(min = 1, max = 35, message = "Signatory carrier authentication issue authentication location name can be of max length {max}")
    private String issueAuthenticationLocationName;
}
