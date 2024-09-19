package com.dpw.runner.shipment.services.dto.TO.fzb;

import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class SignatoryCarrierAuthenticationFZB {

    @JsonProperty("ActualDateTime")
    @NotNull(message = "actualDateTime is required")
    private String actualDateTime;

    @JsonProperty("Signatory")
    @StringModifier(maxLength = 20, pattern = StringModifier.PatternType.TEXT)
    @Size(max = 20, message = "signatory exceeds the maximum size")
    @NotBlank(message = "signatory is required")
    private String signatory;

    @JsonProperty("IssueAuthenticationLocationName")
    @StringModifier(maxLength = 35, pattern = StringModifier.PatternType.ALPHA_NUMERIC)
    @Size(max = 35, message = "issueAuthenticationLocationName exceeds the maximum size")
    private String issueAuthenticationLocationName;
}

