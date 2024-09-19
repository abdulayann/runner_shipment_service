package com.dpw.runner.shipment.services.dto.TO.fzb;

import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class BusinessHeaderDocumentFZB {

    @JsonProperty("ID")
    @StringModifier(maxLength = 12, pattern = StringModifier.PatternType.ALPHA_NUMERIC)
    //@Pattern(regexp = "^[a-zA-Z0-9]*$", message = "id in business header must be alphanumeric")
    //@Size(max = 12, message = "id in business header exceeds the maximum size")
    @NotBlank(message = "id in business header is required")
    private String id;

//    @JsonProperty("IncludedHeaderNote")
//    private List<HeaderNoteFZB> includedHeaderNote;

    @JsonProperty("SignatoryConsignorAuthenticationName")
    @StringModifier(maxLength = 20, pattern = StringModifier.PatternType.TEXT)
    @NotBlank(message = "signatoryConsignorAuthenticationName in business header is required")
    private String signatoryConsignorAuthenticationName;

    @JsonProperty("SignatoryCarrierAuthentication")
    @NotNull
    @Valid
    private SignatoryCarrierAuthenticationFZB signatoryCarrierAuthentication;
}
