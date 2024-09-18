package com.dpw.runner.shipment.services.entity.fzb;
import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class PartyFZB {

    //@NotBlank(message = "PrimaryID is mandatory")
    @Size(max = 35, message = "PrimaryID must be at most 35 characters long")
    private String primaryID;

    @Size(max = 35, message = "AdditionalID must be at most 35 characters long")
    private String additionalID;

    @JsonProperty("Name")
    @NotBlank(message = "Name is mandatory")
    @StringModifier(maxLength = 35, pattern = StringModifier.PatternType.TEXT)
    @Size(max = 35, message = "Name must be at most 35 characters long")
    private String name;

    @Pattern(regexp = "^[A-Za-z0-9]{1,14}$", message = "AccountID must be alphanumeric and have max 14 characters")
    private String accountID;

    @JsonProperty("PartyAddress")
    @Valid
    @NotNull(message = "PostalStructuredAddress is mandatory")
    private PostalStructuredAddressFZB postalStructuredAddress;

    @Valid
    private DefinedTradeContact definedTradeContact;
}
