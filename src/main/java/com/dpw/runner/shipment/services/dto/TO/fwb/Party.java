package com.dpw.runner.shipment.services.dto.TO.fwb;

import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Party {
    @JsonProperty("PrimaryID")
    @Valid
    // todo; check
    private IDdto primaryID;

    @JsonProperty("AdditionalID")
    @Pattern(regexp = "^[a-zA-Z0-9]$", message = "Party additional ID can have max length {max}")
    @Size(max = 35, message = "Additional Id can have max length {max}")
    private String additionalID;

    @JsonProperty("Name")
    @StringModifier(maxLength = 70, pattern = StringModifier.PatternType.TEXT)
    @Size(max = 70, message = "Party name can have max length {max}")
    @NotNull(message = "Party name cannot be null")
    private String name;

    @JsonProperty("AccountID")
    @Size(max = 14, message = "Party account id can be of max length {max}")
    private String accountID;

    @JsonProperty("CargoAgentId")
    @Size(max = 14, message = "Cargo Agent id can be of max length {max}")
    // TODO: Mandatory for Frieght Forwarder party
    // TODO Consitional: Last digit is unweighted modulus 7 check digit of IATA Cargo Agent Code and first 3 digits of IATA Cargo Agent CASS Address
    private String cargoAgentId;

    /** Comment: RoleCode, role are only applicable for Associated party */

    @JsonProperty("RoleCode")
    // TODO: Mandatory for Associated Party, 1:1
    @Size(max = 2, message = "Party role code can only have max length {max}")
    private String roleCode;

    @JsonProperty("Role")
    @Size(max = 70, message = "Party role code can only have max length {max}")
    private String role;

    @Valid
    @JsonProperty("PartyAddress")
    @NotNull(message = "postal structured address cannot be null")
    private PartyAddress partyAddress;

    /** Comment: specifiedCargoAgentLocationId is applicable only in case of FF party */

    @JsonProperty("SpecifiedCargoAgentLocationId")
    @Size(max = 4, message = "Specified cargo agent location id can have max length {max}")
    // TODO: Conditional -> Only occurs in addition to IATA Cargo Agent Code
    private String specifiedCargoAgentLocationId;

    @Valid
    @JsonProperty("DefinedTradeContact")
    private List<DefinedTradeContactDto> definedTradeContacts;
}
