package com.dpw.runner.shipment.services.entity.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class AssociatedReceivedFromParty {

    @JacksonXmlProperty(localName ="AssociatedReceivedFromPartyPrimaryId")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid associated received from party primary id provided")
    @Size(max = 2, message = "Associated received from party primary id can have only max length {max}")
    @NotNull(message = "Associated received from party primary id cannot be null")
    private String primaryId;

    @JacksonXmlProperty(localName ="AssociatedReceivedFromPartyPrimaryIdSchemeAgentId")
    private String schemeAgentId = "1";

    @JacksonXmlProperty(localName ="AssociatedReceivedFromPartyName")
    @Size(max = 70, message = "Associated received from party name can have only max length {max}")
    @NotNull(message = "Associated received from party name cannot be null")
    private String name;

    @JacksonXmlProperty(localName ="AssociatedReceivedFromPartyRoleCode")
    @Size(max = 2, message = "Associated received from party role code can have only max length {max}")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid associated received from party role code provided")
    @NotNull(message = "Associated received from party role code cannot be null")
    private String roleCode;

    @JacksonXmlProperty(localName ="AssociatedReceivedFromPartyRole")
    @Size(max = 70, message = "Associated received from party role can have only max length {max}")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid associated received from party role provided")
    private String role;
}
