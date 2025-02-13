package com.dpw.runner.shipment.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class OrgDataV1 {
    @JsonProperty("Id")
    private Long id;

    @JsonProperty("Guid")
    private String guid;

    @JsonProperty("OrganizationCode")
    private String organizationCode;

    @JsonProperty("OrganizationCategory")
    private String organizationCategory;

    @JsonProperty("ForworderAgent")
    private Boolean forworderAgent;

    @JsonProperty("FullName")
    private String fullName;

    @JsonProperty("Address1")
    private String address1;

    @JsonProperty("Address2")
    private String address2;

    @JsonProperty("Country")
    private String country;

    @JsonProperty("City")
    private String city;

    @JsonProperty("State")
    private String state;

    @JsonProperty("ZipPostCode")
    private String zipPostCode;

    @JsonProperty("Unloco")
    private String unloco;

    @JsonProperty("Email")
    private String email;

    @JsonProperty("LocalName")
    private String localName;

    @JsonProperty("AirAddress")
    private String airAddress;

    @JsonProperty("SeaAddress")
    private String seaAddress;

    @JsonProperty("Receivables")
    private Boolean receivables;

    @JsonProperty("Payables")
    private Boolean payables;
}
