package com.dpw.runner.shipment.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AddressDataV1 {
    @JsonProperty("Id")
    private Long id;

    @JsonProperty("AddressShortCode")
    private String addressShortCode;

    @JsonProperty("Guid")
    private String guid;

    @JsonProperty("CompanyName")
    private String companyName;

    @JsonProperty("Address1")
    private String address1;

    @JsonProperty("Address2")
    private String address2;

    @JsonProperty("OrgOrganizationCode")
    private String orgCode;

    @JsonProperty("AddressType")
    private String addressType;

    @JsonProperty("AdditionalAddressInfo")
    private String additionalAddressInfo;

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

    @JsonProperty("ContactPhone")
    private String contactPhone;

    @JsonProperty("Email")
    private String email;

    @JsonProperty("IECode")
    private String ieCode;

    @JsonProperty("BranchSINumber")
    private String branchSINumber;

    @JsonProperty("SiteIdentifier")
    private String siteIdentifier;

    @JsonProperty("OrgLocalName")
    private String orgLocalName;

    @JsonProperty("TaxRegNumber")
    private String taxRegNumber;

    @JsonProperty("ContactPerson")
    private String contactPerson;

    @JsonProperty("RakcType")
    private String rakcType;

    @JsonProperty("KCRANumber")
    private String kcraNumber;

    @JsonProperty("KCRAExpiry")
    private String kcraExpiry;

    @JsonProperty("RegulatedAgent")
    private Boolean regulatedAgent;

    @JsonProperty("KnownConsignor")
    private Boolean knownConsignor;
}
