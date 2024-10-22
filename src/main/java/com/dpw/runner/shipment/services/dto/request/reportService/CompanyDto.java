package com.dpw.runner.shipment.services.dto.request.reportService;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.io.Serializable;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
@JsonIgnoreProperties
public class CompanyDto implements Serializable {
    @JsonProperty("Id")
    private Integer id;
    @JsonProperty("CompanyName")
    private String companyName;
    @JsonProperty("Guid")
    private String guid;
    @JsonProperty("Code")
    private String code;
    @JsonProperty("OrganizationCategory")
    private String organizationCategory;
    @JsonProperty("Address1")
    private String address1;
    @JsonProperty("Address2")
    private String address2;
    @JsonProperty("Country")
    private String country;
    @JsonProperty("TimeZoneId")
    private String timeZoneId;
    @JsonProperty("City")
    private String city;
    @JsonProperty("State")
    private String state;
    @JsonProperty("ZipPostCode")
    private String zipPostCode;
}
