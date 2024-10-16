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
    private Integer Id;
    @JsonProperty("CompanyName")
    private String CompanyName;
    @JsonProperty("Guid")
    private String Guid;
    @JsonProperty("Code")
    private String Code;
    @JsonProperty("OrganizationCategory")
    private String OrganizationCategory;
    @JsonProperty("Address1")
    private String Address1;
    @JsonProperty("Address2")
    private String Address2;
    @JsonProperty("Country")
    private String Country;
    @JsonProperty("TimeZoneId")
    private String TimeZoneId;
    @JsonProperty("City")
    private String City;
    @JsonProperty("State")
    private String State;
    @JsonProperty("ZipPostCode")
    private String ZipPostCode;
}
