package com.dpw.runner.shipment.services.dto.response;

import lombok.*;

import java.util.List;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class CRPRetrieveResponse {
    private String companyReference;
    private String companyName;
    private String countryCode;
    private String stateName;
    private String addressLine1;
    private String addressLine2;
    private String cityName;
    private String postalCode;
    private String companyEmail;
    private String contactNumberCountryCode;
    private String contactNumber;
    private List<CompanyCodeIssuerDetails> companyCodeIssuerDetails;
    private List<CRPAddressDetails> companyOfficeDetails;

    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CRPAddressDetails {
        private String officeReference;
        private String officeName;
        private String officeType;
        private String officeEmail;
        private String stateName;
        private String cityName;
        private String address;
        private String postalCode;

    }

    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CompanyCodeIssuerDetails {
        private String identifierCodeType;
        private String identifierCode;
        private String identifierValue;
    }
}