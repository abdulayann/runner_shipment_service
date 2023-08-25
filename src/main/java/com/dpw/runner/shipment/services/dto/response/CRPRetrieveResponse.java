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


//        "OrganizationCode": "767676767", //  Mandatory if CRP selcted Customer reference code
//        "FullName": "Hudson Bay", // companu name //  Mandatory if CRP selcted
//        "Address1": "X-123 Square Central", //  Mandatory if CRP selcted
//        "Address2": "British Columbia",
//        "Country": "Canada", // 3 digit code //  Mandatory if CRP selcted
//        "CityCode": "CAVAC", // optional
//        "State": "British Columbia", //  Mandatory if CRP selcted
//        "ZipPostCode": "400001", //  Mandatory if CRP selcted
//        "UnlocoCode": "", // optional : if CRP is having this | Will be location guid
//        "CurrencyCode": "CAD", // optional : if CRP is having this
//        "Phone": "9338888888", // optional : if CRP is having this
//        "Mobile": "9338888888", // optional : if CRP is having this
//        "Email": "contact@godship.com", // optional : if CRP is having this
//        "ActiveClient": true, // always true
//        "DefaultAddressSiteIdentifier": "site", // selected address Fusion identifier
//        "Receivables": true, // if billable customer, then this will be true



//                    /** This meta to be require if customer is selected from CRP & isCustomerFreeText = false */
//                    "AddressShortCode": "NEW", // slected office address code
//                    "CompanyName": "COMPANY", // Mandatory if CRP customer selected
//                    "SiteIdentifier": "474643", // selected address Fusion identifier
//                    "Address1": "Adress 1", // Mandatory if CRP customer selected
//                    "Country": "CAN", // Mandatory if CRP customer selected
//                    "City": "British Columbia", // Mandatory if CRP customer selected
//                    "State": "British Columbia", // Mandatory if CRP customer selected
//                    "ZipPostCode": "400001", // optional : if CRP is having this
//                    "Mobile": "9873987987", // optional : if CRP is having this
//                    "Email": "contact@godship.com" // optional : if CRP is having this

//        "identifierCodeType": "FUSION_ACC_ID",
//        "identifierCode": "123456",
//        "identifierValu