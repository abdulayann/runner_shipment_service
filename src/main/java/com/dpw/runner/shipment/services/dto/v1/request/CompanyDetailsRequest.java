package com.dpw.runner.shipment.services.dto.v1.request;
import lombok.Data;
import java.util.List;

@Data
public class CompanyDetailsRequest {
  private StatusCodeDTO statusCode;
  private TenantDTO tenant;
  private String companyName;
  private String countryCode;
  private String stateName;
  private String taxApplicable = "N";
  private String addressLine1;
  private String cityName;
  private String postalCode;
  private String companyEmail;
  private String contactNumberCountryCode;
  private String contactNumber;
  private List<String> companyRegisteredUnder;
  private String tradersCode;
  private String companyType = "ORG";
  private List<CompanyClientTypeDTO> companyClientTypes;
  private List<CompanyCodeIssuerDetailDTO> companyCodeIssuerDetails;
}

@Data
class StatusCodeDTO {
  private String name = "Submitted";
  private String description = "Submitted";
}

@Data
class TenantDTO {
  private String name;
  private String description;
}

@Data
class CompanyClientTypeDTO {
  private StatusCodeDTO statusCode = new StatusCodeDTO();
  private String clientTypeCode = "BCO";
  private String clientTypeName = "BCO (Importer / Exporter)";
  private List<CompanyServiceProviderDetailDTO> companyServiceProviderDetails;
}

@Data
class CompanyServiceProviderDetailDTO {
  private StatusCodeDTO statusCode = new StatusCodeDTO();
  private String locationCode = "GLOBAL";
  private String serviceProviderCode = "GLOBALFF";
}

@Data
class CompanyCodeIssuerDetailDTO {
  private StatusCodeDTO statusCode = new StatusCodeDTO();
  private String clientTypeCode = "BCO";
  private String locationCode = "GLOBAL";
  private String serviceProviderCode = "GLOBALFF";
  private String issuerCategory = "COMPANY_ROLE";
  private String issuerCategoryValue = "BCO";
  private String correspondingCategory = "Service Provider";
  private String correspondingCategoryValue = "GLOBALFF";
  private String identifierIssuedBy = "FUSION";
  private String identifierCodeType = "BILLABLE_FLAG";
  private String identifierCode = "NO";
  private String identifierValue = "";
  private String isDefault = "No";
}
