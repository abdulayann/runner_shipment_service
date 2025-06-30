package com.dpw.runner.shipment.services.dto.v1.request;
import static com.dpw.runner.shipment.services.commons.constants.Constants.GLOBAL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.GLOBALFF;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SUBMITTED;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;
import java.util.List;

@Data
@JsonInclude(JsonInclude.Include.ALWAYS)
public class CompanyDetailsRequest {
  private StatusCodeDTO statusCode = new StatusCodeDTO();
  private TenantDTO tenant = new TenantDTO();
  private String companyName;
  private String countryCode;
  private String stateName;
  private String taxApplicable = "N";
  private String addressLine1;
  private String addressLine2;
  private String cityName;
  private String postalCode;
  private String companyEmail;
  private String contactNumberCountryCode;
  private String contactNumber;
  private List<String> companyRegisteredUnder;
  private String tradersCode;
  private String companyType = "ORG";
  private List<CompanyClientTypeDTO> companyClientTypes = List.of(new CompanyClientTypeDTO());
  private List<CompanyCodeIssuerDetailDTO> companyCodeIssuerDetails = List.of(new CompanyCodeIssuerDetailDTO());
}

@Data
class StatusCodeDTO {
  private String name = SUBMITTED;
  private String description = SUBMITTED;
}

@Data
class TenantDTO {
  private String name = "FR";
  @JsonInclude(JsonInclude.Include.ALWAYS)
  private String description = null;
}

@Data
class CompanyClientTypeDTO {
  private StatusCodeDTO statusCode = new StatusCodeDTO();
  private String clientTypeCode = "BCO";
  private String clientTypeName = "BCO (Importer / Exporter)";
  private List<CompanyServiceProviderDetailDTO> companyServiceProviderDetails = List.of(new CompanyServiceProviderDetailDTO());
}

@Data
class CompanyServiceProviderDetailDTO {
  private StatusCodeDTO statusCode = new StatusCodeDTO();
  private String locationCode = GLOBAL;
  private String serviceProviderCode = GLOBALFF;
}

@Data
class CompanyCodeIssuerDetailDTO {
  private StatusCodeDTO statusCode = new StatusCodeDTO();
  private String clientTypeCode = "BCO";
  private String locationCode = GLOBAL;
  private String serviceProviderCode = GLOBALFF;
  private String issuerCategory = "COMPANY_ROLE";
  private String issuerCategoryValue = "BCO";
  private String correspondingCategory = "Service Provider";
  private String correspondingCategoryValue = GLOBALFF;
  private String identifierIssuedBy = "FUSION";
  private String identifierCodeType = "BILLABLE_FLAG";
  private String identifierCode = "NO";
  private String identifierValue = "";
  private String isDefault = "No";
}
