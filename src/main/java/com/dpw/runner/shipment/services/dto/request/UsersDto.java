package com.dpw.runner.shipment.services.dto.request;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.io.Serializable;
import java.util.HashSet;
import java.util.List;
import java.util.Map;


@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
@SuppressWarnings({"java:S1948", "java:S1319"})
public class UsersDto implements Serializable {
    private static final long serialVersionUID = 1L;
    private Integer Id;
    public String Username;
    public String DisplayName;
    public String Email;
    public String Phone;
    public Boolean IsAdmin;
    public Integer TenantId;
    public String Code; //Tenant Code
    public String CompanyCode;
    public String TenantDisplayName;
    public String TenantLogo;
    public String TenantPrintLogo;
    public String TenantCountryCode;
    public String HouseBillLogo;
    public Integer CompanyId;
    public String CompanyCurrency;
    public Boolean AddhocCharges;
    public Boolean CurrencyOverride;
    public Boolean FusionIntegration;
    public String InvoicePrefix;
    public Boolean InvoiceNumberLock;
    public String LocalRefPrefix;
    public Boolean LocalRefLock;
    public Boolean TaxType1;
    public Boolean TaxType2;
    public Boolean TaxType3;
    public Boolean TaxType4;
    public Boolean Details;
    public Boolean ChargeCodeAlt;
    public String Header;
    public String Footer;
    public Boolean LocalCurrencyLock;
    public Boolean HousebillNumberLock;
    public String HousebillPrefix;
    public String QuoteFooter;
    public Boolean QuoteNumberLock;
    public String QuotePrefix;
    public Boolean OrgCodeLock ;
    @JsonProperty("Permissions")
    private Map<String, Boolean> Permissions;
    public String EmployeeToken;
    public List<String> userAllowedPermissions;
    public HashSet<String> customeTenantCountry;
    public Boolean allowedTenantCustom;
    public Boolean allowTenantForAEB;
    public HashSet<String> operationReportTenants;
    public HashSet<String> billChargeSaveAndAddEnabledTenants;
    public HashSet<String> elSettingCountry;
    public Long UserId;
    public String TimeZoneId;
    public Boolean EnableTimeZone;
    public HashSet<String> vildenEnabledCountrySet;
    public HashSet<String> dischargePortCountrySet;
    public List<String> DisableSavedFiltersTenantCodes;
    public List<String> EnableSavedFiltersTenantCodes;
    public Boolean EnableSavedFilters;
    public Integer DepartmentId;
    public String DepartmentName;

    public String GwEmpCode;
    public String GwDeptCode;
    public String Erp10EmpCode;
    public String Erp10DeptCode;
    public Boolean UnicoRelated;
    public String AgentIATACode;
    public String AgentCASSCode;
    public Integer SyncTenantId;
    public String LanguageCode;

}
