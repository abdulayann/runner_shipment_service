package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.entity.enums.DigitGrouping;
import com.dpw.runner.shipment.services.entity.enums.GroupingNumber;
import lombok.Getter;

import java.io.Serializable;

@Getter
public class TenantModel implements Serializable {
    public String tenantName;
    public String address1;
    public String address2;
    public String email;
    public String city;
    public String state;
    public String country;
    public String phone;
    public String mobile;
    public String zipPostCode;
    public String websiteUrl;
    public String fax;
    public String vatRegNumber;
    public String panNumber;
    public String companyRegNumber;
    public boolean IATAAgent;
    public Long tenantId;
    public String currencyCode;
    public String AgentIATACode;
    public String AgentCASSCode;
    public Long DefaultOrgId;
    public String AgentIATANumber;

    public Boolean RoundoffLocalCurrencyAmount;
    public Boolean IsGroupingOverseas;
    public DigitGrouping CurrencyDigitGrouping;
    public GroupingNumber CurrencyGroupingNumber;
}
