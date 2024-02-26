package com.dpw.runner.shipment.services.ReportingService.Models.Commons;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;

import java.math.BigDecimal;
import java.util.List;

public class ShipmentResponse implements IRunnerResponse
{
    public String MasterBill;
    public String HouseBill;
    public Long TotalPacks;
    public BigDecimal Weight;
    public String WeightUnit;
    public transient List<String> Consigner;
    public transient List<String> Consignee;
    public transient List<String> ConsignerAddressFreeText ;
    public transient List<String> ConsigneeAddressFreeText ;
    public transient List<String> NotifyPartyAddressFreeText ;
    public String ConsignerCompanyName;
    public String ConsigneeCompanyName;
    public String ConsignerLocalName;
    public String ConsigneeLocalName;
    public String Description;
    public String HsnNumber;
    public String FreightOverseasCurrency;
}
