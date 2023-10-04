package com.dpw.runner.shipment.services.ReportingService.Models.Commons;

import java.math.BigDecimal;
import java.util.List;

public class ShipmentResponse
{
    public String MasterBill;
    public String HouseBill;
    public Long TotalPacks;
    public BigDecimal Weight;
    public String WeightUnit;
    public List<String> Consigner;
    public List<String> Consignee;
    public List<String> ConsignerAddressFreeText ;
    public List<String> ConsigneeAddressFreeText ;
    public List<String> NotifyPartyAddressFreeText ;
    public String ConsignerCompanyName;
    public String ConsigneeCompanyName;
    public String ConsignerLocalName;
    public String ConsigneeLocalName;
    public String Description;
    public String HsnNumber;
}
