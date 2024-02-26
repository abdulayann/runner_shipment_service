package com.dpw.runner.shipment.services.ReportingService.Models.Commons;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;

import java.math.BigDecimal;
import java.util.List;

public class ShipmentResponse implements IRunnerResponse
{
    public String masterBill;
    public String houseBill;
    public Long totalPacks;
    public BigDecimal weight;
    public String weightUnit;
    public List<String> consigner;
    public List<String> consignee;
    public List<String> consignerAddressFreeText;
    public List<String> consigneeAddressFreeText;
    public List<String> notifyPartyAddressFreeText;
    public String consignerCompanyName;
    public String consigneeCompanyName;
    public String consignerLocalName;
    public String consigneeLocalName;
    public String description;
    public String hsnNumber;
    public String freightOverseasCurrency;
}
