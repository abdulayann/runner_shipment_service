package com.dpw.runner.shipment.services.reportingservice.Models.Commons;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

@Data
public class ShipmentResponse implements IRunnerResponse
{
    public String masterBill;
    public String houseBill;
    public Long totalPacks;
    public BigDecimal weight;
    public String weightUnit;
    private List<String> consigner;
    private List<String> consignee;
    private List<String> consignerAddressFreeText;
    private List<String> consigneeAddressFreeText;
    private List<String> notifyPartyAddressFreeText;
    public String consignerCompanyName;
    public String consigneeCompanyName;
    public String consignerLocalName;
    public String consigneeLocalName;
    public String description;
    public String hsnNumber;
    public String freightOverseasCurrency;
}
