package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;

import java.math.BigDecimal;
import java.util.List;

public class HblModel implements IDocumentModel{
    //public List<BillChargesRow> billCharges;
    //public BigDecimal prepaidTotalAmount;
    //public BigDecimal collectTotalAmount;
    public ShipmentDetails shipment;
    public ConsolidationDetails consolidation;
    public Hbl blObject;
    public List<ShipmentContainers> commonContainers;
    public String paymentTerms;
    public String serviceMode;
    public VesselsResponse preCarriageVessel;
    public String paidPlaceCountry;
}
