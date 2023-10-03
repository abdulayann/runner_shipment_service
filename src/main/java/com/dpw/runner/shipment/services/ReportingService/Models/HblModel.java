package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;

import java.util.List;
import java.util.Map;

public class HblModel implements IDocumentModel{
    //public List<BillChargesRow> billCharges;
    //public BigDecimal prepaidTotalAmount;
    //public BigDecimal collectTotalAmount;
    public ShipmentModel shipment;
    public ConsolidationModel consolidation;
    public Hbl blObject;
    public List<ShipmentContainers> commonContainers;
    public String paymentTerms;
    public String serviceMode;
    public VesselsResponse preCarriageVessel;
    public String paidPlaceCountry;
    public long noofPackages = 0;
    public Map<String, Long> containerCountGrouped;
    public Map<String, Long> containerPacksGrouped;
}
