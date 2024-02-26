package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;

import java.util.List;
import java.util.Map;

public class HblModel implements IDocumentModel{
    //public List<BillChargesRow> billCharges;
    //public BigDecimal prepaidTotalAmount;
    //public BigDecimal collectTotalAmount;
    public transient ShipmentModel shipment;
    public transient ConsolidationModel consolidation;
    public TenantModel tenant;
    public Hbl blObject;
    public Boolean isHbl;
    public UsersDto user;
    public String podCountry;
    public List<ShipmentContainers> commonContainers;
    public String paymentTerms;
    public String serviceMode;
    public String releaseType;
    public UnlocationsResponse polPort;
    public UnlocationsResponse podPort;
    public UnlocationsResponse placeOfIssue;
    public String issuePlaceCountry;
    public transient VesselsResponse preCarriageVessel;
    public String paidPlaceCountry;
    public long noofPackages = 0;
    public Map<String, Long> containerCountGrouped;
    public Map<String, Long> containerPacksGrouped;
    public Map<String, Double> containerWeightGrouped;
    public Map<String, Double> containerVolumeGrouped;
    public ShipmentSettingsDetails shipmentSettingsDetails;
    public V1TenantSettingsResponse tenantSettingsResponse;
}
