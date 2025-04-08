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
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.List;
import java.util.Map;

@Data
public class HblModel implements IDocumentModel{
    public ShipmentModel shipment;
    public ConsolidationModel consolidation;
    public TenantModel tenant;
    public Hbl blObject;
    public Boolean isHbl;
    public UsersDto user;
    public String podCountry;
    private List<ShipmentContainers> commonContainers;
    public String paymentTerms;
    public String serviceMode;
    public String releaseType;
    public UnlocationsResponse polPort;
    public UnlocationsResponse podPort;
    public UnlocationsResponse placeOfIssue;
    public String issuePlaceCountry;
    public VesselsResponse preCarriageVessel;
    public String paidPlaceCountry;
    public long noofPackages = 0;
    @JsonProperty("containerCountGrouped")
    private Map<String, Long> containerCountGrouped;
    @JsonProperty("containerPacksGrouped")
    private Map<String, Long> containerPacksGrouped;
    @JsonProperty("containerWeightGrouped")
    private Map<String, Double> containerWeightGrouped;
    @JsonProperty("containerVolumeGrouped")
    private Map<String, Double> containerVolumeGrouped;
    public ShipmentSettingsDetails shipmentSettingsDetails;
    public V1TenantSettingsResponse tenantSettingsResponse;
    public String polName;
    public String polCountry;
    public String podName;
    public Long transportInstructionId;
}
