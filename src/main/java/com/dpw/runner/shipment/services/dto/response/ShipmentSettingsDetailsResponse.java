package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class ShipmentSettingsDetailsResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Boolean HouseBillNumberLock;
    private Boolean RestrictHblGen;
    private Boolean PrintPhoneNumber;
    private String HousebillPrefix;
    private String HousebillNumberGeneration;
    private Integer FooterColumns;
    private Boolean IsAutoPopulateShipType;
    private Boolean PartialCloningEnabled;
    private Boolean ShipConsolidationContainerEnabled;
    private Boolean MultipleShipmentEnabled;
    private Boolean EnableRouteMaster;
    private Boolean ArApFlag;
    private Boolean ShipmentTiCargesLinkage;
    private Integer CooldownTime;
    private Integer AdvancePeriod;
    private Boolean AutoEventCreate;
    private Boolean ShipmentLite;
    private Boolean BillingLite;
    private Boolean Restricted_Locations_Enabled;
    private Boolean IsAtdAtaAutoPopulateEnabled;
    private List<Integer> Restricted_Locations;
    private String ShipmentImportApproverRole;
}
