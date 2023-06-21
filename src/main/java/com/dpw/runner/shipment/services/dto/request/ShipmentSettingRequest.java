package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.Getter;
import lombok.ToString;

import java.util.List;

@Getter
@ApiModel("Shipment Settings Request Model")
@ToString
public class ShipmentSettingRequest extends CommonRequest implements IRunnerRequest {
    public Long id;
    public Boolean HouseBillNumberLock;
    public Boolean RestrictHblGen;
    public Boolean PrintPhoneNumber;
    public String HousebillPrefix;
    public String HousebillNumberGeneration;
    public Integer FooterColumns;
    public Boolean IsAutoPopulateShipType;
    public Boolean PartialCloningEnabled;
    public Boolean ShipConsolidationContainerEnabled;
    public Boolean MultipleShipmentEnabled;
    public Boolean EnableRouteMaster;
    public Boolean ArApFlag;
    public Boolean ShipmentTiCargesLinkage;
    public Integer CooldownTime;
    public Integer AdvancePeriod;
    public Boolean AutoEventCreate;
    public Boolean ShipmentLite;
    public Boolean BillingLite;
    public Boolean Restricted_Locations_Enabled;
    public Boolean IsAtdAtaAutoPopulateEnabled;
    public List<Integer> Restricted_Locations;
    public String ShipmentImportApproverRole;
}
