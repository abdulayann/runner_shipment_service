package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.UUID;

@Data
public class PickupDeliveryDetailsRequestV2 implements IRunnerRequest {
    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("ShipmentGuid")
    private UUID ShipmentGuid;
    @JsonProperty("ActualPickupOrDelivery")
    private LocalDateTime ActualPickupOrDelivery;
    //DECOUPLE + SHIPMENTS REPO CHANGES
    @JsonProperty("AgentDetail")
    private PartyRequestV2 AgentDetail;
    @JsonProperty("BrokerDetail")
    private PartyRequestV2 BrokerDetail;
    @JsonProperty("DestinationDetail")
    private PartyRequestV2 DestinationDetail;

    @JsonProperty("DropMode")
    private String DropMode;
    @JsonProperty("EmptyTruckInDate")
    private LocalDateTime EmptyTruckInDate;
    @JsonProperty("EstimatedPickupOrDelivery")
    private LocalDateTime EstimatedPickupOrDelivery;
    @JsonProperty("FclAvailableDate")
    private LocalDateTime FclAvailableDate;
    @JsonProperty("InterimReceipt")
    private String InterimReceipt;
    @JsonProperty("PickupOrDelivery")
    private LocalDateTime PickupOrDelivery;
    @JsonProperty("LabourCharge")
    private BigDecimal LabourCharge;
    @JsonProperty("LabourChargeUnit")
    private String LabourChargeUnit;
    @JsonProperty("LabourDuration")
    private LocalTime LabourDuration;
    @JsonProperty("LoadedTruckGateOutDate")
    private LocalDateTime LoadedTruckGateOutDate;
    @JsonProperty("PortTransportAdvised")
    private LocalDateTime PortTransportAdvised;
    @JsonProperty("RequiredBy")
    private LocalDateTime RequiredBy;
    @JsonProperty("ShipperRef")
    private String ShipperRef;
    @JsonProperty("SourceDetail")
    private PartyRequestV2 SourceDetail;
    @JsonProperty("StorageCharge")
    private BigDecimal StorageCharge;
    @JsonProperty("StorageChargeUnit")
    private String StorageChargeUnit;
    @JsonProperty("StorageChargeDuration")
    private LocalTime StorageChargeDuration;
    @JsonProperty("StorageDate")
    private LocalDateTime StorageDate;
    @JsonProperty("TransporterDetail")
    private PartyRequestV2 TransporterDetail;
    @JsonProperty("TruckWaitDuration")
    private LocalTime TruckWaitDuration;
    @JsonProperty("TruckWaitTimeChargeUnit")
    private String TruckWaitTimeChargeUnit;
    @JsonProperty("TruckWaitTimeCharge")
    private BigDecimal TruckWaitTimeCharge;
    @JsonProperty("Type")
    private String Type;
    @JsonProperty("UcrReference")
    private String UcrReference;
    @JsonProperty("PickupDeliveryInstruction")
    private String PickupDeliveryInstruction;
}
