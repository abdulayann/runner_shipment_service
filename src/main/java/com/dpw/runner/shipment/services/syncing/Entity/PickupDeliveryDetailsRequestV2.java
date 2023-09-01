package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

@Data
public class PickupDeliveryDetailsRequestV2 {
    private DateTime? ActualPickupOrDelivery;
    //DECOUPLE + SHIPMENTS REPO CHANGES
    private PartyRequestV2 AgentDetail;
    private PartyRequestV2 BrokerDetail;
    private PartyRequestV2 DestinationDetail;

    private String DropMode;
    private DateTime? EmptyTruckInDate;

    private DateTime? EstimatedPickupOrDelivery;
    private DateTime? FclAvailableDate;
    private String InterimReceipt;
    private DateTime? PickupOrDelivery;
    private Int64 LabourCharge;
    private String LabourChargeUnit;
    private Duration LabourDuration;
    private DateTime? LoadedTruckGateOutDate;
    private DateTime? PortTransportAdvised;
    private DateTime? RequiredBy;
    private Int64 ShipmentId;
    private String ShipperRef;
    private PartyRequestV2 SourceDetail;
    private Int64 StorageCharge;
    private String StorageChargeUnit;
    private Duration StorageChargeDuration;
    private PartyRequestV2 TransporterDetail;
    private Duration TruckWeightDuration;
    private String TruckWaitTimeChargeUnit;
    private Int64 TruckWaitTimeCharge;

    private String Type;
    private String UcrReference;
}
