package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.time.LocalDateTime;
import java.time.LocalTime;

@Data
public class PickupDeliveryDetailsRequestV2 {
    private LocalDateTime ActualPickupOrDelivery;
    //DECOUPLE + SHIPMENTS REPO CHANGES
    private PartyRequestV2 AgentDetail;
    private PartyRequestV2 BrokerDetail;
    private PartyRequestV2 DestinationDetail;

    private String DropMode;
    private LocalDateTime EmptyTruckInDate;

    private LocalDateTime EstimatedPickupOrDelivery;
    private LocalDateTime FclAvailableDate;
    private String InterimReceipt;
    private LocalDateTime PickupOrDelivery;
    private Long LabourCharge; // int64
    private String LabourChargeUnit;
    private LocalTime LabourLocalTime;
    private LocalDateTime LoadedTruckGateOutDate;
    private LocalDateTime PortTransportAdvised;
    private LocalDateTime RequiredBy;
    private Long ShipmentId; // int64
    private String ShipperRef;
    private PartyRequestV2 SourceDetail;
    private Long StorageCharge; // int64
    private String StorageChargeUnit;
    private LocalTime StorageChargeLocalTime;
    private PartyRequestV2 TransporterDetail;
    private LocalTime TruckWeightLocalTime;
    private String TruckWaitTimeChargeUnit;
    private Long TruckWaitTimeCharge; // int64

    private String Type;
    private String UcrReference;
}
