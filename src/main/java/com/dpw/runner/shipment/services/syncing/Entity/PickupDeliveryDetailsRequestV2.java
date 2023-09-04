package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.math.BigDecimal;
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
    private BigDecimal LabourCharge;
    private String LabourChargeUnit;
    private LocalTime LabourDuration;
    private LocalDateTime LoadedTruckGateOutDate;
    private LocalDateTime PortTransportAdvised;
    private LocalDateTime RequiredBy;
    private Long ShipmentId;
    private String ShipperRef;
    private PartyRequestV2 SourceDetail;
    private BigDecimal StorageCharge;
    private String StorageChargeUnit;
    private LocalTime StorageChargeDuration;
    private PartyRequestV2 TransporterDetail;
    private LocalTime TruckWeightDuration;
    private String TruckWaitTimeChargeUnit;
    private BigDecimal TruckWaitTimeCharge;

    private String Type;
    private String UcrReference;
}
