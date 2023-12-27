package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.LocalTime;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class PickupDeliveryDetailsModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("EstimatedPickupOrDelivery")
    private LocalDateTime estimatedPickupOrDelivery;
    @JsonProperty("RequiredBy")
    private LocalDateTime requiredBy;
    @JsonProperty("PortTransportAdvised")
    private LocalDateTime portTransportAdvised;
    @JsonProperty("ActualPickupOrDelivery")
    private LocalDateTime actualPickupOrDelivery;
    @JsonProperty("PickupOrDelivery")
    private LocalDateTime pickupOrDelivery;
    @JsonProperty("TransporterDetail")
    private PartiesModel transporterDetail;
    @JsonProperty("BrokerDetail")
    private PartiesModel brokerDetail;
    @JsonProperty("DestinationDetail")
    private PartiesModel destinationDetail;
    @JsonProperty("SourceDetail")
    private PartiesModel sourceDetail;
    @JsonProperty("AgentDetail")
    private PartiesModel agentDetail;
    @JsonProperty("Type")
    private String type;
    @JsonProperty("ShipmentId")
    private Long shipmentId;
    @JsonProperty("DropMode")
    private String dropMode;
    @JsonProperty("LabourCharge")
    private BigDecimal labourCharge;
    @JsonProperty("LabourChargeUnit")
    private String labourChargeUnit;
    @JsonProperty("LabourDuration")
    private LocalTime labourDuration;
    @JsonProperty("ShipperRef")
    private String shipperRef;
    @JsonProperty("InterimReceipt")
    private String interimReceipt;
    @JsonProperty("FclAvailableDate")
    private LocalDateTime fclAvailableDate;
    @JsonProperty("StorageDate")
    private LocalDateTime storageDate;
    @JsonProperty("TruckWaitTimeCharge")
    private BigDecimal truckWaitTimeCharge;
    @JsonProperty("TruckWaitTimeChargeUnit")
    private String truckWaitTimeChargeUnit;
    @JsonProperty("TruckWaitDuration")
    private LocalTime truckWaitDuration;
    @JsonProperty("StorageCharge")
    private BigDecimal storageCharge;
    @JsonProperty("StorageChargeUnit")
    private String storageChargeUnit;
    @JsonProperty("StorageChargeDuration")
    private LocalTime storageChargeDuration;
    @JsonProperty("UcrReference")
    private String ucrReference;
    @JsonProperty("EmptyTruckInDate")
    private LocalDateTime emptyTruckInDate;
    @JsonProperty("LoadedTruckGateOutDate")
    private LocalDateTime loadedTruckGateOutDate;
    @JsonProperty("PickupDeliveryInstruction")
    private String pickupDeliveryInstruction;
}
