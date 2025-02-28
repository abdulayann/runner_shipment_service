package com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel;

import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.config.LocalDateTimeWithTimeZoneSerializer;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class PickupDeliveryDetailsModel implements IDocumentModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("EstimatedPickupOrDelivery")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime estimatedPickupOrDelivery;

    @JsonProperty("EstimatedDelivery")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime estimatedDelivery;

    @JsonProperty("EstimatedPickup")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime estimatedPickup;

    @JsonProperty("RequiredBy")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime requiredBy;

    @JsonProperty("PortTransportAdvised")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime portTransportAdvised;

    @JsonProperty("ActualPickupOrDelivery")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime actualPickupOrDelivery;

    @JsonProperty("ActualPickup")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime actualPickup;

    @JsonProperty("ActualDelivery")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime actualDelivery;


    @JsonProperty("PickupOrDelivery")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
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
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime fclAvailableDate;
    @JsonProperty("StorageDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
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
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime emptyTruckInDate;
    @JsonProperty("LoadedTruckGateOutDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime loadedTruckGateOutDate;
    @JsonProperty("PickupDeliveryInstruction")
    private String pickupDeliveryInstruction;

    @JsonProperty("DeliveryGateIn")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    public LocalDateTime deliveryGateIn;

    @JsonProperty("PickupGateIn")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    public LocalDateTime pickupGateIn;


    @JsonProperty("DeliveryGateOut")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    public LocalDateTime deliveryGateOut;

    @JsonProperty("PickupGateOut")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    public LocalDateTime pickupGateOut;

    @JsonProperty("Remarks")
    public String remarks;

    @JsonProperty("Parties")
    private List<PartiesModel> partiesList;
}
