package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;
import java.util.UUID;

@Data
public class PickupDeliveryDetailsListResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private LocalDateTime estimatedPickupOrDelivery;
    private LocalDateTime estimatedPickup;
    private LocalDateTime estimatedDelivery;
    private LocalDateTime requiredBy;
    private LocalDateTime portTransportAdvised;
    private LocalDateTime actualPickupOrDelivery;
    private LocalDateTime actualPickup;
    private LocalDateTime actualDelivery;
    private LocalDateTime pickupOrDelivery;
    private PartiesResponse transporterDetail;
    private String type;
    private Long shipmentId;
    private String dropMode;
    private BigDecimal labourCharge;
    private String labourChargeUnit;
    private LocalTime labourDuration;
    private String shipperRef;
    private String interimReceipt;
    private LocalDateTime fclAvailableDate;
    private LocalDateTime storageDate;
    private BigDecimal truckWaitTimeCharge;
    private String truckWaitTimeChargeUnit;
    private LocalTime truckWaitDuration;
    private BigDecimal storageCharge;
    private String storageChargeUnit;
    private LocalTime storageChargeDuration;
    private String ucrReference;
    private LocalDateTime emptyTruckInDate;
    private LocalDateTime loadedTruckGateOutDate;
    private LocalDateTime pickupGateIn;
    private LocalDateTime deliveryGateIn;
    private LocalDateTime pickupGateOut;
    private LocalDateTime deliveryGateOut;
    private String remarks;
}
