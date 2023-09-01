package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.UUID;

@Data
public class PickupDeliveryDetailsResponse implements IRunnerResponse {

    private Long id;
    private UUID guid;
    private LocalDateTime estimatedPickupOrDelivery;
    private LocalDateTime requiredBy;
    private LocalDateTime portTransportAdvised;
    private LocalDateTime actualPickupOrDelivery;
    private LocalDateTime pickupOrDelivery;
    private PartiesResponse transporterDetail;
    private PartiesResponse brokerDetail;
    private PartiesResponse destinationDetail;
    private PartiesResponse sourceDetail;
    private PartiesResponse agentDetail;
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
}
