package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.config.CustomLocalTimeSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.UUID;

@Data
public class PickupDeliveryDetailsResponse implements IRunnerResponse {

    private Long id;
    private UUID guid;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime estimatedPickupOrDelivery;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime requiredBy;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime portTransportAdvised;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime actualPickupOrDelivery;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
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
    @JsonSerialize(using = CustomLocalTimeSerializer.class)
    private LocalTime labourDuration;
    private String shipperRef;
    private String interimReceipt;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime fclAvailableDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime storageDate;
    private BigDecimal truckWaitTimeCharge;
    private String truckWaitTimeChargeUnit;
    @JsonSerialize(using = CustomLocalTimeSerializer.class)
    private LocalTime truckWaitDuration;
    private BigDecimal storageCharge;
    private String storageChargeUnit;
    @JsonSerialize(using = CustomLocalTimeSerializer.class)
    private LocalTime storageChargeDuration;
    private String ucrReference;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime emptyTruckInDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime loadedTruckGateOutDate;
    private String pickupDeliveryInstruction;
    private LocalDateTime pickupDeliveryGateIn;
    private LocalDateTime pickupDeliveryGateOut;
}
