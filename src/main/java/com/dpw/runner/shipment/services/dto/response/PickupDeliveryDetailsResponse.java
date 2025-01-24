package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.config.CustomLocalTimeSerializer;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.entity.enums.InstructionType;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
public class PickupDeliveryDetailsResponse implements IRunnerResponse {

    private Long id;
    private UUID guid;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime estimatedPickupOrDelivery;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime estimatedPickup;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime estimatedDelivery;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime actualPickup;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime actualDelivery;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime actualPickupOrDelivery;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime requiredBy;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime portTransportAdvised;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime pickupOrDelivery;
    private PartiesResponse transporterDetail;
    private PartiesResponse brokerDetail;
    private PartiesResponse destinationDetail;
    private PartiesResponse sourceDetail;
    private PartiesResponse agentDetail;
    private InstructionType type;
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
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime pickupGateIn;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime deliveryGateIn;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime pickupGateOut;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime deliveryGateOut;
    private List<Parties> partiesList;
    private String remarks;
    private List<TiLegsReponse> tiLegsList;

    private Map<String, String> masterData;
}
