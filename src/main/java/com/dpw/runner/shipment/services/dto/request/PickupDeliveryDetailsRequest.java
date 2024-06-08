package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.config.CustomLocalTimeDeserializer;
import com.dpw.runner.shipment.services.entity.Parties;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;

@Getter
@Setter
@ApiModel("Pickup Delivery Details Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PickupDeliveryDetailsRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private LocalDateTime estimatedPickupOrDelivery;
    private LocalDateTime requiredBy;
    private LocalDateTime portTransportAdvised;
    private LocalDateTime actualPickupOrDelivery;
    private LocalDateTime pickupOrDelivery;
    private PartiesRequest transporterDetail;
    private PartiesRequest brokerDetail;
    private PartiesRequest destinationDetail;
    private PartiesRequest sourceDetail;
    private PartiesRequest agentDetail;
    private String type;
    private Long shipmentId;
    private String dropMode;
    private BigDecimal labourCharge;
    private String labourChargeUnit;
    @JsonDeserialize(using = CustomLocalTimeDeserializer.class)
    private LocalTime labourDuration;
    private String shipperRef;
    private String interimReceipt;
    private LocalDateTime fclAvailableDate;
    private LocalDateTime storageDate;
    private BigDecimal truckWaitTimeCharge;
    private String truckWaitTimeChargeUnit;
    @JsonDeserialize(using = CustomLocalTimeDeserializer.class)
    private LocalTime truckWaitDuration;
    private BigDecimal storageCharge;
    private String storageChargeUnit;
    @JsonDeserialize(using = CustomLocalTimeDeserializer.class)
    private LocalTime storageChargeDuration;
    private String ucrReference;
    private LocalDateTime emptyTruckInDate;
    private LocalDateTime loadedTruckGateOutDate;
    private String pickupDeliveryInstruction;
    private LocalDateTime pickupDeliveryGateIn;
    private LocalDateTime pickupDeliveryGateOut;
    private List<Parties> partiesList;
}
