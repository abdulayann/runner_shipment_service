package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.entity.Parties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

@Data
@ApiModel("Pickup Delivery Details Instruction Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PickDeliveryDetailsInstructionRequest {
    String instructionType;
    String dropMode;
    List<Parties> partiesList;
    String transportCompany;
    String pickupFrom;
    String deliverTo;
    String transportCompanyAddress;
    String pickupFromAddress;
    String deliveryToAddress;
    String remarks;

    LocalDateTime portTransportAdvised;
    LocalDateTime requiredBy;
    LocalDateTime estimatedPickUpDate;
    LocalDateTime estimatedDeliveryDate;
    LocalDateTime actualPickUpDate;
    LocalDateTime actualDeliveryDate;

    LocalDateTime pickupGateIn;
    LocalDateTime pickupGateOut;
    LocalDateTime deliveryGateIn;
    LocalDateTime deliveryGateOut;
}
