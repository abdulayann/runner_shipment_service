package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.Parties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
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
}
