package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.entity.Parties;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;
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
}
