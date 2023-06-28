package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
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
    private List<Parties> partiesBrokerDetailsList;
    private List<Parties> partiesAgentDetailsList;
    private List<Parties> partiesTransporterDetailsList;
    private List<Parties> partiesDestinationDetailsList;
    private String type;
    private Long shipmentId;
}
