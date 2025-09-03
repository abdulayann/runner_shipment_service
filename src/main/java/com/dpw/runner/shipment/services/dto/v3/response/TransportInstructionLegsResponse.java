package com.dpw.runner.shipment.services.dto.v3.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.UUID;

@Getter
@Setter
public class TransportInstructionLegsResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long sequence;
    private String legType;
    private PartiesResponse origin;
    private PartiesResponse destination;
    private LocalDateTime estimatedPickup;
    private LocalDateTime estimatedDelivery;
    private LocalDateTime actualPickup;
    private LocalDateTime actualDelivery;
    private LocalDateTime requiredBy;
    private String dropMode;
    private String remarks;
}
