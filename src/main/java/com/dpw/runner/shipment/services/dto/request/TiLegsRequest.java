package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TiLegsRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private Long sequence;
    private String legType;
    private PartiesRequest origin;
    private PartiesRequest destination;
    private List<TiReferencesRequest> tiReferencesRequests;
    private List<TiTruckDriverDetailsRequest> tiTruckDriverDetailsRequests;
    private List<TiContainersRequest> tiContainersRequests;
    private List<TiPackagesRequest> tiPackagesRequests;
    private LocalDateTime estimatedPickup;
    private LocalDateTime estimatedDelivery;
    private LocalDateTime actualPickup;
    private LocalDateTime actualDelivery;
    private LocalDateTime requiredBy;
    private String dropMode;
    private String remarks;
}
