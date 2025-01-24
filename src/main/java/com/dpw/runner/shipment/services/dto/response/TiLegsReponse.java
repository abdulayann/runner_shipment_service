package com.dpw.runner.shipment.services.dto.response;

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
public class TiLegsReponse implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private Long sequence;
    private String legType;
    private PartiesResponse origin;
    private PartiesResponse destination;
    private List<TiReferencesResponse> tiReferencesRequests;
    private List<TiTruckDriverDetailsResponse> tiTruckDriverDetailsRequests;
    private List<TiContainersResponse> tiContainersRequests;
    private List<TiPackagesResponse> tiPackagesRequests;
    private LocalDateTime estimatedPickup;
    private LocalDateTime estimatedDelivery;
    private LocalDateTime actualPickup;
    private LocalDateTime actualDelivery;
    private LocalDateTime requiredBy;
    private String dropMode;
    private String remarks;
}
