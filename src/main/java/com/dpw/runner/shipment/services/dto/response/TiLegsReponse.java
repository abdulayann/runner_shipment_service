package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TiLegsReponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long sequence;
    private String legType;
    private PartiesResponse origin;
    private PartiesResponse destination;
    private List<TiReferencesResponse> tiReferences;
    private List<TiTruckDriverDetailsResponse> tiTruckDriverDetails;
    private List<TiContainersResponse> tiContainers;
    private List<TiPackagesResponse> tiPackages;
    private LocalDateTime estimatedPickup;
    private LocalDateTime estimatedDelivery;
    private LocalDateTime actualPickup;
    private LocalDateTime actualDelivery;
    private LocalDateTime requiredBy;
    private String dropMode;
    private String remarks;
}
