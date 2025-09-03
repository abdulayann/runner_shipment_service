package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
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
    @NotBlank(message = "Leg type is required")
    private String legType;
    private PartiesRequest origin;
    private PartiesRequest destination;
    private List<TiReferencesRequest> tiReferences;
    @Valid
    private List<TiTruckDriverDetailsRequest> tiTruckDriverDetails;
    @Valid
    private List<TiContainersRequest> tiContainers;
    @Valid
    private List<TiPackagesRequest> tiPackages;
    private LocalDateTime estimatedPickup;
    private LocalDateTime estimatedDelivery;
    private LocalDateTime actualPickup;
    private LocalDateTime actualDelivery;
    private LocalDateTime requiredBy;
    private String dropMode;
    private String remarks;
}
