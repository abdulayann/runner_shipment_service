package com.dpw.runner.shipment.services.dto.v3.request;

import com.dpw.runner.shipment.services.commons.enums.TILegType;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.TiContainersRequest;
import com.dpw.runner.shipment.services.dto.request.TiPackagesRequest;
import com.dpw.runner.shipment.services.dto.request.TiReferencesRequest;
import com.dpw.runner.shipment.services.dto.request.TiTruckDriverDetailsRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TransportInstructionLegsRequest implements IRunnerRequest {
    private Long id;
    @NotNull(message = "Transport Instruction Id is required")
    private Long tiId;
    private UUID guid;
    private Long sequence;
    @NotNull(message = "Leg type is required")
    private TILegType legType;
    private PartiesRequest origin;
    private PartiesRequest destination;
    private List<TransportInstructionLegsReferenceRequest> tiReferences;
    @Valid
    private List<TransportInstructionLegsTruckDriverRequest> tiTruckDriverDetails;
    @Valid
    private List<TransportInstructionLegsContainersRequest> tiContainers;
    @Valid
    private List<TransportInstructionLegsPackagesRequest> tiPackages;
    private LocalDateTime estimatedPickup;
    private LocalDateTime estimatedDelivery;
    private LocalDateTime actualPickup;
    private LocalDateTime actualDelivery;
    private LocalDateTime requiredBy;
    private String dropMode;
    private String remarks;
}
