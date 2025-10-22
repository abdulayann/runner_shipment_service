package com.dpw.runner.shipment.services.dto.v3.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Size;
import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TransportInstructionLegsContainersListRequest implements IRunnerRequest {

    @Valid
    @Size(min = 1, message = "Please provide ti legs container request data")
    private List<TransportInstructionLegsContainersRequest> containersRequests = new ArrayList<>();

}
