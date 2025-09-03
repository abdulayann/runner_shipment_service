package com.dpw.runner.shipment.services.dto.v3.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import javax.validation.Valid;
import javax.validation.constraints.Size;
import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TransportInstructionLegsPackagesListRequest implements IRunnerRequest {
    @Valid
    @Size(min = 1, message = "Please provide packages request data")
    private List<TransportInstructionLegsPackagesRequest> packagesRequests = new ArrayList<>();

}
