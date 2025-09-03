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
public class TransportInstructionLegsReferenceListRequest implements IRunnerRequest {
    @Valid
    @Size(min = 1, message = "Please provide the reference list data")
    private List<TransportInstructionLegsReferenceRequest> references = new ArrayList<>();
}
