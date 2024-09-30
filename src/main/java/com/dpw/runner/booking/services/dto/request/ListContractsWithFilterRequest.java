package com.dpw.runner.booking.services.dto.request;

import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import lombok.*;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ListContractsWithFilterRequest implements IRunnerRequest {
    private String cargoType;
    private String origin;
    private String destination;
    private ListContractRequest listContractRequest;
}
