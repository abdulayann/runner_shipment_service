package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.time.LocalDate;
import java.util.List;

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
