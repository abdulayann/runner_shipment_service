package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CloneFieldResponse implements IRunnerResponse {
    private CloneSection header;
    private CloneSection party;
    private CloneSection general;
    private CloneSection containers;
    private CloneSection packages;
    private CloneSection cargoSummary;
}