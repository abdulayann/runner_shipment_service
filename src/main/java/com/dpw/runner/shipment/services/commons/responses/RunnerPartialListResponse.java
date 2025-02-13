package com.dpw.runner.shipment.services.commons.responses;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * /**
 * * Generic List response.
 */
@SuppressWarnings("rawtypes")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RunnerPartialListResponse implements IRunnerResponse {
    Object data;
}


