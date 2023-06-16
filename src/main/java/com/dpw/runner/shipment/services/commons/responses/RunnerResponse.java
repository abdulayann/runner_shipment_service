package com.dpw.runner.shipment.services.commons.responses;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class RunnerResponse {
    Object data;
    long pageNo;
    long count;
}
