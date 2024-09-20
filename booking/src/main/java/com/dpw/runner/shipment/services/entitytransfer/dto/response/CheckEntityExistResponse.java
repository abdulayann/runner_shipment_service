package com.dpw.runner.shipment.services.entitytransfer.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;


@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
public class CheckEntityExistResponse implements IRunnerResponse {
    private Boolean isEntityExists;
    private String message;
}
