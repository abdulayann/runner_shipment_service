package com.dpw.runner.shipment.services.entitytransfer.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SendConsoleValidationResponse implements IRunnerResponse {
    private Boolean isError;
    private String consoleErrorMessage;
    private String shipmentErrorMessage;
    private List<Long> shipmentIds;
}
