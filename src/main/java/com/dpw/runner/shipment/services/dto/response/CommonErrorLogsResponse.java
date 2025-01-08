package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.CommonErrorType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CommonErrorLogsResponse implements IRunnerResponse {
    private Long id;
    private Long entityId;
    private String entityType;
    private CommonErrorType errorType;
    private String errorMessage;
}
