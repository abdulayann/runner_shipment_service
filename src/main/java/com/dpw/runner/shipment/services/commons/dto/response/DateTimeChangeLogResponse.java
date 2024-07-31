package com.dpw.runner.shipment.services.commons.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.entity.enums.DateType;
import lombok.Data;

import java.time.LocalDateTime;

@Data
public class DateTimeChangeLogResponse implements IRunnerResponse {
    private LocalDateTime currentValue;
    private DateType dateType;
    private Long shipmentId;
    private String sourceOfUpdate;
    private LocalDateTime updatedAt;
}
