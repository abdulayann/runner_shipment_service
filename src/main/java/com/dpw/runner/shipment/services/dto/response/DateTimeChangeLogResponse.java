package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.DateType;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import lombok.Data;

import java.time.LocalDateTime;

@Data
public class DateTimeChangeLogResponse implements IRunnerResponse {
    @ExcludeTimeZone
    private LocalDateTime currentValue;
    private DateType dateType;
    private Long shipmentId;
    private String sourceOfUpdate;
    private LocalDateTime updatedAt;
}
