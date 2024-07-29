package com.dpw.runner.shipment.services.commons.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
public class BookingCarriageResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long shipmentId;
    private String portOfLoading;
    private String portOfDischarge;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime eta;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime etd;
    private String vessel;
    private String voyage;
    private String carriageType;
    private String carriageMode;
}
