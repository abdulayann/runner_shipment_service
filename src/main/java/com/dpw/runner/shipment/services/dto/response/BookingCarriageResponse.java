package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
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
    private LocalDateTime eta;
    private LocalDateTime etd;
    private String vessel;
    private String voyage;
    private String carriageType;
    private String carriageMode;
}
