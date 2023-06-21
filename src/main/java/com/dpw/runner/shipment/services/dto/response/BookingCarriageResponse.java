package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.Date;
import java.util.UUID;

@Data
public class BookingCarriageResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long bookingId;
    private Long shipmentId;
    private Long vesselId;
    private Long polId;
    private Long podId;
    private Date eta;
    private Date etd;
    private String vessel;
    private String voyage;
    private String carriageType;
    private String carriageMode;
}
