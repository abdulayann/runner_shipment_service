package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import lombok.Data;

import java.time.Duration;
import java.util.Date;
import java.util.UUID;

@Data
public class ServiceDetailsResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long shipmentId;
    private Long consolidationId;
    private String serviceType;
    private PartiesResponse contractor;
    private int srvLocation;
    private Date bookingDate;
    private String serviceCount;
    private Duration serviceDuration;
    private Date completionDate;
    private String refNumber;
    private String serviceNotes;
}
