package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.time.Duration;
import java.util.Date;
import java.util.UUID;

@Data
public class ShipmentServiceResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long shipmentId;
    private Long consolidationId;
    private String serviceType;
    private Long contractorId;
    private Long contractorAddressId;
    private int srvLocation;
    private Date bookingDate;
    private String serviceCount;
    private Duration serviceDuration;
    private Date completionDate;
    private String refNumber;
    private String serviceNotes;
}
