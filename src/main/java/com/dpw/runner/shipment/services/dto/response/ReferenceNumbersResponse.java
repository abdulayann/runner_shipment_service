package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.util.UUID;

@Data
public class ReferenceNumbersResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long consolidationId;
    private String countryOfIssue;
    private String type;
    private String referenceNumber;
    private Long shipmentId;
    private Boolean isPortalEnable;
    private Long bookingId;
}
