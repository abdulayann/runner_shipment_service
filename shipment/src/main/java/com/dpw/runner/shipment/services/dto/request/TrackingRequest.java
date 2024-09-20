package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class TrackingRequest implements IRunnerRequest {
    private String referenceNumber;
    private String bookingNumber;
    private String bookingNumberSource;
    private String typeOfFilter;
}
