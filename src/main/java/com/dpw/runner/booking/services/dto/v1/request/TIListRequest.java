package com.dpw.runner.booking.services.dto.v1.request;

import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.util.UUID;

@Data
public class TIListRequest implements IRunnerRequest {
    private UUID shipmentGuid;
}
