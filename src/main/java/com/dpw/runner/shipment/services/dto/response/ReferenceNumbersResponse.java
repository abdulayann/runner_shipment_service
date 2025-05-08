package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.UUID;

@Data
@Builder
@ApiModel("Reference Number Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ReferenceNumbersResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long consolidationId;
    private String countryOfIssue;
    private String type;
    private String referenceNumber;
    private Long shipmentId;
    private Boolean isPortalEnable;
    private String message;
    private Long bookingId;
}
