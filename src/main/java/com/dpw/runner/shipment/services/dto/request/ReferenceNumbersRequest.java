package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Getter
@Setter
@ApiModel("Reference Numbers Request Model")
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ReferenceNumbersRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private Long consolidationId;
    private String countryOfIssue;
    private String type;
    private String referenceNumber;
    private Long shipmentId;
    private Boolean isPortalEnable;
    private Long bookingId;
}
